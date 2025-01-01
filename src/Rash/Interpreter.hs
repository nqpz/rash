module Rash.Interpreter
  ( interpret
  , emptyState
  , thawState
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception

import qualified Text.ShellEscape as TSE

import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified System.Process as Proc

import Data.Char
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.List as L

import Rash.SequenceUtilities (Sequence, sequenceLength, sequenceToList)
import qualified Rash.Representation.Internal as RI


-- SETTTINGS

maxNSteps :: Int
maxNSteps = 2000


-- UTILITIES

-- | Run a process with optional standard in, and return the exit code and
-- standard out.
runProcess :: String -> Maybe String -> IO (Int, String)
runProcess cmdAndArgs stdinM = do
  let cp = Proc.shell cmdAndArgs
      stdin = case stdinM of
        Nothing -> ""
        Just s -> s
  (exitCode, stdout, _stderr) <- Proc.readCreateProcessWithExitCode cp stdin
  let i = case exitCode of
        Exit.ExitSuccess -> 0
        Exit.ExitFailure n -> n
  pure (i, stdout)


data InterpM a = InterpM { runInterpM :: RI.Context -> RI.State -> IO (a, RI.State)
                         }

instance Monad InterpM where
  m >>= f = InterpM $ \c s -> do
    (x', s') <- runInterpM m c s
    runInterpM (f x') c s'

instance Functor InterpM where
  fmap = liftM

instance Applicative InterpM where
  pure x = InterpM $ \_ s -> pure (x, s)
  (<*>) = ap

instance MonadIO InterpM where
  liftIO m = InterpM $ \_ s -> do
    r <- m
    pure (r, s)

getContext :: InterpM RI.Context
getContext = InterpM $ \c s -> pure (c, s)

getState :: InterpM RI.State
getState = InterpM $ \_ s -> pure (s, s)

putState :: RI.State -> InterpM ()
putState s = InterpM $ \_ _ -> pure ((), s)

setPC :: Int -> InterpM ()
setPC pc = do
  s <- getState
  putState s { RI.statePC = pc }

modifyPC :: (Int -> Int) -> InterpM ()
modifyPC f = do
  pc <- RI.statePC <$> getState
  setPC $ f pc

setExitCode :: Int -> InterpM ()
setExitCode ec = do
  s <- getState
  putState s { RI.statePrevExitCode = ec }

getVar :: Int -> InterpM T.Text
getVar i = do
  vars <- RI.stateVars <$> getState
  liftIO $ MA.readArray vars i

setVar :: Int -> T.Text -> InterpM ()
setVar i t = do
  vars <- RI.stateVars <$> getState
  liftIO $ MA.writeArray vars i t

interpret :: RI.Context -> RI.State -> IO ()
interpret c s = fst <$> runInterpM (interpretM 0) c s

interpretM :: Int -> InterpM ()
interpretM nSteps
  | nSteps > maxNSteps = liftIO $ putStrLn "Too many steps; stopping."
  | otherwise = do
    pc <- RI.statePC <$> getState
    RI.Assembly insts <- RI.contextAssembly <$> getContext
    if pc >= sequenceLength insts
      then interpretInstruction RI.Exit
      else do
      let instCur = insts IA.! pc
      interpretInstruction instCur
      interpretM (nSteps + 1)

emptyState :: Int -> IO RI.State
emptyState nVars = do
  vars <- MA.newArray (0, nVars - 1) (T.pack "")
  pure RI.State { RI.statePC = 0
                , RI.stateVars = vars
                , RI.stateJustRestarted = False
                , RI.statePrevExitCode = 0
                }

freezeState :: RI.State -> IO RI.IState
freezeState st = do
  vars <- MA.freeze $ RI.stateVars st
  pure $ RI.IState { RI.iStatePC = RI.statePC st
                  , RI.iStateVars = vars
                  }

thawState :: RI.IState -> IO RI.State
thawState ist = do
  vars <- MA.thaw $ RI.iStateVars ist
  pure $ RI.State { RI.statePC = RI.iStatePC ist
                  , RI.stateVars = vars
                  , RI.stateJustRestarted = True
                  , RI.statePrevExitCode = 0
                  }

evalParts :: Sequence RI.Part -> InterpM T.Text
evalParts ps = do
  let ps1 = extractParts ps
      ps2 = sequenceToList ps1
  ps3 <- mapM id ps2
  pure $ T.concat ps3

extractParts :: Sequence RI.Part -> Sequence (InterpM T.Text)
extractParts = IA.amap extractPart

extractPart :: RI.Part -> InterpM T.Text
extractPart = \case
  RI.TextPart t -> pure t
  RI.IDPart b v -> do r <- getVar v
                      pure $ if b then shEsc r else r
          where shEsc = TE.decodeUtf8 . TSE.bytes . TSE.sh . TE.encodeUtf8

interpretCommand :: RI.Command -> InterpM (Int, String)
interpretCommand (RI.Command cmd stdinM) = do
  cmd' <- evalParts cmd
  stdinM' <- sequence (evalParts <$> stdinM)
  liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')

interpretInstruction :: RI.Instruction -> InterpM ()
interpretInstruction = \case
  RI.Read var -> do
    s <- getState
    c <- getContext

    if RI.stateJustRestarted s
      then do
      setVar var $ RI.contextReadArgs c
      putState s { RI.stateJustRestarted = False }
      modifyPC (+ 1)
      setExitCode 0

      else do
      let paths = RI.contextPaths c
          asm = RI.contextAssembly c
      iState <- liftIO $ freezeState s
      liftIO $ writeFile (RI.pathASM paths) (show asm)
      liftIO $ writeFile (RI.pathState paths) (show iState)
      liftIO Exit.exitSuccess

  RI.Run command -> do
    (ec, out) <- interpretCommand command
    liftIO $ putStr out
    modifyPC (+ 1)
    setExitCode ec

  RI.AssignRun v command -> do
    (ec, out) <- interpretCommand command
    setVar v $ T.pack $ L.dropWhileEnd isSpace out
    modifyPC (+ 1)
    setExitCode ec

  RI.Assign v parts -> do
    parts' <- evalParts parts
    setVar v parts'
    modifyPC (+ 1)
    setExitCode 0

  RI.JumpIfRetZero p -> do
    ec <- RI.statePrevExitCode <$> getState
    if (ec == 0)
      then setPC p
      else modifyPC (+ 1)
    setExitCode 0

  RI.Jump p -> do
    setPC p
    setExitCode 0

  RI.Exit -> do
    paths <- RI.contextPaths <$> getContext
    liftIO $ flip catch (\e -> (e :: IOException) `seq` pure ()) $ do
      Dir.removeFile $ RI.pathASM paths
      Dir.removeFile $ RI.pathState paths
    liftIO Exit.exitSuccess
