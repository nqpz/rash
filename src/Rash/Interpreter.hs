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
import qualified Rash.InternalRepresentation as M


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


data InterpM a = InterpM { runInterpM :: M.Context -> M.State -> IO (a, M.State)
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

getContext :: InterpM M.Context
getContext = InterpM $ \c s -> pure (c, s)

getState :: InterpM M.State
getState = InterpM $ \_ s -> pure (s, s)

putState :: M.State -> InterpM ()
putState s = InterpM $ \_ _ -> pure ((), s)

setPC :: Int -> InterpM ()
setPC pc = do
  s <- getState
  putState s { M.statePC = pc }

modifyPC :: (Int -> Int) -> InterpM ()
modifyPC f = do
  pc <- M.statePC <$> getState
  setPC $ f pc

setExitCode :: Int -> InterpM ()
setExitCode ec = do
  s <- getState
  putState s { M.statePrevExitCode = ec }

getVar :: Int -> InterpM T.Text
getVar i = do
  vars <- M.stateVars <$> getState
  liftIO $ MA.readArray vars i

setVar :: Int -> T.Text -> InterpM ()
setVar i t = do
  vars <- M.stateVars <$> getState
  liftIO $ MA.writeArray vars i t

interpret :: M.Context -> M.State -> IO ()
interpret c s = fst <$> runInterpM (interpretM 0) c s

interpretM :: Int -> InterpM ()
interpretM nSteps
  | nSteps > maxNSteps = liftIO $ putStrLn "Too many steps; stopping."
  | otherwise = do
    pc <- M.statePC <$> getState
    M.Assembly insts <- M.contextAssembly <$> getContext
    if pc >= sequenceLength insts
      then interpretInstruction M.Exit
      else do
      let instCur = insts IA.! pc
      interpretInstruction instCur
      interpretM (nSteps + 1)

emptyState :: Int -> IO M.State
emptyState nVars = do
  vars <- MA.newArray (0, nVars - 1) (T.pack "")
  pure M.State { M.statePC = 0
               , M.stateVars = vars
               , M.stateJustRestarted = False
               , M.statePrevExitCode = 0
               }

freezeState :: M.State -> IO M.IState
freezeState st = do
  vars <- MA.freeze $ M.stateVars st
  pure $ M.IState { M.iStatePC = M.statePC st
                  , M.iStateVars = vars
                  }

thawState :: M.IState -> IO M.State
thawState ist = do
  vars <- MA.thaw $ M.iStateVars ist
  pure $ M.State { M.statePC = M.iStatePC ist
                 , M.stateVars = vars
                 , M.stateJustRestarted = True
                 , M.statePrevExitCode = 0
                 }

evalParts :: Sequence M.Part -> InterpM T.Text
evalParts ps = do
  let ps1 = extractParts ps
      ps2 = sequenceToList ps1
  ps3 <- mapM id ps2
  pure $ T.concat ps3

extractParts :: Sequence M.Part -> Sequence (InterpM T.Text)
extractParts = IA.amap extractPart

extractPart :: M.Part -> InterpM T.Text
extractPart p = case p of
  M.TextPart t -> pure t
  M.IDPart b v -> do r <- getVar v
                     pure $ if b then shEsc r else r
          where shEsc = TE.decodeUtf8 . TSE.bytes . TSE.sh . TE.encodeUtf8

interpretInstruction :: M.Instruction -> InterpM ()
interpretInstruction inst = case inst of
  M.Read var -> do
    s <- getState
    c <- getContext

    if M.stateJustRestarted s
      then do
      setVar var $ M.contextReadArgs c
      putState s { M.stateJustRestarted = False }
      modifyPC (+ 1)
      setExitCode 0

      else do
      let paths = M.contextPaths c
          asm = M.contextAssembly c
      iState <- liftIO $ freezeState s
      liftIO $ writeFile (M.pathASM paths) (show asm)
      liftIO $ writeFile (M.pathState paths) (show iState)
      liftIO Exit.exitSuccess

  M.Run cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> pure Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    liftIO $ putStr out
    modifyPC (+ 1)
    setExitCode ec

  M.AssignRun v cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> pure Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    setVar v $ T.pack $ L.dropWhileEnd isSpace out
    modifyPC (+ 1)
    setExitCode ec

  M.Assign v parts -> do
    parts' <- evalParts parts
    setVar v parts'
    modifyPC (+ 1)
    setExitCode 0

  M.JumpIfRetZero p -> do
    ec <- M.statePrevExitCode <$> getState
    if (ec == 0)
      then setPC p
      else modifyPC (+ 1)
    setExitCode 0

  M.Jump p -> do
    setPC p
    setExitCode 0

  M.Exit -> do
    paths <- M.contextPaths <$> getContext
    liftIO $ flip catch (\e -> (e :: IOException) `seq` pure ()) $ do
      Dir.removeFile $ M.pathASM paths
      Dir.removeFile $ M.pathState paths
    liftIO Exit.exitSuccess
