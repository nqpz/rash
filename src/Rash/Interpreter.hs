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

import Rash.Utilities (Sequence, sequenceLength, sequenceToList)
import Rash.EndModel


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


data InterpM a = InterpM { runInterpM :: Context -> State -> IO (a, State)
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

getContext :: InterpM Context
getContext = InterpM $ \c s -> pure (c, s)

getState :: InterpM State
getState = InterpM $ \_ s -> pure (s, s)

putState :: State -> InterpM ()
putState s = InterpM $ \_ _ -> pure ((), s)

setPC :: Int -> InterpM ()
setPC pc = do
  s <- getState
  putState s { statePC = pc }

modifyPC :: (Int -> Int) -> InterpM ()
modifyPC f = do
  pc <- statePC <$> getState
  setPC $ f pc

setExitCode :: Int -> InterpM ()
setExitCode ec = do
  s <- getState
  putState s { statePrevExitCode = ec }

getVar :: Int -> InterpM T.Text
getVar i = do
  vars <- stateVars <$> getState
  liftIO $ MA.readArray vars i

setVar :: Int -> T.Text -> InterpM ()
setVar i t = do
  vars <- stateVars <$> getState
  liftIO $ MA.writeArray vars i t

interpret :: Context -> State -> IO ()
interpret c s = fst <$> runInterpM (interpretM 0) c s

interpretM :: Int -> InterpM ()
interpretM nSteps
  | nSteps > maxNSteps = liftIO $ putStrLn "Too many steps; stopping."
  | otherwise = do
    pc <- statePC <$> getState
    Assembly insts <- contextAssembly <$> getContext
    if pc >= sequenceLength insts
      then interpretInstruction Exit
      else do
      let instCur = insts IA.! pc
      interpretInstruction instCur
      interpretM (nSteps + 1)

emptyState :: Int -> IO State
emptyState nVars = do
  vars <- MA.newArray (0, nVars - 1) (T.pack "")
  pure State { statePC = 0
             , stateVars = vars
             , stateJustRestarted = False
             , statePrevExitCode = 0
             }

freezeState :: State -> IO IState
freezeState st = do
  vars <- MA.freeze $ stateVars st
  pure $ IState { iStatePC = statePC st
                , iStateVars = vars
                }

thawState :: IState -> IO State
thawState ist = do
  vars <- MA.thaw $ iStateVars ist
  pure $ State { statePC = iStatePC ist
               , stateVars = vars
               , stateJustRestarted = True
               , statePrevExitCode = 0
               }

evalParts :: Sequence Part -> InterpM T.Text
evalParts ps = do
  let ps1 = extractParts ps
      ps2 = sequenceToList ps1
  ps3 <- mapM id ps2
  pure $ T.concat ps3

extractParts :: Sequence Part -> Sequence (InterpM T.Text)
extractParts = IA.amap extractPart

extractPart :: Part -> InterpM T.Text
extractPart p = case p of
  TextPart t -> pure t
  IDPart b v -> do r <- getVar v
                   pure $ if b then shEsc r else r
          where shEsc = TE.decodeUtf8 . TSE.bytes . TSE.sh . TE.encodeUtf8

interpretInstruction :: Instruction -> InterpM ()
interpretInstruction inst = case inst of
  Read var -> do
    s <- getState
    c <- getContext

    if stateJustRestarted s
      then do
      setVar var $ contextReadArgs c
      putState s { stateJustRestarted = False }
      modifyPC (+ 1)
      setExitCode 0

      else do
      let paths = contextPaths c
          asm = contextAssembly c
      iState <- liftIO $ freezeState s
      liftIO $ writeFile (pathASM paths) (show asm)
      liftIO $ writeFile (pathState paths) (show iState)
      liftIO Exit.exitSuccess

  Run cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> pure Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    liftIO $ putStr out
    modifyPC (+ 1)
    setExitCode ec

  AssignRun v cmd stdinM -> do
    cmd' <- evalParts cmd
    stdinM' <- case stdinM of
      Nothing -> pure Nothing
      Just stdin -> Just <$> evalParts stdin
    (ec, out) <- liftIO $ runProcess (T.unpack cmd') (T.unpack <$> stdinM')
    setVar v $ T.pack $ L.dropWhileEnd isSpace out
    modifyPC (+ 1)
    setExitCode ec

  Assign v parts -> do
    parts' <- evalParts parts
    setVar v parts'
    modifyPC (+ 1)
    setExitCode 0

  JumpIfRetZero p -> do
    ec <- statePrevExitCode <$> getState
    if (ec == 0)
      then setPC p
      else modifyPC (+ 1)
    setExitCode 0

  Jump p -> do
    setPC p
    setExitCode 0

  Exit -> do
    paths <- contextPaths <$> getContext
    liftIO $ flip catch (\e -> (e :: IOException) `seq` pure ()) $ do
      Dir.removeFile $ pathASM paths
      Dir.removeFile $ pathState paths
    liftIO Exit.exitSuccess
