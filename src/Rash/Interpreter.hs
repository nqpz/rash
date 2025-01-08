module Rash.Interpreter
  ( interpret
  ) where

import Control.Monad (void)
import Control.Monad.Reader (ReaderT, MonadReader, runReaderT, ask)
import Control.Monad.State (StateT, MonadState, runStateT, get, put, modify, MonadIO, liftIO)
import Control.Exception

import qualified Text.ShellEscape as TSE

import qualified System.Exit as Exit
import qualified System.Process as Proc

import Data.Char
import qualified Data.Array.IArray as IA
import qualified Data.Array.MArray as MA
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.List as L

import Rash.Array (Array, sequenceLength, sequenceToList)
import qualified Rash.Representation.Internal as RI
import Rash.IOStateKeeping (dumpState, cleanState)


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


newtype InterpM a = InterpM { unInterpM :: ReaderT RI.Context (StateT RI.State IO) a
                            }
  deriving (Functor, Applicative, Monad, MonadReader RI.Context, MonadState RI.State, MonadIO)

runInterpM :: InterpM a -> RI.Context -> RI.State -> IO (a, RI.State)
runInterpM m context = runStateT (runReaderT (unInterpM m) context)

setPC :: Int -> InterpM ()
setPC pc = modify (\s -> s { RI.statePC = pc })

incrPC :: InterpM ()
incrPC = do
  pc <- RI.statePC <$> get
  setPC (pc + 1)

setExitCode :: Int -> InterpM ()
setExitCode ec = modify (\s -> s { RI.statePrevExitCode = ec })

getVar :: Int -> InterpM T.Text
getVar i = do
  vars <- RI.stateVars <$> get
  liftIO $ MA.readArray vars i

setVar :: Int -> T.Text -> InterpM ()
setVar i t = do
  vars <- RI.stateVars <$> get
  liftIO $ MA.writeArray vars i t

interpret :: RI.Context -> RI.State -> IO ()
interpret c s = fst <$> runInterpM (interpretM 0) c s

interpretM :: Int -> InterpM ()
interpretM nSteps
  | nSteps > maxNSteps = liftIO $ putStrLn "Too many steps; stopping."
  | otherwise = do
    pc <- RI.statePC <$> get
    RI.Assembly insts <- RI.contextAssembly <$> ask
    if pc >= sequenceLength insts
      then void $ interpretInstruction RI.Exit
      else do
      let instCur = insts IA.! pc
      continue <- interpretInstruction instCur
      if continue
        then interpretM (nSteps + 1)
        else do
        k <- RI.contextIOStateKeeping <$> ask
        case k of
          RI.WriteAndReadFiles _ ->
            liftIO Exit.exitSuccess
          RI.InMemory _ _ _ signalExit ->
            liftIO signalExit

evalParts :: Array RI.Part -> InterpM T.Text
evalParts ps = do
  let ps1 = extractParts ps
      ps2 = sequenceToList ps1
  ps3 <- mapM id ps2
  pure $ T.concat ps3

extractParts :: Array RI.Part -> Array (InterpM T.Text)
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

interpretInstruction :: RI.Instruction -> InterpM Bool
interpretInstruction = \case
  RI.Read var -> do
    s <- get
    c <- ask

    if RI.stateJustRestarted s
      then do
      setVar var $ RI.contextReadArgs c
      put s { RI.stateJustRestarted = False }
      incrPC
      setExitCode 0
      pure True
      else do
      liftIO $ dumpState (RI.contextAssembly c) s (RI.contextIOStateKeeping c)
      pure False

  RI.Run command -> do
    (ec, out) <- interpretCommand command
    k <- RI.contextIOStateKeeping <$> ask
    case k of
      RI.WriteAndReadFiles _ ->
        liftIO $ putStr out
      RI.InMemory _ _ add _ ->
        liftIO $ add out
    incrPC
    setExitCode ec
    pure True

  RI.AssignRun v command -> do
    (ec, out) <- interpretCommand command
    setVar v $ T.pack $ L.dropWhileEnd isSpace out
    incrPC
    setExitCode ec
    pure True

  RI.Assign v parts -> do
    parts' <- evalParts parts
    setVar v parts'
    incrPC
    setExitCode 0
    pure True

  RI.JumpIfRetZero p -> do
    ec <- RI.statePrevExitCode <$> get
    if (ec == 0)
      then setPC p
      else incrPC
    setExitCode 0
    pure True

  RI.Jump p -> do
    setPC p
    setExitCode 0
    pure True

  RI.Exit -> do
    c <- ask
    liftIO $ flip catch (\e -> (e :: IOException) `seq` pure ()) $ cleanState (RI.contextIOStateKeeping c)
    pure False
