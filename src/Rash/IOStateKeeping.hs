module Rash.IOStateKeeping
  ( dumpState
  , retrieveState
  , cleanState
  ) where

import Control.Monad (when)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified Data.Text as T
import qualified Data.Array.MArray as MA
import Data.IORef (readIORef, writeIORef)

import Rash.Parser (parseFile)
import Rash.Representation.ParseToInternal (asmParseToInternal)
import qualified Rash.Representation.Parse as RP
import qualified Rash.Representation.Internal as RI

-- TODO: Do we need to write the assembly every time if there are multiple reads?
dumpState :: RI.RashPaths -> RI.Assembly -> RI.State -> RI.IOStateKeeping -> IO ()
dumpState paths asm state ioStateKeeping = do
  iState <- freezeState state
  case ioStateKeeping of
    RI.WriteAndReadFiles -> do
      writeFile (RI.pathASM paths) (show asm)
      writeFile (RI.pathState paths) (show iState)
    RI.InMemory asmMRef stateMRef _ -> do
      writeIORef asmMRef (Just asm)
      writeIORef stateMRef (Just iState)

retrieveState :: RI.RashPaths -> FilePath -> String -> RI.IOStateKeeping -> IO (RI.Assembly, RI.State)
retrieveState paths fname readArgs = \case
  RI.WriteAndReadFiles -> do
    Dir.createDirectoryIfMissing True $ RI.pathDir paths

    -- The following code is fragile.
    asmExists <- Dir.doesFileExist $ RI.pathASM paths
    stateExists <- Dir.doesFileExist $ RI.pathState paths
    when (asmExists /= stateExists) $ do
      when asmExists $ Dir.removeFile $ RI.pathASM paths
      when stateExists $ Dir.removeFile $ RI.pathState paths
      fail "unexpected"

    if asmExists
      then do
      asm <- read <$> (readFile $ RI.pathASM paths)
      iState <- read <$> (readFile $ RI.pathState paths)
      state <- thawState iState
      pure (asm, state)
      else do
      retrieveNewState fname readArgs

  RI.InMemory asmMRef stateMRef _ -> do
    asmM <- readIORef asmMRef
    stateM <- readIORef stateMRef
    case (asmM, stateM) of
      (Just asm, Just iState) -> do
        state <- thawState iState
        pure (asm, state)
      (Nothing, Nothing) -> do
        retrieveNewState fname readArgs
      (Nothing, Just _) ->
        fail "unexpected"
      (Just _, Nothing) ->
        fail "unexpected"

retrieveNewState :: FilePath -> String -> IO (RI.Assembly, RI.State)
retrieveNewState fname readArgs = do
  res <- parseFile fname
  case res of
    Left errorMessage -> do
      print errorMessage
      Exit.exitFailure
    Right insts -> do
      let insts' = RP.Assign "initial_arguments" [RP.TextPart readArgs] : insts
      let (a, nVars) = asmParseToInternal $ RP.Assembly insts'
      s <- emptyState nVars
      pure (a, s)

cleanState :: RI.RashPaths -> RI.IOStateKeeping -> IO ()
cleanState paths = \case
  RI.WriteAndReadFiles -> do
    Dir.removeFile $ RI.pathASM paths
    Dir.removeFile $ RI.pathState paths
  RI.InMemory _ _ _ ->
    pure ()

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
