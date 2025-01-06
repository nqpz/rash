module Rash.IOStateKeeping
  ( dumpState
  , retrieveState
  ) where

import Control.Monad (when)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified Data.Text as T
import qualified Data.Array.MArray as MA

import Rash.Parser (parseFile)
import Rash.Representation.ParseToInternal (asmParseToInternal)
import qualified Rash.Representation.Parse as RP
import qualified Rash.Representation.Internal as RI

dumpState :: RI.RashPaths -> RI.Assembly -> RI.IState -> RI.IOStateKeeping -> IO ()
dumpState paths asm iState = \case
  RI.WriteAndReadFiles -> do
    writeFile (RI.pathASM paths) (show asm) -- TODO: Do we need to write the assembly every time if there are multiple reads?
    writeFile (RI.pathState paths) (show iState)

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
      Exit.exitFailure

    if asmExists
    then do
      a <- read <$> (readFile $ RI.pathASM paths)
      i <- read <$> (readFile $ RI.pathState paths)
      s <- thawState i
      pure (a, s)
    else do
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

emptyState :: Int -> IO RI.State
emptyState nVars = do
  vars <- MA.newArray (0, nVars - 1) (T.pack "")
  pure RI.State { RI.statePC = 0
                , RI.stateVars = vars
                , RI.stateJustRestarted = False
                , RI.statePrevExitCode = 0
                }

thawState :: RI.IState -> IO RI.State
thawState ist = do
  vars <- MA.thaw $ RI.iStateVars ist
  pure $ RI.State { RI.statePC = RI.iStatePC ist
                  , RI.stateVars = vars
                  , RI.stateJustRestarted = True
                  , RI.statePrevExitCode = 0
                  }
