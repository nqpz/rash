module Rash.IOStateKeeping
  ( dumpState
  , retrieveState
  ) where

import Control.Monad (when)
import qualified System.Directory as Dir
import qualified System.Exit as Exit
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as May
import qualified Data.Array.MArray as MA

import Rash.SequenceUtilities (Sequence, listToSequence)
import Rash.Parser (parseFile)
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

asmParseToInternal :: RP.Assembly -> (RI.Assembly, Int)
asmParseToInternal (RP.Assembly insts) = (RI.Assembly $ listToSequence insts'', nVars)
  where insts' :: [RP.Instruction]
        insts' = filter (not . isParseLabel) insts

        insts'' :: [RI.Instruction]
        insts'' = map instConv insts'

        commandConv :: RP.Command -> RI.Command
        commandConv (RP.Command cmd stdinM) = RI.Command (partsConv cmd) (partsConv <$> stdinM)

        instConv :: RP.Instruction -> RI.Instruction
        instConv = \case
          RP.Read tid ->
            RI.Read (varMap M.! tid)
          RP.Run command ->
            RI.Run (commandConv command)
          RP.AssignRun v command ->
            RI.AssignRun (varMap M.! v) (commandConv command)
          RP.Assign v parts ->
            RI.Assign (varMap M.! v) (partsConv parts)
          RP.JumpIfRetZero label ->
            RI.JumpIfRetZero (labelPoss M.! label)
          RP.Jump label ->
            RI.Jump (labelPoss M.! label)
          RP.Exit ->
            RI.Exit
          RP.Label _ ->
            error "FATAL: all labels should have been removed"

        partsConv :: [RP.Part] -> Sequence RI.Part
        partsConv = listToSequence . map partConv

        partConv :: RP.Part -> RI.Part
        partConv = \case
          RP.TextPart s -> RI.TextPart $ T.pack s
          RP.IDPart b v -> RI.IDPart b (varMap M.! v)

        labelPoss :: M.Map RP.Label Int
        labelPoss = M.fromList $ buildPs insts 0

        buildPs :: [RP.Instruction] -> Int -> [(RP.Label, Int)]
        buildPs [] _ = []
        buildPs (i : is) n = case i of
          RP.Label label -> (label, n) : buildPs is n
          _ -> buildPs is (n + 1)

        nVars :: Int
        nVars = M.size varMap

        varMap :: M.Map RP.ID RI.ID
        varMap = M.fromList $ zip (L.nub (L.concatMap instVars insts')) [0..]

        instVars :: RP.Instruction -> [RP.ID]
        instVars = \case
          RP.Read v ->
            [v]
          RP.Run (RP.Command ps Nothing) ->
            partsVars ps
          RP.Run (RP.Command ps0 (Just ps1)) ->
            partsVars (ps0 ++ ps1)
          RP.AssignRun v (RP.Command ps Nothing) ->
            [v] ++ partsVars ps
          RP.AssignRun v (RP.Command ps0 (Just ps1)) ->
            [v] ++ partsVars (ps0 ++ ps1)
          RP.Assign v ps ->
            [v] ++ partsVars ps
          _ ->
            []

        partsVars :: [RP.Part] -> [RP.ID]
        partsVars = May.catMaybes . map partVar

        partVar :: RP.Part -> Maybe RP.ID
        partVar = \case
          RP.TextPart _ -> Nothing
          RP.IDPart _ v -> Just v

isParseLabel :: RP.Instruction -> Bool
isParseLabel (RP.Label _) = True
isParseLabel _ = False
