module Rash.Runner
  ( runFile
  ) where

import Control.Monad

import qualified System.Environment as Env
import qualified System.Directory as Dir
import qualified System.FilePath as Path
import qualified System.Exit as Exit

import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe as May

import Rash.SequenceUtilities (Sequence, listToSequence)
import qualified Rash.EndModel as M
import qualified Rash.TemporaryModel as MT
import Rash.Interpreter (interpret, emptyState, thawState)
import Rash.Parser (parseFile)


runFile :: FilePath -> String -> IO ()
runFile fname readArgs = do
  paths <- rashPaths fname
  Dir.createDirectoryIfMissing True $ M.pathDir paths

  -- The following code is fragile.
  asmExists <- Dir.doesFileExist $ M.pathASM paths
  stateExists <- Dir.doesFileExist $ M.pathState paths
  when (asmExists /= stateExists) $ do
    when asmExists $ Dir.removeFile $ M.pathASM paths
    when stateExists $ Dir.removeFile $ M.pathState paths
    Exit.exitFailure
  let exists = asmExists -- any of the two will do

  (asm, state) <-
    if exists
    then do
      a <- (read <$> (readFile $ M.pathASM paths))
      i <- (read <$> (readFile $ M.pathState paths))
      s <- thawState i
      pure (a, s)
    else do
      res <- parseFile fname
      case res of
        Left errorMessage -> do
          print errorMessage
          Exit.exitFailure
        Right insts -> do
          let insts' = MT.TempAssign "initial_arguments" [MT.TempTextPart readArgs] : insts
          let (a, nVars) = asmTempToAsm $ MT.TempAssembly insts'
          s <- emptyState nVars
          pure (a, s)

  let context = M.Context { M.contextAssembly = asm
                          , M.contextReadArgs = T.pack readArgs
                          , M.contextPaths = paths
                          }
  interpret context state

isTempLabel :: MT.TempInstruction -> Bool
isTempLabel (MT.TempLabel _) = True
isTempLabel _ = False

asmTempToAsm :: MT.TempAssembly -> (M.Assembly, Int)
asmTempToAsm (MT.TempAssembly insts) = (M.Assembly $ listToSequence insts'', nVars)
  where insts' :: [MT.TempInstruction]
        insts' = filter (not . isTempLabel) insts

        insts'' :: [M.Instruction]
        insts'' = map instConv insts'

        instConv :: MT.TempInstruction -> M.Instruction
        instConv ti = case ti of
          MT.TempRead tid -> M.Read (varMap M.! tid)
          MT.TempRun cmd stdinM -> M.Run (partsConv cmd) (partsConv <$> stdinM)
          MT.TempAssignRun v cmd stdinM ->
            M.AssignRun (varMap M.! v) (partsConv cmd) (partsConv <$> stdinM)
          MT.TempAssign v parts -> M.Assign (varMap M.! v) (partsConv parts)
          MT.TempJumpIfRetZero label -> M.JumpIfRetZero (labelPoss M.! label)
          MT.TempJump label -> M.Jump (labelPoss M.! label)
          MT.TempExit -> M.Exit
          MT.TempLabel _ -> error "FATAL: all labels should have been removed"

        partsConv :: [MT.TempPart] -> Sequence M.Part
        partsConv = listToSequence . map partConv

        partConv :: MT.TempPart -> M.Part
        partConv p = case p of
          MT.TempTextPart s -> M.TextPart $ T.pack s
          MT.TempIDPart b v -> M.IDPart b (varMap M.! v)

        labelPoss :: M.Map MT.TempLabel Int
        labelPoss = M.fromList $ buildPs insts 0

        buildPs :: [MT.TempInstruction] -> Int -> [(MT.TempLabel, Int)]
        buildPs [] _ = []
        buildPs (i : is) n = case i of
          MT.TempLabel label -> (label, n) : buildPs is n
          _ -> buildPs is (n + 1)

        nVars :: Int
        nVars = M.size varMap

        varMap :: M.Map MT.TempID M.ID
        varMap = M.fromList $ zip (L.nub (L.concatMap instVars insts')) [0..]

        instVars :: MT.TempInstruction -> [MT.TempID]
        instVars inst = case inst of
          MT.TempRead v -> [v]
          MT.TempRun ps Nothing -> partsVars ps
          MT.TempRun ps0 (Just ps1) -> partsVars (ps0 ++ ps1)
          MT.TempAssignRun v ps Nothing -> [v] ++ partsVars ps
          MT.TempAssignRun v ps0 (Just ps1) -> [v] ++ partsVars (ps0 ++ ps1)
          MT.TempAssign v ps -> [v] ++ partsVars ps
          _ -> []

        partsVars :: [MT.TempPart] -> [MT.TempID]
        partsVars = May.catMaybes . map partVar

        partVar :: MT.TempPart -> Maybe MT.TempID
        partVar p = case p of
          MT.TempTextPart _ -> Nothing
          MT.TempIDPart _ v -> Just v

rashPaths :: FilePath -> IO M.RashPaths
rashPaths fname = do
  fnameCanon <- Dir.canonicalizePath fname
  dbDir <- Env.getEnv "CONCIEGGS_DB_DIR"
  let fnameSave = Path.combine dbDir "rash" ++ fnameCanon
      dirSave = fst $ Path.splitFileName fnameSave
      asmSave = fnameSave ++ ".asm"
      stateSave = fnameSave ++ ".state"
  pure M.RashPaths { M.pathOrig = fname
                   , M.pathDir = dirSave
                   , M.pathASM = asmSave
                   , M.pathState = stateSave
                   }
