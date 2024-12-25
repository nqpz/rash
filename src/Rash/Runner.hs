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

import Rash.Utilities (xor, Sequence, listToSequence, RashPaths(..))
import Rash.EndModel
import Rash.TemporaryModel
import Rash.Interpreter (interpret, emptyState, thawState)
import Rash.Parser (parseFile)

runFile :: FilePath -> String -> IO ()
runFile fname readArgs = do
  paths <- rashPaths fname
  Dir.createDirectoryIfMissing True $ pathDir paths

  -- The following code is fragile.
  asmExists <- Dir.doesFileExist $ pathASM paths
  stateExists <- Dir.doesFileExist $ pathState paths
  when (asmExists `xor` stateExists) $ do
    when asmExists $ Dir.removeFile $ pathASM paths
    when stateExists $ Dir.removeFile $ pathState paths
    Exit.exitFailure
  let exists = asmExists -- any of the two will do

  (asm, state) <-
    if exists
    then do
      a <- (read <$> (readFile $ pathASM paths))
      i <- (read <$> (readFile $ pathState paths))
      s <- thawState i
      pure (a, s)
    else do
      res <- parseFile fname
      case res of
        Left errorMessage -> do
          print errorMessage
          Exit.exitFailure
        Right insts -> do
          let insts' = TempAssign "initial_arguments" [TempTextPart readArgs] : insts
          let (a, nVars) = asmTempToAsm $ TempAssembly insts'
          s <- emptyState nVars
          pure (a, s)

  let context = Context { contextAssembly = asm
                        , contextReadArgs = T.pack readArgs
                        , contextPaths = paths
                        }
  interpret context state

isTempLabel :: TempInstruction -> Bool
isTempLabel (TempLabel _) = True
isTempLabel _ = False

asmTempToAsm :: TempAssembly -> (Assembly, Int)
asmTempToAsm (TempAssembly insts) = (Assembly $ listToSequence insts'', nVars)
  where insts' :: [TempInstruction]
        insts' = filter (not . isTempLabel) insts

        insts'' :: [Instruction]
        insts'' = map instConv insts'

        instConv :: TempInstruction -> Instruction
        instConv ti = case ti of
          TempRead tid -> Read (varMap M.! tid)
          TempRun cmd stdinM -> Run (partsConv cmd) (partsConv <$> stdinM)
          TempAssignRun v cmd stdinM ->
            AssignRun (varMap M.! v) (partsConv cmd) (partsConv <$> stdinM)
          TempAssign v parts -> Assign (varMap M.! v) (partsConv parts)
          TempJumpIfRetZero label -> JumpIfRetZero (labelPoss M.! label)
          TempJump label -> Jump (labelPoss M.! label)
          TempExit -> Exit
          TempLabel _ -> error "FATAL: all labels should have been removed"

        partsConv :: [TempPart] -> Sequence Part
        partsConv = listToSequence . map partConv

        partConv :: TempPart -> Part
        partConv p = case p of
          TempTextPart s -> TextPart $ T.pack s
          TempIDPart b v -> IDPart b (varMap M.! v)

        labelPoss :: M.Map TempLabel Int
        labelPoss = M.fromList $ buildPs insts 0

        buildPs :: [TempInstruction] -> Int -> [(TempLabel, Int)]
        buildPs [] _ = []
        buildPs (i : is) n = case i of
          TempLabel label -> (label, n) : buildPs is n
          _ -> buildPs is (n + 1)

        nVars :: Int
        nVars = M.size varMap

        varMap :: M.Map TempID ID
        varMap = M.fromList $ zip (L.nub (L.concatMap instVars insts')) [0..]

        instVars :: TempInstruction -> [TempID]
        instVars inst = case inst of
          TempRead v -> [v]
          TempRun ps Nothing -> partsVars ps
          TempRun ps0 (Just ps1) -> partsVars (ps0 ++ ps1)
          TempAssignRun v ps Nothing -> [v] ++ partsVars ps
          TempAssignRun v ps0 (Just ps1) -> [v] ++ partsVars (ps0 ++ ps1)
          TempAssign v ps -> [v] ++ partsVars ps
          _ -> []

        partsVars :: [TempPart] -> [TempID]
        partsVars = May.catMaybes . map partVar

        partVar :: TempPart -> Maybe TempID
        partVar p = case p of
          TempTextPart _ -> Nothing
          TempIDPart _ v -> Just v

rashPaths :: FilePath -> IO RashPaths
rashPaths fname = do
  fnameCanon <- Dir.canonicalizePath fname
  dbDir <- Env.getEnv "CONCIEGGS_DB_DIR"
  let fnameSave = Path.combine dbDir "rash" ++ fnameCanon
      dirSave = fst $ Path.splitFileName fnameSave
      asmSave = fnameSave ++ ".asm"
      stateSave = fnameSave ++ ".state"
  pure RashPaths { pathOrig = fname
                 , pathDir = dirSave
                 , pathASM = asmSave
                 , pathState = stateSave
                 }
