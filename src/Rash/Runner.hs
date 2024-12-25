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
          let insts' = MT.Assign "initial_arguments" [MT.TextPart readArgs] : insts
          let (a, nVars) = asmTempToAsm $ MT.Assembly insts'
          s <- emptyState nVars
          pure (a, s)

  let context = M.Context { M.contextAssembly = asm
                          , M.contextReadArgs = T.pack readArgs
                          , M.contextPaths = paths
                          }
  interpret context state

isTempLabel :: MT.Instruction -> Bool
isTempLabel (MT.Label _) = True
isTempLabel _ = False

asmTempToAsm :: MT.Assembly -> (M.Assembly, Int)
asmTempToAsm (MT.Assembly insts) = (M.Assembly $ listToSequence insts'', nVars)
  where insts' :: [MT.Instruction]
        insts' = filter (not . isTempLabel) insts

        insts'' :: [M.Instruction]
        insts'' = map instConv insts'

        instConv :: MT.Instruction -> M.Instruction
        instConv ti = case ti of
          MT.Read tid -> M.Read (varMap M.! tid)
          MT.Run cmd stdinM -> M.Run (partsConv cmd) (partsConv <$> stdinM)
          MT.AssignRun v cmd stdinM ->
            M.AssignRun (varMap M.! v) (partsConv cmd) (partsConv <$> stdinM)
          MT.Assign v parts -> M.Assign (varMap M.! v) (partsConv parts)
          MT.JumpIfRetZero label -> M.JumpIfRetZero (labelPoss M.! label)
          MT.Jump label -> M.Jump (labelPoss M.! label)
          MT.Exit -> M.Exit
          MT.Label _ -> error "FATAL: all labels should have been removed"

        partsConv :: [MT.Part] -> Sequence M.Part
        partsConv = listToSequence . map partConv

        partConv :: MT.Part -> M.Part
        partConv p = case p of
          MT.TextPart s -> M.TextPart $ T.pack s
          MT.IDPart b v -> M.IDPart b (varMap M.! v)

        labelPoss :: M.Map MT.Label Int
        labelPoss = M.fromList $ buildPs insts 0

        buildPs :: [MT.Instruction] -> Int -> [(MT.Label, Int)]
        buildPs [] _ = []
        buildPs (i : is) n = case i of
          MT.Label label -> (label, n) : buildPs is n
          _ -> buildPs is (n + 1)

        nVars :: Int
        nVars = M.size varMap

        varMap :: M.Map MT.ID M.ID
        varMap = M.fromList $ zip (L.nub (L.concatMap instVars insts')) [0..]

        instVars :: MT.Instruction -> [MT.ID]
        instVars inst = case inst of
          MT.Read v -> [v]
          MT.Run ps Nothing -> partsVars ps
          MT.Run ps0 (Just ps1) -> partsVars (ps0 ++ ps1)
          MT.AssignRun v ps Nothing -> [v] ++ partsVars ps
          MT.AssignRun v ps0 (Just ps1) -> [v] ++ partsVars (ps0 ++ ps1)
          MT.Assign v ps -> [v] ++ partsVars ps
          _ -> []

        partsVars :: [MT.Part] -> [MT.ID]
        partsVars = May.catMaybes . map partVar

        partVar :: MT.Part -> Maybe MT.ID
        partVar p = case p of
          MT.TextPart _ -> Nothing
          MT.IDPart _ v -> Just v

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
