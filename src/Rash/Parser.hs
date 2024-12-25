module Rash.Parser
  ( parseFile
  ) where

import Control.Monad
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import qualified Rash.TemporaryModel as MT


parseFile :: FilePath -> IO (Either P.ParseError [MT.TempInstruction])
parseFile = P.parseFromFile instructionsP

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

stringParts :: String -> [MT.TempPart]
stringParts s = case P.parse stringPartsP "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps

stringPartsP :: P.Parser [MT.TempPart]
stringPartsP = P.many stringPartP

stringPartP :: P.Parser MT.TempPart
stringPartP = (MT.TempIDPart False <$> idPartP "$")
              P.<|> (MT.TempIDPart True <$> idPartP "$'")
              P.<|> (MT.TempTextPart <$> textPartP)

idPartP :: String -> P.Parser MT.TempID
idPartP p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  pure var

textPartP :: P.Parser String
textPartP = P.many1 (P.satisfy (/= '$'))

instructionsP :: P.Parser [MT.TempInstruction]
instructionsP =
  May.catMaybes <$>
  (P.sepEndBy ((commentP >> pure Nothing)
               P.<|> ((Just . MT.TempLabel) <$> labelP)
               P.<|> (exitP >> pure (Just MT.TempExit))
               P.<|> ((Just . MT.TempRead) <$> readP)
               P.<|> ((Just . MT.TempJump) <$> jumpP)
               P.<|> ((Just . MT.TempJumpIfRetZero) <$> jumpZeroP)
               P.<|> ((Just . uncurry MT.TempRun) <$> runP)
               P.<|> (Just <$> assignGeneralP)
               P.<?> "instruction")
   (many lineEnd))

commentP :: P.Parser ()
commentP = do
  symbol "#"
  void $ P.many notLineEnd

labelP :: P.Parser MT.TempLabel
labelP = do
  symbol ":"
  P.many1 notLineEnd

exitP :: P.Parser ()
exitP = P.try $ do
  symbol "exit"
  lineEnd

readP :: P.Parser MT.TempID
readP = do
  symbol "read "
  P.many1 notLineEnd

jumpP :: P.Parser MT.TempLabel
jumpP = do
  symbol "j "
  P.many1 notLineEnd

jumpZeroP :: P.Parser MT.TempLabel
jumpZeroP = do
  symbol "jz "
  P.many1 notLineEnd

runP :: P.Parser ([MT.TempPart], Maybe [MT.TempPart])
runP = ((, Nothing) <$> runNoStdinP)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdinP)

runNoStdinP :: P.Parser [MT.TempPart]
runNoStdinP = do
  symbol ">"
  stringParts <$> P.many1 notLineEnd

runStdinP :: P.Parser ([MT.TempPart], [MT.TempPart])
runStdinP = do
  symbol "<"
  inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
  cmd <- runNoStdinP
  pure (cmd, inp)

assignGeneralP :: P.Parser MT.TempInstruction
assignGeneralP = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (MT.TempAssignRun var) <$> runP)
   <|> (MT.TempAssign var <$> (stringParts <$> P.many1 notLineEnd)))
