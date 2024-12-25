module Rash.Parser
  ( parseFile
  ) where

import Control.Monad
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import Rash.TemporaryModel


parseFile :: FilePath -> IO (Either P.ParseError [TempInstruction])
parseFile = P.parseFromFile instructionsP

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

stringParts :: String -> [TempPart]
stringParts s = case P.parse stringPartsP "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps

stringPartsP :: P.Parser [TempPart]
stringPartsP = P.many stringPartP

stringPartP :: P.Parser TempPart
stringPartP = (TempIDPart False <$> idPartP "$")
              P.<|> (TempIDPart True <$> idPartP "$'")
              P.<|> (TempTextPart <$> textPartP)

idPartP :: String -> P.Parser TempID
idPartP p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  pure var

textPartP :: P.Parser String
textPartP = P.many1 (P.satisfy (/= '$'))

instructionsP :: P.Parser [TempInstruction]
instructionsP =
  May.catMaybes <$>
  (P.sepEndBy ((commentP >> pure Nothing)
               P.<|> ((Just . TempLabel) <$> labelP)
               P.<|> (exitP >> pure (Just TempExit))
               P.<|> ((Just . TempRead) <$> readP)
               P.<|> ((Just . TempJump) <$> jumpP)
               P.<|> ((Just . TempJumpIfRetZero) <$> jumpZeroP)
               P.<|> ((Just . uncurry TempRun) <$> runP)
               P.<|> (Just <$> assignGeneralP)
               P.<?> "instruction")
   (many lineEnd))

commentP :: P.Parser ()
commentP = do
  symbol "#"
  void $ P.many notLineEnd

labelP :: P.Parser TempLabel
labelP = do
  symbol ":"
  P.many1 notLineEnd

exitP :: P.Parser ()
exitP = P.try $ do
  symbol "exit"
  lineEnd

readP :: P.Parser TempID
readP = do
  symbol "read "
  P.many1 notLineEnd

jumpP :: P.Parser TempLabel
jumpP = do
  symbol "j "
  P.many1 notLineEnd

jumpZeroP :: P.Parser TempLabel
jumpZeroP = do
  symbol "jz "
  P.many1 notLineEnd

runP :: P.Parser ([TempPart], Maybe [TempPart])
runP = ((, Nothing) <$> runNoStdinP)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdinP)

runNoStdinP :: P.Parser [TempPart]
runNoStdinP = do
  symbol ">"
  stringParts <$> P.many1 notLineEnd

runStdinP :: P.Parser ([TempPart], [TempPart])
runStdinP = do
  symbol "<"
  inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
  cmd <- runNoStdinP
  pure (cmd, inp)

assignGeneralP :: P.Parser TempInstruction
assignGeneralP = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (TempAssignRun var) <$> runP)
   <|> (TempAssign var <$> (stringParts <$> P.many1 notLineEnd)))
