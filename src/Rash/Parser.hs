module Rash.Parser
  ( parseFile
  ) where

import Control.Monad
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import qualified Rash.TemporaryModel as MT


parseFile :: FilePath -> IO (Either P.ParseError [MT.Instruction])
parseFile = P.parseFromFile instructionsP

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

stringParts :: String -> [MT.Part]
stringParts s = case P.parse stringPartsP "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps

stringPartsP :: P.Parser [MT.Part]
stringPartsP = P.many stringPartP

stringPartP :: P.Parser MT.Part
stringPartP = (MT.IDPart False <$> idPartP "$")
              P.<|> (MT.IDPart True <$> idPartP "$'")
              P.<|> (MT.TextPart <$> textPartP)

idPartP :: String -> P.Parser MT.ID
idPartP p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  pure var

textPartP :: P.Parser String
textPartP = P.many1 (P.satisfy (/= '$'))

instructionsP :: P.Parser [MT.Instruction]
instructionsP =
  May.catMaybes <$>
  (P.sepEndBy ((commentP >> pure Nothing)
               P.<|> ((Just . MT.Label) <$> labelP)
               P.<|> (exitP >> pure (Just MT.Exit))
               P.<|> ((Just . MT.Read) <$> readP)
               P.<|> ((Just . MT.Jump) <$> jumpP)
               P.<|> ((Just . MT.JumpIfRetZero) <$> jumpZeroP)
               P.<|> ((Just . uncurry MT.Run) <$> runP)
               P.<|> (Just <$> assignGeneralP)
               P.<?> "instruction")
   (many lineEnd))

commentP :: P.Parser ()
commentP = do
  symbol "#"
  void $ P.many notLineEnd

labelP :: P.Parser MT.Label
labelP = do
  symbol ":"
  P.many1 notLineEnd

exitP :: P.Parser ()
exitP = P.try $ do
  symbol "exit"
  lineEnd

readP :: P.Parser MT.ID
readP = do
  symbol "read "
  P.many1 notLineEnd

jumpP :: P.Parser MT.Label
jumpP = do
  symbol "j "
  P.many1 notLineEnd

jumpZeroP :: P.Parser MT.Label
jumpZeroP = do
  symbol "jz "
  P.many1 notLineEnd

runP :: P.Parser ([MT.Part], Maybe [MT.Part])
runP = ((, Nothing) <$> runNoStdinP)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdinP)

runNoStdinP :: P.Parser [MT.Part]
runNoStdinP = do
  symbol ">"
  stringParts <$> P.many1 notLineEnd

runStdinP :: P.Parser ([MT.Part], [MT.Part])
runStdinP = do
  symbol "<"
  inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
  cmd <- runNoStdinP
  pure (cmd, inp)

assignGeneralP :: P.Parser MT.Instruction
assignGeneralP = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (MT.AssignRun var) <$> runP)
   <|> (MT.Assign var <$> (stringParts <$> P.many1 notLineEnd)))
