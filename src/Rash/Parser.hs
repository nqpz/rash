module Rash.Parser
  ( parseFile
  ) where

import Control.Monad
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import qualified Rash.ParsedRepresentation as RP


parseFile :: FilePath -> IO (Either P.ParseError [RP.Instruction])
parseFile = P.parseFromFile instructionsP

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

stringParts :: String -> [RP.Part]
stringParts s = case P.parse stringPartsP "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps

stringPartsP :: P.Parser [RP.Part]
stringPartsP = P.many stringPartP

stringPartP :: P.Parser RP.Part
stringPartP = (RP.IDPart False <$> idPartP "$")
              P.<|> (RP.IDPart True <$> idPartP "$'")
              P.<|> (RP.TextPart <$> textPartP)

idPartP :: String -> P.Parser RP.ID
idPartP p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  pure var

textPartP :: P.Parser String
textPartP = P.many1 (P.satisfy (/= '$'))

instructionsP :: P.Parser [RP.Instruction]
instructionsP =
  May.catMaybes <$>
  (P.sepEndBy ((commentP >> pure Nothing)
               P.<|> ((Just . RP.Label) <$> labelP)
               P.<|> (exitP >> pure (Just RP.Exit))
               P.<|> ((Just . RP.Read) <$> readP)
               P.<|> ((Just . RP.Jump) <$> jumpP)
               P.<|> ((Just . RP.JumpIfRetZero) <$> jumpZeroP)
               P.<|> ((Just . uncurry RP.Run) <$> runP)
               P.<|> (Just <$> assignGeneralP)
               P.<?> "instruction")
   (many lineEnd))

commentP :: P.Parser ()
commentP = do
  symbol "#"
  void $ P.many notLineEnd

labelP :: P.Parser RP.Label
labelP = do
  symbol ":"
  P.many1 notLineEnd

exitP :: P.Parser ()
exitP = P.try $ do
  symbol "exit"
  lineEnd

readP :: P.Parser RP.ID
readP = do
  symbol "read "
  P.many1 notLineEnd

jumpP :: P.Parser RP.Label
jumpP = do
  symbol "j "
  P.many1 notLineEnd

jumpZeroP :: P.Parser RP.Label
jumpZeroP = do
  symbol "jz "
  P.many1 notLineEnd

runP :: P.Parser ([RP.Part], Maybe [RP.Part])
runP = ((, Nothing) <$> runNoStdinP)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdinP)

runNoStdinP :: P.Parser [RP.Part]
runNoStdinP = do
  symbol ">"
  stringParts <$> P.many1 notLineEnd

runStdinP :: P.Parser ([RP.Part], [RP.Part])
runStdinP = do
  symbol "<"
  inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
  cmd <- runNoStdinP
  pure (cmd, inp)

assignGeneralP :: P.Parser RP.Instruction
assignGeneralP = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (RP.AssignRun var) <$> runP)
   <|> (RP.Assign var <$> (stringParts <$> P.many1 notLineEnd)))
