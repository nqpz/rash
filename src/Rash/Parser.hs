module Rash.Parser
  ( parseFile
  ) where

import Control.Monad
import Control.Applicative

import qualified Text.Parsec as P
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import qualified Rash.Representation.Parse as RP


parseFile :: FilePath -> IO (Either P.ParseError [RP.Instruction])
parseFile = P.parseFromFile instructions

symbol :: String -> P.Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: P.Parser ()
lineEnd = void $ P.satisfy isLineEnd

notLineEnd :: P.Parser Char
notLineEnd = P.satisfy (not . isLineEnd)

instructions :: P.Parser [RP.Instruction]
instructions =
  May.catMaybes <$>
  (P.sepEndBy ((comment >> pure Nothing)
               P.<|> ((Just . RP.Label) <$> label)
               P.<|> (exit >> pure (Just RP.Exit))
               P.<|> ((Just . RP.Read) <$> readInstruction)
               P.<|> ((Just . RP.Jump) <$> jump)
               P.<|> ((Just . RP.JumpIfRetZero) <$> jumpZero)
               P.<|> ((Just . uncurry RP.Run) <$> run)
               P.<|> (Just <$> assignGeneral)
               P.<?> "instruction")
   (P.many lineEnd))

stringParts :: String -> [RP.Part]
stringParts s = case P.parse (P.many stringPart) "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps
  where stringPart :: P.Parser RP.Part
        stringPart = (RP.IDPart False <$> idPart "$")
                     P.<|> (RP.IDPart True <$> idPart "$'")
                     P.<|> (RP.TextPart <$> textPart)

idPart :: String -> P.Parser RP.ID
idPart p = do
  symbol (p ++ "{")
  var <- P.many1 (P.satisfy (/= '}'))
  symbol "}"
  pure var

textPart :: P.Parser String
textPart = P.many1 (P.satisfy (/= '$'))

comment :: P.Parser ()
comment = do
  symbol "#"
  void $ P.many notLineEnd

label :: P.Parser RP.Label
label = do
  symbol ":"
  P.many1 notLineEnd

exit :: P.Parser ()
exit = P.try $ do
  symbol "exit"
  lineEnd

readInstruction :: P.Parser RP.ID
readInstruction = do
  symbol "read "
  P.many1 notLineEnd

jump :: P.Parser RP.Label
jump = do
  symbol "j "
  P.many1 notLineEnd

jumpZero :: P.Parser RP.Label
jumpZero = do
  symbol "jz "
  P.many1 notLineEnd

run :: P.Parser ([RP.Part], Maybe [RP.Part])
run = ((, Nothing) <$> runNoStdin)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdin)
  where runNoStdin :: P.Parser [RP.Part]
        runNoStdin = do
          symbol ">"
          stringParts <$> P.many1 notLineEnd

        runStdin :: P.Parser ([RP.Part], [RP.Part])
        runStdin = do
          symbol "<"
          inp <- stringParts <$> P.many1 (P.satisfy (/= '>'))
          cmd <- runNoStdin
          pure (cmd, inp)

assignGeneral :: P.Parser RP.Instruction
assignGeneral = do
  var <- P.many1 (P.satisfy (/= '='))
  symbol "="
  ((uncurry (RP.AssignRun var) <$> run)
   <|> (RP.Assign var <$> (stringParts <$> P.many1 notLineEnd)))
