module Rash.Parser
  ( parseFile
  ) where

import Control.Monad (void)

import Text.Parsec ((<|>), (<?>), many, many1, satisfy)
import qualified Text.Parsec as P
import Text.Parsec.String (Parser)
import qualified Text.Parsec.String as P

import qualified Data.Maybe as May

import qualified Rash.Representation.Parse as RP


parseFile :: FilePath -> IO (Either P.ParseError [RP.Instruction])
parseFile = P.parseFromFile instructions

symbol :: String -> Parser ()
symbol = P.try . void . P.string

isLineEnd :: Char -> Bool
isLineEnd c = c == '\n' || c == '\r'

lineEnd :: Parser ()
lineEnd = void $ satisfy isLineEnd

notLineEnd :: Parser Char
notLineEnd = satisfy (not . isLineEnd)

instructions :: Parser [RP.Instruction]
instructions =
  May.catMaybes <$>
  (P.sepEndBy ((comment >> pure Nothing)
               <|> ((Just . RP.Label) <$> label)
               <|> (exit >> pure (Just RP.Exit))
               <|> ((Just . RP.Read) <$> readInstruction)
               <|> ((Just . RP.Jump) <$> jump)
               <|> ((Just . RP.JumpIfRetZero) <$> jumpZero)
               <|> ((Just . uncurry RP.Run) <$> run)
               <|> (Just <$> assignInstruction)
               <?> "instruction")
   (many lineEnd))

stringParts :: String -> [RP.Part]
stringParts s = case P.parse (many stringPart) "input" s of
  Left _ -> [] -- no errors anyway
  Right ps -> ps
  where stringPart :: Parser RP.Part
        stringPart = (RP.IDPart False <$> idPart "$")
                     <|> (RP.IDPart True <$> idPart "$'")
                     <|> (RP.TextPart <$> textPart)

idPart :: String -> Parser RP.ID
idPart p = do
  symbol (p ++ "{")
  var <- many1 (satisfy (/= '}'))
  symbol "}"
  pure var

textPart :: Parser String
textPart = many1 (satisfy (/= '$'))

comment :: Parser ()
comment = do
  symbol "#"
  void $ many notLineEnd

label :: Parser RP.Label
label = do
  symbol ":"
  many1 notLineEnd

exit :: Parser ()
exit = P.try $ do
  symbol "exit"
  lineEnd

readInstruction :: Parser RP.ID
readInstruction = do
  symbol "read "
  many1 notLineEnd

jump :: Parser RP.Label
jump = do
  symbol "j "
  many1 notLineEnd

jumpZero :: Parser RP.Label
jumpZero = do
  symbol "jz "
  many1 notLineEnd

run :: Parser ([RP.Part], Maybe [RP.Part])
run = ((, Nothing) <$> runNoStdin)
       <|> ((\(t, u) -> (t, Just u)) <$> runStdin)
  where runNoStdin :: Parser [RP.Part]
        runNoStdin = do
          symbol ">"
          stringParts <$> many1 notLineEnd

        runStdin :: Parser ([RP.Part], [RP.Part])
        runStdin = do
          symbol "<"
          inp <- stringParts <$> many1 (satisfy (/= '>'))
          cmd <- runNoStdin
          pure (cmd, inp)

assignInstruction :: Parser RP.Instruction
assignInstruction = do
  var <- many1 (satisfy (/= '='))
  symbol "="
  assignRun var <|> assign var
  where assignRun :: String -> Parser RP.Instruction
        assignRun var = uncurry (RP.AssignRun var) <$> run

        assign :: String -> Parser RP.Instruction
        assign var = RP.Assign var <$> (stringParts <$> many1 notLineEnd)
