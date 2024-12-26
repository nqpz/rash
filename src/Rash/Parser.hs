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

unarySymbol :: String -> Parser String
unarySymbol s = do
  symbol (s ++ " ")
  many1 notLineEnd

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
               <|> (Just <$> runInstruction)
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
readInstruction = unarySymbol "read"

jump :: Parser RP.Label
jump = unarySymbol "j"

jumpZero :: Parser RP.Label
jumpZero = unarySymbol "jz"

runInstruction :: Parser RP.Instruction
runInstruction = RP.Run <$> command

command :: Parser RP.Command
command = runNoStdin <|> runStdin
  where cmd :: Parser [RP.Part]
        cmd = do
          symbol ">"
          stringParts <$> many1 notLineEnd

        runNoStdin :: Parser RP.Command
        runNoStdin = RP.Command <$> cmd <*> pure Nothing

        runStdin :: Parser RP.Command
        runStdin = do
          symbol "<"
          RP.Command <$> cmd <*> ((Just . stringParts) <$> many1 (satisfy (/= '>')))

assignInstruction :: Parser RP.Instruction
assignInstruction = do
  var <- many1 (satisfy (/= '='))
  symbol "="
  assignRun var <|> assign var
  where assignRun :: String -> Parser RP.Instruction
        assignRun var = RP.AssignRun var <$> command

        assign :: String -> Parser RP.Instruction
        assign var = RP.Assign var <$> (stringParts <$> many1 notLineEnd)
