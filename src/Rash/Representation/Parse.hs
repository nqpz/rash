module Rash.Representation.Parse
  ( ID
  , Label
  , Part(..)
  , Command(..)
  , Instruction(..)
  , Assembly(..)
  ) where

type ID = String

type Label = String

data Part = TextPart String
          | IDPart Bool ID
          deriving (Show)

data Command = Command { commandParts :: [Part]
                       , assignStdin :: Maybe [Part]
                       }
             deriving (Show)

data Instruction = Read { assignID :: ID
                        }
                 | Run { command :: Command
                       }
                 | AssignRun { assignID :: ID
                             , command :: Command
                             }
                 | Assign { assignID :: ID
                          , contentParts :: [Part]
                          }
                 | JumpIfRetZero { jumpPos :: Label
                                 }
                 | Jump { jumpPos :: Label
                        }
                 | Exit
                 | Label { label :: Label
                         }
                 deriving (Show)

data Assembly = Assembly [Instruction]
              deriving (Show)
