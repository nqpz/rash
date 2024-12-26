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

data Command = Command { tempCommandParts :: [Part]
                       , tempAssignStdin :: Maybe [Part]
                       }
             deriving (Show)

data Instruction = Read { tempAssignID :: ID
                        }
                 | Run { command :: Command
                       }
                 | AssignRun { tempAssignID :: ID
                             , command :: Command
                             }
                 | Assign { tempAssignID :: ID
                          , tempContentParts :: [Part]
                          }
                 | JumpIfRetZero { tempJumpPos :: Label
                                 }
                 | Jump { tempJumpPos :: Label
                        }
                 | Exit
                 | Label { tempLabel :: Label
                         }
                 deriving (Show)

data Assembly = Assembly [Instruction]
              deriving (Show)
