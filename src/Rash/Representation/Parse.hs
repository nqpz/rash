module Rash.Representation.Parse
  ( ID
  , Label
  , Part(..)
  , Instruction(..)
  , Assembly(..)
  ) where

type ID = String

type Label = String

data Part = TextPart String
          | IDPart Bool ID
          deriving (Show)

data Instruction = Read { tempAssignID :: ID
                        }
                 | Run { tempCommandParts :: [Part]
                       , tempAssignStdin :: Maybe [Part]
                       }
                 | AssignRun { tempAssignID :: ID
                             , tempCommandParts :: [Part]
                             , tempAssignStdin :: Maybe [Part]
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
