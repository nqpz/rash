module Rash.TemporaryModel
  ( TempID
  , TempLabel
  , TempPart(..)
  , TempInstruction(..)
  , TempAssembly(..)
  ) where

type TempID = String

type TempLabel = String

data TempPart = TempTextPart String
              | TempIDPart Bool TempID
              deriving (Show)

data TempInstruction = TempRead { tempAssignID :: TempID
                                }
                     | TempRun { tempCommandParts :: [TempPart]
                               , tempAssignStdin :: Maybe [TempPart]
                               }
                     | TempAssignRun { tempAssignID :: TempID
                                     , tempCommandParts :: [TempPart]
                                     , tempAssignStdin :: Maybe [TempPart]
                                     }
                     | TempAssign { tempAssignID :: TempID
                                  , tempContentParts :: [TempPart]
                                  }
                     | TempJumpIfRetZero { tempJumpPos :: TempLabel
                                         }
                     | TempJump { tempJumpPos :: TempLabel
                                }
                     | TempExit
                     | TempLabel { tempLabel :: TempLabel
                                 }
                 deriving (Show)

data TempAssembly = TempAssembly [TempInstruction]
                  deriving (Show)


