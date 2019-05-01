module Disease.Common where

data DiabetesInfo =
    Type1
  | Type2
  deriving (Eq, Ord, Show)

data CancerType =
    Breast
  | Lung
  | Bowel
  | Prostate
  deriving (Eq, Ord, Show)

data CancerStage =
    Stage1
  | Stage2
  | Stage3
  | Stage4
  deriving (Eq, Ord, Show)
