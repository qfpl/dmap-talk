module Details.Common where

newtype Id = Id { getId :: Int }
  deriving (Eq, Ord, Show)

newtype Weight = Weight { getWeight :: Int }
  deriving (Eq, Ord, Show)
