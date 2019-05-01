{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Details.Version2 where

import Data.Functor.Classes

import Data.Text (Text)
import Data.Time.Calendar (Day)

import Details.Common

data Details f =
  Details {
    dId :: f Id
  , dInitials :: f Text
  , dDOB :: f Day
  , dWeight :: f (Maybe Weight)
  }

deriving instance (Eq (f Id), Eq (f Text), Eq (f Day), Eq (f (Maybe Weight))) => Eq (Details f)
deriving instance (Ord (f Id), Ord (f Text), Ord (f Day), Ord (f (Maybe Weight))) => Ord (Details f)
deriving instance (Show (f Id), Show (f Text), Show (f Day), Show (f (Maybe Weight))) => Show (Details f)

-- instance Eq1 f => Eq (Details f) where
--   Details id1 in1 d1 == Details id2 in2 d2 =
--     eq1 id1 id2 && eq1 in1 in2 && eq1 d1 d2

-- instance Ord1 f => Ord (Details f) where
--   compare (Details id1 in1 d1) (Details id2 in2 d2) =
--     case compare1 id1 id2 of
--       LT -> LT
--       GT -> GT
--       EQ -> case compare1 in1 in2 of
--         LT -> LT
--         GT -> GT
--         EQ -> compare1 d1 d2
