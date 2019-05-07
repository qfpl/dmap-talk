{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Notes where

import Data.Functor.Classes
import Data.Functor.Identity

import Control.Monad.Primitive
import Data.Unique.Tag

import Data.Dependent.Sum (EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

data NoteEntryKey a where
  NEKNote :: (Eq a, Ord a, Show a) => Tag (PrimState IO) a -> NoteEntryKey a

deriveGEq ''NoteEntryKey
deriveGCompare ''NoteEntryKey
deriveGShow ''NoteEntryKey

instance Eq1 f => EqTag NoteEntryKey f where
  eqTagged (NEKNote t1) (NEKNote t2)
    | t1 == t2 = eq1
    | otherwise = \_ _ -> False

instance Ord1 f => OrdTag NoteEntryKey f where
  compareTagged (NEKNote t1) (NEKNote t2) =
    case compare t1 t2 of
      LT -> \_ _ -> LT
      GT -> \_ _ -> GT
      EQ -> compare1

instance Show1 f => ShowTag NoteEntryKey f where
  showTaggedPrec (NEKNote _) = showsPrec1

newtype Notes f = Notes { getNotes :: DMap NoteEntryKey f }

deriving instance Eq1 f => Eq (Notes f)
deriving instance Ord1 f => Ord (Notes f)
deriving instance Show1 f => Show (Notes f)

addNote :: (Eq a, Ord a, Show a) => a -> Notes Identity -> IO (Notes Identity)
addNote o n = do
  tag <- newTag
  pure . Notes . DMap.insert (NEKNote tag) (Identity o) . getNotes $ n

noteExample :: IO ()
noteExample = do
  let ns = Notes DMap.empty
  ns'  <- addNote (1 :: Int) ns
  ns'' <- addNote (False :: Bool) ns'
  print ns''
