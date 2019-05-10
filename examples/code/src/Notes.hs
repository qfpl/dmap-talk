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

import Data.Time.Clock (UTCTime, getCurrentTime)

import Control.Monad.Primitive
import Data.Unique.Tag

import Data.Dependent.Sum (EqTag(..), OrdTag(..), ShowTag(..))
import Data.Dependent.Map (DMap)
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare.TH
import Data.GADT.Show.TH

data NoteEntryKey a where
  NEKNote :: (Eq a, Ord a, Show a)
          => UTCTime
          -> Tag (PrimState IO) a
          -> NoteEntryKey a

deriveGEq ''NoteEntryKey
deriveGCompare ''NoteEntryKey
deriveGShow ''NoteEntryKey

instance Eq1 f => EqTag NoteEntryKey f where
  eqTagged (NEKNote u1 t1) (NEKNote u2 t2)
    | u1 == u2 && t1 == t2 = eq1
    | otherwise = \_ _ -> False

instance Ord1 f => OrdTag NoteEntryKey f where
  compareTagged (NEKNote u1 t1) (NEKNote u2 t2) =
    case compare (u1, t1) (u2, t2) of
      LT -> \_ _ -> LT
      GT -> \_ _ -> GT
      EQ -> compare1

instance Show1 f => ShowTag NoteEntryKey f where
  showTaggedPrec (NEKNote _ _) = showsPrec1

type Notes f = DMap NoteEntryKey f

addNote :: (Eq a, Ord a, Show a) => a -> Notes Identity -> IO (Notes Identity)
addNote o n = do
  utc <- getCurrentTime
  tag <- newTag
  pure . DMap.insert (NEKNote utc tag) (Identity o) $ n

noteExample :: IO ()
noteExample = do
  let ns = DMap.empty
  ns'  <- addNote (1 :: Int) ns
  ns'' <- addNote (False :: Bool) ns'
  print ns''
