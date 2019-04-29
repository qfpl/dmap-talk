{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
module Scratch where

import Data.Constraint.Extras
import Data.Constraint.Extras.TH

import Data.Dependent.Sum (DSum(..), ShowTag(..))
import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap
import Data.GADT.Compare.TH
import Data.GADT.Show.TH

import Diagrams.GG

data DemoKey a where
  DK_X :: DemoKey Double
  DK_Y :: DemoKey Double
  DK_Z :: DemoKey Int

deriveGEq ''DemoKey
deriveGCompare ''DemoKey
deriveGShow ''DemoKey
deriveArgDict ''DemoKey

instance Has' Show DemoKey f => ShowTag DemoKey f where
  showTaggedPrec k n x = has' @Show @f k (showsPrec n x)

instance FromEntry DemoKey where
  type Entry DemoKey = (Double, Double, Maybe Int)
  fromEntry (x, y, mz) =
    maybe id (DMap.insert DK_Z . pure) mz $
    DMap.fromList [ DK_X :=> pure x
                  , DK_Y :=> pure y
                  ]

demoMetadata :: DMap DemoKey ColumnMetadata
demoMetadata =
  DMap.fromList
    [ DK_X :=> ColumnMetadata "X" OMSkip
    , DK_Y :=> ColumnMetadata "Y" OMSkip
    , DK_Z :=> ColumnMetadata "Z" (OMDefault 0)
    ]

demoData :: [(Double, Double, Maybe Int)]
demoData =
  [ (1, 1, Nothing)
  , (2, 3, Nothing)
  , (3, 4, Just 1)
  , (4, 6, Nothing)
  ]

demoLayer :: Layer DemoKey []
demoLayer =
  let
    -- m = Mapping (This DK_X) (This DK_Y) (Just (This DK_Z)) Nothing
    m = Mapping DK_X DK_Y
  in
    Layer (loadColumns demoMetadata demoData) m
