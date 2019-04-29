{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE TypeFamilies #-}
module Diagrams.GG.Load where

import Data.Functor.Identity (Identity(..))

import Data.Dependent.Map (DMap, Some(..))
import qualified Data.Dependent.Map as DMap

import Data.GADT.Compare (GEq, GCompare)

import Diagrams.GG.Types

attachColumnMetaData :: GCompare k
                     => DMap k ColumnMetadata
                     -> DMap k f
                     -> DMap k (Column f)
attachColumnMetaData dm =
  DMap.intersectionWithKey (\_ v -> Column v) dm

handleMissing :: ColumnMetadata a
              -> Maybe a
              -> Maybe a
handleMissing _ (Just x) =
  Just x
handleMissing md Nothing =
  case _cmOnMissing md of
    OMSkip -> Nothing
    OMDefault x -> Just x

-- TODO would be nice to be able to work with more things that are foldable

gatherDatum :: GCompare k
           => DMap k ColumnMetadata
           -> DMap k Identity
           -> Maybe (DMap k Identity)
gatherDatum mds x =
  DMap.traverseWithKey (\k md -> fmap Identity $ handleMissing md (fmap runIdentity $ DMap.lookup k x)) mds

gatherData :: GCompare k
           => DMap k ColumnMetadata
           -> [DMap k Identity]
           -> DMap k []
gatherData _ [] =
  DMap.empty
gatherData mds (x : xs) =
  let
    md = gatherDatum mds x
    ds = gatherData mds xs
  in
    case md of
      Nothing -> ds
      Just d ->
        if DMap.null ds
        then DMap.map (pure . runIdentity) d
        else DMap.intersectionWithKey (\_ x -> (runIdentity x :)) d ds

class FromEntry k where
  type Entry k
  fromEntry :: Applicative f => Entry k -> DMap k f

loadColumns :: (GCompare k, FromEntry k)
            => DMap k ColumnMetadata
            -> [Entry k]
            -> DMap k (Column [])
loadColumns md =
  attachColumnMetaData md .
  gatherData md .
  fmap fromEntry
