{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
module Diagrams.GG.Render where

import Data.Maybe (fromMaybe)

import qualified Data.Text as Text

import qualified Control.Foldl as L
import Data.Constraint.Extras

import Data.Dependent.Map (DMap, Some(..), GCompare)
import qualified Data.Dependent.Map as DMap

import Diagrams.Prelude hiding (has)
import Diagrams.Backend.SVG
import Graphics.SVGFonts

import Diagrams.GG.Types

renderMe :: FilePath -> Diagram B -> IO ()
renderMe fp = renderSVG fp (mkWidth 500)

go :: (GCompare k, Has Ord k)
   => Layer k []
   -> Diagram B
go l =
  let
    stats = gatherFrameState . _lData $ l
    d = DMap.map _cData . _lData $ l
    md = DMap.map _cMetadata . _lData $ l
  in
    -- TODO pull this out into something which grabs origin and range if available,
    -- otherwise gives decent defaults
    fromMaybe (circle 1) $ case _lMapping l of
      Mapping kx ky -> do
        xName <- fmap _cmName $ DMap.lookup kx md
        yName <- fmap _cmName $ DMap.lookup ky md
        xs <- fmap (fmap asDouble) $ DMap.lookup kx d
        xStats <- fmap (fmap asDouble) $ DMap.lookup kx stats
        x0 <- _csMin xStats
        xRange <- csRange xStats
        let xs' = fmap (subtract x0) xs
        ys <- fmap (fmap asDouble) $ DMap.lookup ky d
        yStats <- fmap (fmap asDouble) $ DMap.lookup ky stats
        y0 <- _csMin yStats
        yRange <- csRange yStats
        let ys' = fmap (subtract y0) ys
        let
        -- radius of circle should be 1% of max xRange yRange? or min? the 1% probably should come from somewhere
          points = zipWith (\x y -> circle 1.0 # scale 0.1 # translate (r2 (x, y)) # showOrigin) xs' ys'
          bounds = square 1.0 # translate (r2 (0.5, 0.5)) # scaleX xRange # scaleY yRange # showOrigin
          xAxis = stroke (textSVG' def (Text.unpack xName)) # translateX (xRange / 2.0) # showOrigin
          yAxis = stroke (textSVG' def (Text.unpack yName)) # rotateBy 0.25 # translateY (yRange / 2.0) # showOrigin
        pure $
          (yAxis ||| ((mconcat points <> bounds) === xAxis ))
