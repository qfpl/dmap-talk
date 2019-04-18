{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Stuff.Widget where

import Control.Monad (join)
import Control.Monad.Fix (MonadFix)

import Data.Maybe (fromMaybe)

import Text.Read (readMaybe)

import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Monoid (Endo(..))

import Data.Dependent.Sum (DSum(..))
import Data.Dependent.Map (DMap, GCompare)
import qualified Data.Dependent.Map as DMap

import Reflex.Dom.Core
import Data.Functor.Misc

import qualified Data.Text as Text

import Control.Lens

import Data.Vessel
import Data.Validation

import Common.Stuff
import Common.Stuff.Validate

-- data K a where
--   K1 :: K (IdentityV Int)
--   K2 :: K (SingleV Bool)

-- newtype V f = V (Vessel K f)

newtype W e t m a =
  W {
    getWidget :: a Maybe
              -> Event t (a Maybe)
              -> Event t (a (Const [e]))
              -> m (Event t (Endo (a Maybe)))
  }

-- instance (Monad m, Reflex t, Semigroup (a Endo)) => Semigroup (W e t m a) where
--   W f1 <> W f2 = W $ \iv ev de -> do
--     e1 <- f1 iv ev de
--     e2 <- f2 iv ev de
--     pure $ e1 <> e2

-- instance (Monad m, Reflex t, Semigroup (a Endo)) => Monoid (W e t m a) where
--   mempty = W $ \_ _ _ -> pure never
--   mappend = (<>)

addValidation :: (Monad m, Reflex t, MonadHold t m, Monoid (a (Const [e])), MonadFix m)
              => Validator e a
              -> Event t b
              -> W e t m a
              -> W e t m a
addValidation v eValidate (W fn) = W $ \iv ev ee -> mdo
  e <- fn iv ev $ ee <> ee'

  -- this is delayed by a frame, which seems bad
  -- a foldDyn followed by a holdDyn will do that
  d <- foldDyn appEndo iv $ leftmost [e, (\x -> Endo (const x)) <$> ev]
  let (ee', _) = fanEither $ toEither . getValidator v <$> current d <@ eValidate

  pure e

keyWidget :: (GCompare k, Monad m, Reflex t, MonadHold t m, MonadFix m, Eq a, Eq e)
          => k a
          -> W e t m (Wrap a)
          -> W e t m (DMap k)
keyWidget k (W fn) = W $ \iv ev ee -> do
  let
    iv' = Wrap . join . DMap.lookup k $ iv
    ev' = Wrap . join . DMap.lookup k <$> ev
    ee' = Wrap . maybe (Const []) id . DMap.lookup k <$> ee
  e <- fn iv' ev' ee'
  pure $ Endo . (\fn -> DMap.adjust (unWrap . fn . Wrap) k) . appEndo <$> e

wrapWidget :: (Monad m, Reflex t)
           => (Maybe a -> Event t (Maybe a) -> Event t [e] -> m (Event t (Endo (Maybe a))))
           -> W e t m (Wrap a)
wrapWidget fn = W $ \iv ev ee -> do
  e <- fn (unWrap iv) (unWrap <$> ev) (getConst . unWrap <$> ee)
  pure $ Endo . (\fn -> Wrap . fn . unWrap) . appEndo <$> e

widgetYear :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m) => W Error t m (Wrap Years)
widgetYear = wrapWidget $ \iv ev ee -> do
  i <- el "div" $ inputElement $
    def & inputElementConfig_initialValue .~ (fromMaybe "" $ Text.pack . show <$> iv)
        & inputElementConfig_setValue .~ (fmap (Text.pack . show) . fmapMaybe id $ ev)
  el "div" $ do
    de <- holdDyn [] ee
    display de
  pure $ Endo . const . fmap Years . readMaybe . Text.unpack <$> _inputElement_input i

widgetBool :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m) => W Error t m (Wrap Bool)
widgetBool = wrapWidget $ \iv ev de -> do
  cb <- checkbox (fromMaybe False iv) $ def & setValue .~ (fmapMaybe id ev)
  pure $ Endo . const . Just <$> cb ^. checkbox_change

-- something to focus on subkeys would be nice
-- focusDMapWidget :: (Monad m, Reflex t) => Prism' (k1 a) (k2 a) -> W e t m (DMap k2) -> W e t m (DMap k1)
-- focusDMapWidget p (W fn) = W $ \dv de -> do
--   pure never

-- making this a monad stack, or at least using Reader, would be a big improvement
-- actually, making this a DomBuilder would be handy
widgetProgrammer :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => W Error t m (DMap AttendeeKey)
widgetProgrammer = W $ \iv ev de ->
  el "div" $ do
    text "Programmers"
    eL <- el "div" $ do
      text "Langauges"
      eH <- el "div" $ do
        text "Haskell"
        getWidget (keyWidget (AKProgrammer (PKLanguage Haskell)) widgetYear) iv ev de
      eS <- getWidget (keyWidget (AKProgrammer (PKLanguage Scala)) widgetYear) iv ev de
      eC <- getWidget (keyWidget (AKProgrammer (PKLanguage Clojure)) widgetYear) iv ev de
      pure $ eH <> eS <> eC
    eT <- el "div" $ do
      text "Technologies"
      eW <- getWidget (keyWidget (AKProgrammer (PKTechnology Web)) widgetBool) iv ev de
      eD <- getWidget (keyWidget (AKProgrammer (PKTechnology DB)) widgetBool) iv ev de
      eR <- getWidget (keyWidget (AKProgrammer (PKTechnology REST)) widgetBool) iv ev de
      eM <- getWidget (keyWidget (AKProgrammer (PKTechnology Mobile)) widgetBool) iv ev de
      pure $ eW <> eD <> eR <> eM
    pure $ eL <> eT

widgetManager :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => W Error t m (DMap AttendeeKey)
widgetManager = W $ \iv ev de ->
  el "div" $ do
    text "Managers"
    eR <- el "div" $ do
      text "Roles"
      eL <- getWidget (keyWidget (AKManager (MKRole TeamLead)) widgetYear) iv ev de
      eP1 <- getWidget (keyWidget (AKManager (MKRole ProjectManager)) widgetYear) iv ev de
      eP2 <- getWidget (keyWidget (AKManager (MKRole ProductManager)) widgetYear) iv ev de
      pure $ eL <> eP1 <> eP2
    eM <- el "div" $ do
      text "Methodologies"
      eS <- getWidget (keyWidget (AKManager (MKMethodology Scrum)) widgetBool) iv ev de
      eA <- getWidget (keyWidget (AKManager (MKMethodology Agile)) widgetBool) iv ev de
      eW <- getWidget (keyWidget (AKManager (MKMethodology Waterfall)) widgetBool) iv ev de
      pure $ eS <> eA <> eW
    pure $ eR <> eM

-- we can probably use prisms / isos to focus this down
-- might be able to get ix / at into play
widgetAttendee :: (Monad m, Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => W Error t m Attendee
widgetAttendee = W $ \iv ev de -> do
  let iv' = unAttendee iv
      ev' = unAttendee <$> ev
      de' = unAttendee <$> de
  eP <- getWidget widgetProgrammer iv' ev' de'
  eM <- getWidget widgetManager iv' ev' de'
  pure $ Endo . (\fn -> Attendee . fn . unAttendee) . appEndo <$> eP <> eM
