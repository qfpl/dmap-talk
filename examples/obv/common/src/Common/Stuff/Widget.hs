{-|
Copyright   : (c) 2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
module Common.Stuff.Widget where

import Data.Functor.Const
import Data.Monoid (Endo(..))

import Reflex.Dom.Core

import Common.Stuff
import Common.Stuff.Validate

newtype W e t m a = W (Dynamic t (a Maybe) -> Dynamic t (a (Const [e])) -> m (Event t (a Endo)))

wrapWidget :: (Monad m, Reflex t)
           => (Dynamic t (Maybe a) -> Dynamic t [e] -> m (Event t (Endo a)))
           -> W e t m (Wrap a)
wrapWidget fn = W $ \dv de -> do
  e <- fn (unWrap <$> dv) (getConst . unWrap <$> de)
  pure $ Wrap <$> e

widgetYear :: (Monad m, Reflex t) => W Error t m (Wrap Years)
widgetYear = wrapWidget $ \dv de -> do
  pure never

widgetBool :: (Monad m, Reflex t) => W Error t m (Wrap Bool)
widgetBool = wrapWidget $ \dv de -> do
  pure never

widgetAttendee :: (Monad m, Reflex t) => W Error t m Attendee
widgetAttendee = W $ \dv de -> do
  pure never
