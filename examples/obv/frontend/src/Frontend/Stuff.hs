{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend.Stuff where

import Control.Monad.Fix (MonadFix)

import Data.Monoid (Endo(..))

import Data.Functor.Compose
import Data.Functor.Identity

import Reflex.Dom.Core

import Common.Stuff
import Common.Stuff.Validate
import Frontend.Stuff.Widget

-- map of people, ability to query by language using f ~ Proxy?

-- Attendee Maybe -> Validation e (Attendee Identity)
-- Manager Maybe -> Validation e (Manager Identity)

-- Dynamic t (Attendee (Const e)) -> Dynamic t (Attendee Maybe) -> Event t (Validation e (Attendee Endo))

stuff :: (DomBuilder t m, PostBuild t m, MonadHold t m, MonadFix m) => m ()
stuff =
  el "div" $ mdo
    text "Hi"
    e <- el "div" $
      getWidget (addValidation validateAttendee e widgetAttendee) initialAttendee never never
    d <- foldDyn appEndo initialAttendee e
    eValidate <- el "div" $
      button "Validate"
    el "div" $
      display d
    pure ()
