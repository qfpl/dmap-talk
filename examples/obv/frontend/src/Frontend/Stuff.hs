{-# LANGUAGE OverloadedStrings #-}
module Frontend.Stuff where

import Data.Functor.Compose
import Data.Functor.Identity

import Reflex.Dom.Core

import Common.Stuff

-- map of people, ability to query by language using f ~ Proxy?

-- Attendee Maybe -> Validation e (Attendee Identity)
-- Manager Maybe -> Validation e (Manager Identity)

-- Dynamic t (Attendee (Const e)) -> Dynamic t (Attendee Maybe) -> Event t (Validation e (Attendee Endo))

stuff :: (DomBuilder t m, Monad m) => m ()
stuff =
  el "div" $ text "Hi"
