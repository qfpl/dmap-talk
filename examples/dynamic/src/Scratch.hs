{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeApplications #-}
module Scratch where

import Control.Error

import Control.Monad.Primitive

import Data.Unique.Tag

import Data.Dependent.Sum (ShowTag(..))
import Data.Dependent.Map (DMap(..))
import qualified Data.Dependent.Map as DMap

import Data.Constraint.Extras

data Action = Add String String | Print | Delete Int | Quit

parse :: IO Action
parse = do
  s <- getLine
  let
    ma = case words s of
      ["a", ty, v] ->
        Just $ Add ty v
      ["p"] ->
        Just Print
      ["d", i] ->
        Delete <$> readMay i
      ["q"] ->
        Just Quit
      _ ->
        Nothing
  case ma of
    Just a ->
      pure a
    Nothing -> do
      putStrLn "Invalid command"
      parse

type M = DMap (Tag (PrimState IO)) ((,) Int)

-- need the keys to be finitely enumerated if we're going to use constraints-extras
-- going to need the FRP example here

-- instance ArgDict (Tag RealWorld) where
--   type ConstraintsFor (Tag RealWorld) c = (c _)
--   argDict = Dict

instance ShowTag (Tag RealWorld) ((,) Int) where
  showTaggedPrec t n _ = showsPrec n t

showMe :: Has Show (Tag RealWorld) => M -> String
showMe = unlines . DMap.foldrWithKey (\k v x -> has @Show k (show v) : x) []

action :: Has Show (Tag RealWorld) => Action -> Int -> M -> IO ()
action (Add "i" s) i m = do
  tag <- newTag
  let mv = readMay s :: Maybe Int
  case mv of
    Nothing -> putStrLn "Not an Int" >> loop i m
    Just v -> loop (succ i) $ DMap.insert tag (i, v) m
action (Add "b" s) i m = do
  tag <- newTag
  let mv = readMay s :: Maybe Bool
  case mv of
    Nothing -> putStrLn "Not a Bool" >> loop i m
    Just v -> loop (succ i) $ DMap.insert tag (i, v) m
action (Add ty _) i m = do
  putStrLn $ ty ++ " is not a type we support"
  loop i m
action Print i m = do
  putStrLn $ showMe m
  loop i m
action (Delete d) i m =
  loop i $ DMap.mapMaybeWithKey (\_ (j, v) -> if j == d then Nothing else Just (j, v)) m
action Quit _ _ =
  pure ()

loop :: Has Show (Tag RealWorld) => Int -> M -> IO ()
loop i m = do
  a <- parse
  action a i m

go :: Has Show (Tag RealWorld) => IO ()
go = loop 0 DMap.empty
