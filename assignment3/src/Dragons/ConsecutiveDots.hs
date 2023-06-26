{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : Dragons.ConsecutiveDots
Description : Consecutive Dots-specific things students don't need to see
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved

This module collects functions and instances for ConsecutiveDots-specific data
structures (so they don't belong in the generic game framework), but
are also implementation details that students don't need to concern
themselves with.
-}
module Dragons.ConsecutiveDots where

import AI
import ConsecutiveDots
import Data.Aeson
import Dragons.Game

toAITable :: [(String, AIFunc)] -> [(String, GenericAIFunc GameState Move)]
toAITable = (fmap . fmap) toGenericAIFunc
  where
    toGenericAIFunc :: AIFunc -> GenericAIFunc GameState Move
    toGenericAIFunc aiFunc st = case aiFunc of
      NoLookahead f -> [f st]
      WithLookahead f -> map (f st) [1..]

rules1100 :: Int -> Int -> Int -> GameRules GameState Move
rules1100 n h w = GameRules
  { gameInitialState = initialState n (w,h) 
  , gameGetTurn = turn
  , gameApplyMove = applyMove
  }

-- | Used in multiple places for viewing the board.
unwrapColumn :: Column -> [Maybe Player]
unwrapColumn (i, xs) = replicate i Nothing ++ map Just xs

-- How to turn move types to and from JSON. Best practice is
-- to define instances next to either the data type or the
-- typeclass. These are "orphan" instances, and normally poor
-- practice, but we don't want to have too much mysterious code in
-- files that students need to read. 

-- This semester, as moves are simply ints, these instances already exist.

instance ToJSON GameState where
  toJSON (State n t bnd brd) = object
    [ "connect" .= n
    , "turn" .= t
    , "bounds" .= bnd
    , "board" .= jsonBoard (map unwrap brd)
    ]
    where
      jsonBoard = String . (foldMap . foldMap) (\case
        Just Blue -> "1"
        Just Red -> "2"
        Nothing -> " ")
      unwrap (i, xs) = replicate i Nothing ++ map Just xs
