{-# LANGUAGE OverloadedStrings #-}

module App where

import Farm
import CodeWorld
import Data.List (splitAt)
import Data.Text (pack)
import Data.Char (chr)
import GridRenderer
import TestPatterns

appMain :: IO ()
appMain = activityOf (initial monoculture) handleEvent render

-- | The 'Bool' is whether we are in the Debug mode.
-- The first 'Int' is the number of generations evolved. The second
-- is how far to jump when pressing space.
data Model = Model Bool Int Int Farm

-- | The model at the start of a simulation.
initial :: Farm -> Model
initial = Model False 2022 1

-- | Events we'd like our program to handle. Returned by 'parseEvent'.
data AppEvent
  = ChangeCrop Location
    -- ^ Change a paddock in the farm.
  | LoadTestPattern TestPattern
    -- ^ Replace the farm with one of the test patterns.
  | ToInitial
    -- ^ Revert to initial state
  | Step
    -- ^ Update the farm by one step.
  | IncreaseYear
  | DecreaseYear
  | ToggleDebug

data TestPattern = One | Two | Three | Four

handleEvent :: Event -> Model -> Model
handleEvent ev m = case parseEvent m ev of
  Nothing -> m
  Just appEv -> applyEvent appEv m

-- | CodeWorld has many events and we are not interested in most of
-- them. Parse them to our app-specific event type.
--
-- Further reading, if interested: "Parse, don't validate"
-- https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/
parseEvent :: Model -> Event -> Maybe AppEvent
parseEvent (Model _ _ _ farm) ev = case ev of
  KeyPress k
    | k == "1" -> Just (LoadTestPattern One)
    | k == "2" -> Just (LoadTestPattern Two)
    | k == "3" -> Just (LoadTestPattern Three)
    | k == "4" -> Just (LoadTestPattern Four)
    | k == "Q" -> Just ToInitial
    | k == "D" -> Just ToggleDebug
    | k == "." || k == "spacebar" -> Just Step
    | k == "=" -> Just IncreaseYear
    | k == "-" -> Just DecreaseYear
    | otherwise -> Nothing
  PointerPress p -> case getLocation p of
    Nothing -> Nothing
    Just coord -> Just (ChangeCrop coord) 
  _ -> Nothing

  where
    getLocation p = case fromPoint farm p of 
      Just (x,y) -> Just $ Lot (chr (x + fromEnum 'a')) y
      Nothing -> Nothing

-- | Here we apply the events we care about.
applyEvent :: AppEvent -> Model -> Model
applyEvent ev (Model b n steps farm) = case ev of
  ToInitial -> initial monoculture
  ChangeCrop paddock -> Model b n steps (at paddock reallocatePaddock farm)
    where
      reallocatePaddock (Paddock c _ _) = allocatePaddock (cycleCrop c)
  LoadTestPattern pat -> initial farm'
    where
      farm' = case (pat, farm) of
        (One, _)   -> happyCow
        (Two, _)   -> monoculture
        (Three, _) -> stableMix
        (Four, _) -> myFarm
  Step -> Model b (n + 1) steps (updateFarm farm)
  IncreaseYear -> Model b n (steps + 1) farm
  DecreaseYear -> Model b n (max 1 (steps - 1)) farm
  ToggleDebug -> Model (not b) n steps farm

render :: Model -> Picture
render (Model b year steps farm)
  = translated (-14) 9 (write ("Current Year: " ++ show year))
  & renderFarm b renderPaddock farm
  & translated (-9) (-9) (if b then write ("Predicted farm value from " 
    ++ show year 
    ++ " through " 
    ++ show (year + steps) 
    ++ ": $" 
    ++ (show . (round :: Double -> Int)) (predictIncome steps farm))
    else blank)
  where 
    write = dilated 0.5 . lettering . pack 

-- | Apply a function to a certain Paddock inside a Farm, and return a
-- new Farm where that Paddock has been replaced with the result of the
-- function call.
at :: Location -> (Paddock -> Paddock) -> Farm -> Farm
at p@(Lot a n) f farm@(Farm w h cells) = case get p farm of
  Nothing -> farm
  Just c -> Farm w h cells' where
    cells' = beforeCells ++ f c:afterCells
    (beforeCells, _:afterCells) = splitAt (w * n + m) cells
    m = fromEnum a - fromEnum 'a'

-- | Replace a Paddock within a Farm.
setAt :: Location -> Paddock -> Farm -> Farm
setAt p c = at p (const c)
