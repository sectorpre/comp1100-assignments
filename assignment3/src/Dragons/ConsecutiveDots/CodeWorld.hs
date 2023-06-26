{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : Dragons.ConsecutiveDots.Codeworld
Description : CodeWorld interface for the Consecutive Dots game
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved
-}
module Dragons.ConsecutiveDots.CodeWorld where

import           ConsecutiveDots
import           CodeWorld
import           Data.List
import           Data.Text (Text)
import qualified Data.Text as T
import           Dragons.ConsecutiveDots (unwrapColumn)
import           Dragons.Game
import           Dragons.Game.UI.CodeWorld as UI

-- | Our UI-specific state.
data UIModel
  = Idle
    -- ^ Waiting for something interesting to happen (AI move, network
    -- move, etc.).
  | SelectColumn
    -- ^ Player needs to choose a column to drop their piece into
  deriving (Eq, Show)

-- | Most events CodeWorld sends to us are uninteresting, so we parse
-- down into this structure.
data SimpleEvent
  = Esc
  | ClickLocation Int
  deriving (Eq, Show)

codeWorldUI :: GameConfig GameState Move -> IO (GameUI GameState Move)
codeWorldUI config = UI.codeWorldUI config $ CodeWorldUI
  { cwInitialModel = Idle
  , cwView = view
  , cwUpdate = update
  }

-- | Render the whole scene as a 'Picture'.
view :: UIMode Move -> GameState -> UIModel -> Picture
view mode state model = pictures
  [ drawModeText mode
  , drawModelText model
  , drawBoard state
  ]

-- | Describe the 'UIMode', which is what the framework is currently
-- doing, or what it is asking of the user.
drawModeText :: UIMode Move -> Picture
drawModeText mode = translated (-6) 7 . scaled 0.5 0.5 . lettering $ case mode of
  Started -> "Initialising"
  AwaitingMove p _ _ -> pieceName p <> " to move"
  AIThinking p name -> pieceName p <> " (" <> T.pack name <> ") is thinking"
  AIFailedToMove p name -> pieceName p <> " (" <> T.pack name <> ") failed to move"
  AIIllegalMove p name -> pieceName p <> " (" <> T.pack name <> ") made an illegal move"
  NetworkIllegalMove p -> pieceName p <> " (network) made an illegal move"
  Finished o _ -> "Game over. " <> case o of
    Winner p -> pieceName p <> " wins!"
    Draw -> "It's a draw!"

-- | Additional labels from the 'UIModel', which tracks exactly what
-- we're asking of the player as he or she builds up a move.
drawModelText :: UIModel -> Picture
drawModelText model = translated (-6) 6 . scaled 0.5 0.5 $ case model of
  Idle-> blank
  SelectColumn -> lettering "Click a column to drop your tile."

-- | Draw the board and all its pieces to the centre of the screen.
drawBoard :: GameState -> Picture
drawBoard state = centreGrid state $ pictures
 (zipWith draw (concat (transpose board')) (allLocations state))
  & grid state & background
  where
    board' = map unwrapColumn (board state)

    background = coloured (dull green) 
      (foldMap 
      (\x -> 
        solidPolygon 
        [(x + 0.25,0.25),(x + 0.25,-h + 1),(x + 0.75,-h+1),(x + 0.75,0.25)])
        [0..w-2]
      & solidPolygon [(-0.5,0.25),(-0.5,-h + 1),(-0.25,-h+1),(-0.25,0.25)]
      & solidPolygon [(w-1+0.25,0.25),(w-1+0.25,-h + 1),(w-0.5,-h+1),(w-0.5,0.25)]
      & solidPolygon [(-0.5,-h+0.5),(-0.5,-h + 1.1),(w-0.5,-h+1.1),(w-0.5,-h+0.5)])
    
    (w, h) = (\(x,y) -> (fromIntegral x, fromIntegral y)) $ bounds state
    
    draw :: Maybe Player -> Location -> Picture
    draw mp (x, y) = translated rx (-ry) (drawSpace mp)
      where 
        rx = fromIntegral x
        ry = fromIntegral y

    
allLocations :: GameState -> [Location]
allLocations state = [ (x, y) | y <- [0..h-1], x <- [0..w-1]]
  where (w,h) = bounds state

-- | Draw a board square in a square 1.0 units each side.
drawSpace :: Maybe Player -> Picture
drawSpace p = case p of
    Just Red -> coloured red
     (solidCircle 0.25) & circle 0.25
    Just Blue -> coloured blue
     (solidCircle 0.25) & circle 0.25
    Nothing -> solidCircle 0.25

                 
-- | Labels for pieces that match how we draw them.
pieceName :: Player -> Text
pieceName = player "Blue" "Red"

-- | The grid that makes up the board.
grid :: GameState -> Picture
grid state
  = pictures (map (\x -> polyline [(x - 0.25,0.25),(x - 0.25,-h + 1)]) [0..w-1]
  ++ map (\x -> polyline [(x + 0.25,0.25),(x + 0.25,-h + 1)]) [0..w-1])
  where
    (w, h) = (\(x,y) -> (fromIntegral x, fromIntegral y)) $ bounds state

-- | Translate the grid into the centre of the screen.
centreGrid :: GameState -> Picture -> Picture
centreGrid state = scaled sf sf . translated ((-w'+1)/2) ((h'-1)/2)
  where
    sf = min (18/w') (10/h')
    (w, h) = bounds state
    (w', h') = (fromIntegral w, fromIntegral h)

-- | This is like the update function given to CodeWorld's
-- 'activityOf' function, but we take additional arguments for the
-- game state and 'UIMode', and return additional information to the
-- framework when we have a completed move.
update
  :: UIMode Move
  -> GameState
  -> UIModel
  -> Event
  -> (UIModel, Maybe (UIResponse Move))
update mode state model ev = case mode of
  Started -> idle
  Finished _ quit -> (Idle, Just quit)
  AIThinking _ _ -> idle
  AIFailedToMove _ _ -> idle
  AIIllegalMove _ _ -> idle
  NetworkIllegalMove _ -> idle
  AwaitingMove _ _ respond -> case model of
    Idle -> (SelectColumn, Nothing)
    SelectColumn ->
      withSimpleEvent $ \case
        ClickLocation x -> (Idle, Just $ respond x) 
        Esc -> idle

  where
    -- Parse CodeWorld event into SimpleEvent if possible.
    simpleEvent :: Maybe SimpleEvent
    simpleEvent = case ev of
      KeyPress "Esc" -> Just Esc
      PointerPress p -> ClickLocation <$> column state p
      _ -> Nothing

    -- If the current event parses to a SimpleEvent, feed it to the
    -- function. Otherwise do nothing.
    withSimpleEvent
      :: (SimpleEvent -> (UIModel, Maybe (UIResponse Move)))
      -> (UIModel, Maybe (UIResponse Move))
    withSimpleEvent f = maybe ignore f simpleEvent

    idle = (Idle, Nothing)
    ignore = (model, Nothing)

-- | Convert a 'Point' in screen space into a 'Location' on the game
-- board, if that makes sense.
column :: GameState -> Point -> Maybe Int
column state (px, _) = find onColumn [0..w-1]
  where
    (w, h) = bounds state
    (w', h') = (fromIntegral w - 1, fromIntegral h - 1)
    onColumn x = abs dx < 0.25
      where
        x' :: Double
        x' = (px * sf + w') / 2
        dx = fromIntegral x - x'
        
        sf = max (w' / 8) (h' / 4)