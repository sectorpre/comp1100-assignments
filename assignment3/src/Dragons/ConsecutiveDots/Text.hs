{-|
Module      : Dragons.ConsecutiveDots.Text
Description : Text interface for the Consecutive Dots game
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved
-}
module Dragons.ConsecutiveDots.Text where

import ConsecutiveDots
import Data.Maybe
import Data.List
import Text.Read
import Dragons.ConsecutiveDots (unwrapColumn)
import Dragons.Game
import Dragons.Game.UI.Text as UI

textUI :: GameConfig GameState Move -> GameUI GameState Move
textUI config = UI.textUI config $ TextUI
  { textRenderState = renderState
  , textReadMove = readMove
  }

renderState :: GameState -> String
renderState state = unlines $ catMaybes
  [ renderTurn (turn state)
  , Just $ renderBoard state
  ]

renderTurn :: Turn -> Maybe String
renderTurn t = case t of
  Turn Blue -> Just "Blue (O) to move"
  Turn Red -> Just "Red (X) to move"
  GameOver _ -> Nothing

renderBoard :: GameState -> String
renderBoard state = unlines . transpose $ intersperse walls cols
  where
    cols = zipWith (:) ['0'..'9'] (map (map renderSquare . unwrapColumn) brd)

    brd = board state 
    (_, h) = bounds state

    walls = replicate (h + 1) '|'

    renderSquare :: Maybe Player -> Char
    renderSquare sq = case sq of
      Nothing -> ' '
      Just Blue -> 'o'
      Just Red -> 'x'
     
-- | Ask for a move, check that it's valid.
readMove :: GameState -> Maybe (Player, Move) -> IO Move
readMove state _ = loop
  where
    loop = do
      putStrLn $ "Enter a column. Examples: " ++ intercalate ", " (map renderMove [0..3])
      line <- getLine
      case parseMove line of
        Nothing -> errLoop "Malformed move, try again."
        Just mv -> case applyMove mv state of
          Just _ -> pure mv
          _ ->  errLoop "Not a valid move."

    errLoop s = putStrLn s *> loop

-- | Parse a 'String' that should describe a 'Move', if it makes sense
-- for where we are in the current game.
parseMove :: String -> Maybe Move
parseMove = readMaybe 

renderMove :: Move -> String
renderMove = show
