{-|
Module      : ConsecutiveDots
Description : The logic and rules of the Consecutive Dots board game
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved

This module contains the data types, rules, and logic for the
Consecutive Dots game. You should skim through the functions and look
at the type signatures.

You don't need to understand how the code in this file works
in order to complete the assignment, but it will be necessary
to understand what some of the functions in this file do
so that you can use them in your code in AI.hs. In particular
you should look out for functions that update the game state.

You are welcome to ask questions about the code in this
module in public posts on piazza.
-}
module ConsecutiveDots where

-- | The game state contains
--  1. How many in a row a player needs in order to win. 
--  2. Which player's turn it is.
--  3. The size of the board.
--  4. The current occupation of each piece on the board.
data GameState = State Int Turn (Int,Int) Board
  deriving (Eq, Show)

-- | It is either a player's turn, or the game is over.
data Turn = Turn Player | GameOver Outcome 
  deriving (Eq, Show)

data Player
  = Blue -- ^ Player 1
  | Red -- ^ Player 2
  deriving (Eq, Show)

data Outcome = Winner Player | Draw deriving (Eq, Show)

-- | A 'Board' always consists of 'gameWidth' columns, each of length
-- no greater than 'gameHeight'.
type Board = [Column]

-- | Each column is comprised of the number of empty spaces it contains,
-- followed by the list of player's tiles that have been placed in it.
-- The head of the list is the most recent (top) tile 
type Column = (Int, [Player])

type Location = (Int, Int)

type Move = Int

turn :: GameState -> Turn
turn (State _ trn _ _) = trn

-- | Given a function to update a Turn, update a whole GameState
adjustTurn :: (Turn -> Turn) -> GameState -> GameState
adjustTurn f (State n trn bnds brd) = State n (f trn) bnds brd

-- | Given a Turn, set Turn in a GameState to that value.
setTurn :: Turn -> GameState -> GameState
setTurn trn = adjustTurn (const trn)

changeTurn :: GameState -> GameState
changeTurn = adjustTurn (\t -> case t of
  Turn p -> Turn (otherPlayer p)
  _ -> t)

bounds :: GameState -> (Int, Int)
bounds (State _ _ bnds _) = bnds

board :: GameState -> Board
board (State _ _ _ brd) = brd

setBoard :: GameState -> Board -> GameState
setBoard (State n p b _) = State n p b 

-- | Given positive integers for width and height, re
initialState :: Int -> (Int, Int) -> GameState
initialState n (w,h) = State n (Turn Blue) (w, h) (replicate w (h, []))

otherPlayer :: Player -> Player
otherPlayer p = case p of
  Red -> Blue
  Blue -> Red

-- | Drop a tile of player p's into a given column,
-- if that column exists and there is room. 
dropTile :: Move -> GameState -> Maybe (GameState, Location)
dropTile col state@(State _ (Turn p) (w,_) brd)
  | col >= w = Nothing
  | otherwise = case splitAt col brd of
    (_, []) -> error "dropTile: no board"
    (_, (0,_):_) -> Nothing
    (beforeCols, (i,ps):afterCols) -> 
      Just (setBoard state (beforeCols ++ (i-1,p:ps):afterCols),
       (col, i - 1))
dropTile _ _ = Nothing

-- | Given a column and a row, both indexing from 0, return the 
-- player whose piece is at that location, if any piece is there at all.
-- Note that the column 'w-1' is connected to (followed by) column 0.
get :: GameState -> Location -> Maybe Player
get (State _ _ (w,h) brd) (col, row) = case brd !! (col `mod` w) of
    (r, ps)
      | r > row || h <= row || row < 0 -> Nothing
      | otherwise -> Just (ps !! (row - r))

-- | Promote an operation on 'Int's to an operation on Tuples.
op :: (Int -> Int -> Int) -> Location -> Location -> Location
op o (a,b) (c,d) = (a `o` c, b `o` d)

-- | Given a minimum number for a winning sequence, and a location where a
--  tile is to be dropped (of the current player) return the list of lists of
-- locations we must check to see whether this is a winning move.  
possibleConnections :: Int -> Location -> [[Location]]
possibleConnections n loc = step (+) (0,1) :
  zipWith (++)
  (map (reverse . drop 1 . step (flip (-))) neighbours)
  (map (step (+)) neighbours)
  where
    step :: (Int -> Int -> Int) -> Location -> [Location]
    step on by = take n (iterate (op on by) loc) 
    neighbours = [(-1, 1)
                 ,(-1, 0)
                 ,(-1, -1)]

-- | Given the location of the last move, check whether the game is over
-- and update the 'Turn' to reflect this.  
updateTurn :: GameState -> Location -> GameState
updateTurn state@(State _ (Turn p) _ _) loc
  | checkWin state loc =
    setTurn (GameOver (Winner p)) state
  | all (\(i,_) -> i == 0) (board state) = setTurn (GameOver Draw) state
  | otherwise = changeTurn state
updateTurn state _ = state

-- | Given a location, check if it forms part of a winning sequence.
checkWin :: GameState -> Location -> Bool
checkWin state@(State n _ _ _) loc = any 
  (consecutive n . map (get state)) (possibleConnections n loc)

-- | Find whether there are at least n consecutive non-Nothing
-- elements in a list
consecutive :: Eq a => Int -> [Maybe a] -> Bool
consecutive _ [] = False
consecutive n (x:xs) =
 length xs >= (n - 1) 
 && (not (null x) && all (== x) (take (n - 1) xs) ||
 consecutive n xs)  

-- | Apply a move if it is legal, otherwise Nothing.
applyMove :: Move -> GameState -> Maybe GameState
applyMove mv = fmap (uncurry updateTurn) . dropTile mv



