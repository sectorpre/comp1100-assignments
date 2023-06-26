{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : AI
Description : AIs for Conseuctive Dots
Copyright   : (c) 2022 ANU
License     : AllRightsReserved
-}
module AI where

import           ConsecutiveDots

-- | Type of AI functions you can choose to write.
data AIFunc
  = NoLookahead (GameState -> Move)
    -- ^ Simple AIs that do not need lookahead.
  | WithLookahead (GameState -> Int -> Move)
    -- ^ AIs that want to look ahead. The assignment framework will
    -- call the function over and over with increasing integer
    -- arguments @1, 2, 3, ...@ until your AI's time limit is up.

data RoseTree a = RoseNode a [RoseTree a]
  deriving (Eq, Show)

-- | The table of all AIs that your assignment provides. The AI named
-- "default" in this table is the one your tutor will dedicate most of
-- their attention to marking. DO NOT submit with firstColumn as "default".
ais :: [(String, AIFunc)]
ais = [("default", WithLookahead aiMinimax), ("greedy", NoLookahead aiGreedy)]

-- >>>>>> all AIs <<<<<<

-- | A very simple AI, which tries moving in each column until
-- it finds a legal move.
firstColumn :: GameState -> Move
firstColumn state = try 0
  where
    try n = case applyMove n state of
      Nothing -> try (n + 1)
      Just _ -> n

-- | Makes a move based on how many pieces in a row the Ai will have
aiGreedy :: GameState -> Move
aiGreedy state = case helper state of 
  Nothing -> error "aiGreedy could not detect a move. This should not happen!"
  Just move -> move

  where
    helper :: GameState -> Maybe Move
    helper state@(State n player (sizeX,sizeY) board) | n == 0 = Nothing
                                                      | otherwise = case checkNum state of
                                                        Nothing -> 
                                                          helper (State (n-1) player (sizeX,sizeY) board)
                                                        Just move -> Just move

-- | Minimax AI
aiMinimax :: GameState -> Int -> Move
aiMinimax state@(State _ (Turn player) _ _) num = 
  case priorityCheck (roseGen player (2*num) (RoseNode (state, Just 0) [])) of
    Just x -> x
    Nothing -> error "aiMinimax no move determined: this should not happen!"

-- >>>>> misc functions <<<<<<

-- takes a gamestate and checks whether a location will give the ai n pieces in a row
checkNum :: GameState -> Maybe Move
checkNum state = helper (getLocations state 0)
  where
    helper :: [Location] -> Maybe Move
    helper locations = case locations of 
      [] -> Nothing 
      ((x, y):[]) -> case locCheck (x,y) state of
          Nothing -> Nothing
          _ -> Just x
      ((x, y):xs) -> case locCheck (x,y) state of
          Nothing -> helper xs
          _ -> Just x

-- Checks whether a location is a winning location
locCheck :: Location -> GameState -> Maybe GameState
locCheck (x, _) state = case applyMove x state of
          Nothing -> Nothing
          Just newstate -> case newstate of 
            (State _ (GameOver _) _ _) -> Just newstate
            _ -> Nothing

-- from a gamestate obtains all possible locations
getLocations :: GameState -> Int -> [Location]
getLocations (State n player (sizeX,sizeY) board) current = case board of 
  [] -> []
  (x:[]) -> case x of 
    (spaces, _) -> [(current, (spaces - 1))]
  (x:xs) -> case x of  
    (spaces, _) -> [(current, (spaces - 1))] 
    ++ getLocations (State n player (sizeX,sizeY) xs) (current+1)

-- add all elements of a list into one list together
addElems :: [[Double]] -> [Double]
addElems list = case list of 
  [] -> []
  (x:[]) -> x
  (x:xs) -> x ++ (addElems xs) 
                        

--https://stackoverflow.com/questions/19082953/how-to-sort-a-list-in-haskell-in-command-line-ghci
--Sorts a list from greatest to smallest
quicksort :: Ord a => [a] -> [a]
quicksort []     = []
quicksort (p:xs) = (quicksort greater) ++ [p] ++ (quicksort lesser)
    where
        lesser  = filter (< p) xs
        greater = filter (>= p) xs

-- returns which index has the greatest value
valueFind :: [Double] -> Int -> (Int, Double)-> Int
valueFind list curr (gIndex,gValue) = case list of 
  [] -> error "valuefind error!"
  (x:[]) | x > gValue -> curr
         | otherwise -> gIndex
  (x:xs) | x > gValue -> valueFind xs (curr+1) (curr, x)   
         | otherwise -> valueFind xs (curr+1) (gIndex, gValue)

-- >>>>>>>>> Rosetree processing functions <<<<<<<< 

-- Determines an optimal move based on a rosetree by comparing all "routes"
-- This is the "main" function that processes all nodes in a rosetree
priorityCheck :: RoseTree (GameState, Maybe Int) -> Maybe Move
priorityCheck (RoseNode (_, _) nodes) = 
  Just (valueFind (map head (map quicksort (map (nodeRunner 0 0) nodes))) 0 (0, 0))

-- This function is for testing purposes
-- Simply returns all nodes within a node
listRose :: RoseTree (GameState, Maybe Int) -> [RoseTree (GameState, Maybe Int)]
listRose currNode@(RoseNode _ nodes) = nodes

-- returns the priorities of all "routes" within a node
nodeRunner :: Int -> Double -> RoseTree (GameState, Maybe Int) -> [Double]
-- when it reaches a positive node
nodeRunner _ _ (RoseNode (_ ,(Nothing)) _) = []
nodeRunner runTimes totalp (RoseNode (_ ,(Just p1)) nodes) = 

  -- calls worstNode to simulate enemy's move
  case (worstNode nodes Nothing) of
    (RoseNode (_, (Just p2)) []) -> 
      [totalp + 
      (fromIntegral (p1))*((0.9)^(runTimes^2)) + 
      (fromIntegral (p2))*((0.70)^(runTimes^2))]
    (RoseNode (_, (Just p2)) xs) ->  
      addElems (map (nodeRunner (runTimes+1) (totalp + 
      (fromIntegral (p1))*((0.9)^(runTimes^2)) + 
      (fromIntegral (p2))*((0.70)^(runTimes^2)))) xs)

-- returns the node with the lowest priority
-- Simulates the enemy's (player) best possible move
worstNode :: [RoseTree (GameState, Maybe Int)] -> Maybe (RoseTree (GameState, Maybe Int)) -> RoseTree (GameState, Maybe Int)
worstNode nodes Nothing = case nodes of
  [] -> 
    error "worstNode called on empty list"
  (currWorst@((RoseNode (_, Just _ ) _)):xs) -> 
    worstNode xs (Just currWorst)
  (((RoseNode (_, Nothing) _)):xs) -> 
    worstNode xs Nothing  
worstNode nodes (Just currWorst@((RoseNode (_, int2) _))) = case nodes of 
   ([]) -> currWorst
   ((node@(RoseNode (_, int1) _)):[]) | int1 == Nothing -> currWorst
                                      | int1 > int2 -> currWorst
                                      | otherwise -> node
   ((node@(RoseNode (_, int1) _)):xs) | int1 == Nothing ->  worstNode xs (Just currWorst)
                                      | int1 > int2 -> worstNode xs (Just currWorst)
                                      | otherwise -> worstNode xs (Just node)  

-- >>>>>>> Rosetree generation functions <<<<<<

-- Generates a rosetree based on the lookahead
-- This is the "main" function used to generate a rosetree
roseGen :: Player -> Int -> RoseTree (GameState, Maybe Int) -> RoseTree (GameState, Maybe Int)
roseGen player num node | num > 0 = 
  roseGen player (num-1) (roseMap (rosetreeCalc player) (node))
                        | otherwise = node 

-- Determines the priority of a location 
-- This is called on each node to determine its priority
priorityFind :: Int -> Player -> Location-> GameState -> (GameState, Maybe Int)
priorityFind maxRow startPlayer location state@(State n k l p)| n == 0 = (state, Nothing)
                                                              | otherwise = case locCheck location state of
                                                                Nothing -> 
                                                                  priorityFind maxRow startPlayer location (State (n-1) k l p)
                                                                Just (State priority newPlayer (sizeX,sizeY) b) -> case newPlayer of
                                                                  GameOver (Winner x) | x == startPlayer -> 
                                                                    ((State maxRow (Turn (otherPlayer x)) (sizeX,sizeY) b), Just (priority))
                                                                                      | x /= startPlayer -> 
                                                                    ((State maxRow (Turn (otherPlayer x)) (sizeX,sizeY) b), Just (-(priority)))
                                                                  GameOver Draw -> (state, Just 0)
                                                                              
-- Takes a function and applies it to every node with an empty list
roseMap :: (RoseTree (GameState, Maybe Int) -> RoseTree (GameState, Maybe Int)) -> RoseTree (GameState, Maybe Int) -> RoseTree (GameState, Maybe Int)
roseMap f rt = case rt of
    (RoseNode x []) -> f (RoseNode (x) [])
    (RoseNode x rt) -> RoseNode (x) (helper f rt)

    where
        helper :: (RoseTree (GameState, Maybe Int) -> RoseTree (GameState, Maybe Int)) -> [RoseTree (GameState, Maybe Int)] -> [RoseTree (GameState, Maybe Int)]
        helper f rts = case rts of 
            [] -> error "roseMap not applying on empty list" 
            (x:[]) -> [roseMap f x]
            (x:xs) -> [roseMap f x] ++ helper f xs

-- Generates the next "layers" for a node
rosetreeCalc :: Player -> RoseTree (GameState, Maybe Int) -> RoseTree (GameState, Maybe Int)
rosetreeCalc _ (RoseNode (baseState, Nothing) []) = (RoseNode (baseState, Nothing) [])
rosetreeCalc startPlayer node@(RoseNode (baseState, _) []) = 
  helper startPlayer node (getLocations baseState 0)
  where
    -- Generates the priorities for each particular location and stores it in the rosetree
    helper :: Player -> RoseTree (GameState, Maybe Int) -> [Location] -> RoseTree (GameState, Maybe Int)
    helper player currNode@(RoseNode (state@(State n _ _ _), priority) nodes) locations = case locations of
      ([]) -> currNode
      ((x, y):[]) -> 
        RoseNode (state, priority) (nodes ++ 
        [(RoseNode (priorityFind n player (x,y) state) [])])
      ((x, y):xs) -> 
        helper player (RoseNode (state, priority) (nodes ++ 
        [(RoseNode (priorityFind n player (x,y) state) [])])) xs








