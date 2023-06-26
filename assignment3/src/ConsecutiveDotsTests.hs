{-|
Module      : Consecutive Dots Tests
Description : Tests for the Consecutive Dots game
Copyright   : (c) 2022 The Australian National University
License     : AllRightsReserved
-}
module ConsecutiveDotsTests where

import           ConsecutiveDots
import           Data.Aeson
import           Dragons.ConsecutiveDots      ()
import           Dragons.ConsecutiveDots.Text
import           Testing

consecutiveDotsTests :: Test
consecutiveDotsTests = TestGroup "ConsecutiveDots" [
     possibleConnectionsTest    
   , applyMoveTests
   , jsonTests
  ]

possibleConnectionsTest :: Test
possibleConnectionsTest = TestGroup 
  "Possible Connections gets the correct sets of points"
  [ Test "Vertical" $
  assert ([(0,0), (0,1), (0,2)] `elem` possibleConnections 3 (0,0))
  , Test "Horizontal" $
  assert (
    [(2,0),(1,0), (0,0), (-1,0), (-2,0)] `elem` possibleConnections 3 (0,0))
  , Test "Diagonal Down" $
  assert (
    [(2,2),(1,1), (0,0), (-1,-1), (-2,-2)] `elem` possibleConnections 3 (0,0))
  , Test "Diagonal Up" $
  assert (
    [(2,-2),(1,-1), (0,0), (-1,1), (-2,2)] `elem` possibleConnections 3 (0,0))]


applyMoveTests :: Test
applyMoveTests = TestGroup "applyMoves"
  [ Test "On Empty Board"
      (assertEqual (applyMove 0 (initialState 3 (3, 3))) 
      (Just $ State 3 (Turn Red) (3,3) [(2,[Blue]),(3,[]),(3,[])]))
  , Test "Fail on full column"
      (assertEqual (applyMove 0
       (State 3 (Turn Red) (3,3) [(0,[Blue, Red, Blue]), (3,[]),(3,[])]))
      Nothing)
  , Test "Win on Normal"
            (assertEqual (applyMove 2
       (State 3 (Turn Blue) (3,3) 
       [(0,[Blue, Red, Blue]), (0,[Red, Red, Blue]),(3,[])]))
      (Just 
      (State 3 (GameOver $ Winner Blue) (3,3) 
      [(0,[Blue, Red, Blue]), (0,[Red, Red, Blue]),(2,[Blue])])))
  , Test "Win on Wrap"
       (assertEqual (applyMove 2
       (State 3 (Turn Red) (3,3) 
       [(0,[Blue, Red, Blue]), (0,[Red, Red, Blue]),(3,[])]))
      (Just 
      (State 3 (GameOver $ Winner Red) (3,3) 
      [(0,[Blue, Red, Blue]), (0,[Red, Red, Blue]),(2,[Red])])))
  ]

jsonTests :: Test
jsonTests = TestGroup "JSON encode/decode"
  [ Test ("simple encode/decode of Move " ++ show mv)
      (assertEqual (decode (encode mv)) (Just mv))
      | mv <- 
        [1..10 :: Move] 
  ]

