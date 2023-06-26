{-|
Module      : AITests
Description : Tests for your AI functions
Copyright   : (c) 2020 Your Name Here
License     : AllRightsReserved
-}
module AITests where

import           AI
import           ConsecutiveDots
import           Testing

aiTests :: Test
aiTests = TestGroup "AI"
  [miscFunctionsTest,
  roseTreeGenTests,
  roseTreeProcTests
  ]

miscFunctionsTest :: Test
miscFunctionsTest = TestGroup "misc functions testing"
  [Test "addElemsTest"
    (assertEqual 
    (addElems ([[1.0],[2.0],[3.0]]))
    ([1.0,2.0,3.0])),
  Test "getLocationsTest"
    (assertEqual 
    (getLocations (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])]) 
    (0)) 
    ([(0,2),(1,2),(2,2)])),
  Test "locCheckTest"
    (assertEqual (locCheck (0,0) 
    (State 3 (Turn Blue) (3,3) [(1,[Blue, Blue]),(2,[Red]),(2,[Red])]))
    (Just (State 3 (GameOver (Winner Blue)) (3,3) [(0,[Blue, Blue, Blue]),(2,[Red]),(2,[Red])]))
    ),
  Test "checkNumTest"
    (assertEqual (checkNum 
    (State 3 (Turn Blue) (3,3) [(1,[Blue, Blue]),(2,[Red]),(2,[Red])])) 
    (Just 0)),
  Test "quicksortTest"
    (assertEqual (quicksort
    ([1,2,1,1,5,3,3,4])) 
    ([5,4,3,3,2,1,1,1])),
  Test "valueFindTest"
    (assertEqual (valueFind [2.0,1.0,3.0,1.0] 0 (0,0))
    (2)
    )]

-- >>>>>>>>>>>>>><<<<<<<<<<<<<<<<
testState :: GameState
testState = State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])]

testState2 :: GameState
testState2 = State 3 (Turn Red) (3,3) [(2,[Red]),(3,[]),(3,[])]


roseTreeGenTests :: Test
roseTreeGenTests = TestGroup "rosetree generation testing" 
  [Test "rosetreeCalctest"
    (assertEqual 
    (rosetreeCalc (Blue) (RoseNode (testState, Just 0) []))
    (RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])],Just 0) 
      [
        RoseNode (State 3 (Turn Red) (3,3) [(2,[Blue]),(3,[]),(3,[])],Just 1) [],
        RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(2,[Blue]),(3,[])],Just 1) [],
        RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(3,[]),(2,[Blue])],Just 1) []
        ])),
  Test "roseMapTest"
    (assertEqual 
    (roseMap (rosetreeCalc Blue) (RoseNode (testState, Just 0) []))
    (RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])],Just 0) 
      [
        RoseNode (State 3 (Turn Red) (3,3) [(2,[Blue]),(3,[]),(3,[])],Just 1) [],
        RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(2,[Blue]),(3,[])],Just 1) [],
        RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(3,[]),(2,[Blue])],Just 1) []
        ])),
  Test "priorityFindTest"
    (assertEqual 
    (priorityFind 3 Red (0,1) testState2)
    ((State 3 (Turn Blue) (3,3) [(1,[Red, Red]),(3,[]),(3,[])]), Just 2)),
  Test "roseGenTest"
    (assertEqual 
    (roseGen Blue 2 (RoseNode (testState, Just 0) []))
    (RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])],Just 0) 
      [RoseNode (State 3 (Turn Red) (3,3) [(2,[Blue]),(3,[]),(3,[])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(1,[Red,Blue]),(3,[]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(2,[Blue]),(2,[Red]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(2,[Blue]),(3,[]),(2,[Red])],Just (-1)) []],
      RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(2,[Blue]),(3,[])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(2,[Red]),(2,[Blue]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(1,[Red,Blue]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(2,[Blue]),(2,[Red])],Just (-1)) []],
      RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(3,[]),(2,[Blue])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(2,[Red]),(3,[]),(2,[Blue])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(2,[Red]),(2,[Blue])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(1,[Red,Blue])],Just (-1)) []]]))
  ]

-- >>>>>><<<<<<<<<<

rosetreesample :: RoseTree (GameState, Maybe Int)
rosetreesample = 
  RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(3,[])],Just 0) 
      [RoseNode (State 3 (Turn Red) (3,3) [(2,[Blue]),(3,[]),(3,[])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(1,[Red,Blue]),(3,[]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(2,[Blue]),(2,[Red]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(2,[Blue]),(3,[]),(2,[Red])],Just (-1)) []],
      RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(2,[Blue]),(3,[])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(2,[Red]),(2,[Blue]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(1,[Red,Blue]),(3,[])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(2,[Blue]),(2,[Red])],Just (-1)) []],
      RoseNode (State 3 (Turn Red) (3,3) [(3,[]),(3,[]),(2,[Blue])],Just 1) 
        [RoseNode (State 3 (Turn Blue) (3,3) [(2,[Red]),(3,[]),(2,[Blue])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(2,[Red]),(2,[Blue])],Just (-1)) [],
        RoseNode (State 3 (Turn Blue) (3,3) [(3,[]),(3,[]),(1,[Red,Blue])],Just (-1)) []]]

roseTreeProcTests :: Test
roseTreeProcTests = TestGroup "roseTree processing functions tests" 
  [Test "worstNode test"
    (assertEqual 
    (worstNode ([
      RoseNode (testState, Nothing) [],
      RoseNode (testState, Just (-5)) [],
      RoseNode (testState, Just (-2)) [],
      RoseNode (testState, Nothing) [],
      RoseNode (testState, Just (-1)) []]) Nothing)
    (RoseNode (testState, Just (-5)) [])),
  Test "nodeRunner test"
    (assertEqual 
    (map (nodeRunner 0 0.0) (listRose (rosetreesample)))
    ([[0.0],[0.0],[0.0]]))
  ]
