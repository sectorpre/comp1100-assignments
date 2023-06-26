module FarmTest where

import Farm
import Testing
import TestPatterns

-- | The list of tests to run. When you define additional test cases,
-- you must list them here or they will not be checked. You should
-- remove these examples when submitting the assignment.
tests :: [Test]
tests =
  [ allLocTest
  , getTest1
  , getTest2
  , happyCowTest
  , monocultureTest
  , stableMixTest
  ]

-- allLoc function test.
allLocTest :: Test
allLocTest = Test "allLocations 3 2" (assertEqual (allLocations 3 2) ([Lot 'a' 0, Lot 'b' 0, Lot 'c' 0, Lot 'a' 1, Lot 'b' 1, Lot 'c' 1] :: [Location]))

-- get function test
getTest1 :: Test 
getTest1 = Test "get Lot a 0 stableMix" (assertEqual (get (Lot 'a' 0) (stableMix)) (Just(Paddock Livestock 500 3) :: Maybe Paddock))

getTest2 :: Test 
getTest2 = Test "get Lot a 0 stableMix" (assertEqual (get (Lot 'h' 0) (stableMix)) (Just(Paddock Livestock 500 3) :: Maybe Paddock))

-- predictIncome function tests. Ensures the overall calculations are correct
happyCowTest :: Test
happyCowTest = Test "happyCow 2022-2030" (assertEqual (round (predictIncome 8 happyCow)) (3504 :: Int))

monocultureTest :: Test
monocultureTest = Test "monoculture 2022-2050" (assertEqual (round (predictIncome 28 monoculture)) (97486 :: Int))

stableMixTest :: Test
stableMixTest = Test "stableMix 2022-2030" (assertEqual (round (predictIncome 8 stableMix)) (112911 :: Int))