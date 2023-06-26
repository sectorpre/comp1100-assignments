module Dragons.TestMain where

import AITests
import ConsecutiveDotsTests
import Testing

allTests :: Test
allTests = TestGroup "All Tests"
  [ consecutiveDotsTests
  , aiTests
  ]

testMain :: IO ()
testMain = runTests allTests
