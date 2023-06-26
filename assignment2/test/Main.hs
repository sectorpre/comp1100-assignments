module Main where

import FarmTest
import Testing

-- | A Haskell program starts by running the computation defined by
-- 'main'. We run the tests defined in `AutomataTest`.
main :: IO ()
main = runTests tests
