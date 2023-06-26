module TestPatterns where

import Farm

-- | This pattern should experience infinite growth.
happyCow :: Farm
happyCow = parseFarm toCrop 3 3 paddocks where
  paddocks = concat
    [ "   "
    , " c "
    , "   "
    ]


-- | This pattern will quickly decay to produce nothing.
monoculture :: Farm
monoculture = parseFarm toCrop 4 4 paddocks where
  paddocks = concat
    [ "wwww"
    , "wwww"
    , "wwww"
    , "wwww"
    ]


-- | This pattern should reach an equilibrium.
stableMix :: Farm
stableMix = parseFarm toCrop 8 6 paddocks where
  paddocks = concat
    [ "c      c"
    , " wciwci "
    , " iwciwc "
    , " ciwciw "
    , " wciwci "
    , "c      c"
    ]

-- | A test pattern of your own.
myFarm :: Farm
myFarm = parseFarm toCrop 3 4 paddocks where
  paddocks = concat
    [ "c  "
    , " wc"
    , " ii"
    , " cc"
    ]

-- | Given a way to parse a character, and expected bounds of the
-- Farm, parse a string describing paddocks into a Farm.
parseFarm :: (Char -> Crop) -> Int -> Int -> String -> Farm
parseFarm f w h paddocks
  | length paddocks == w * h = Farm w h (map (allocatePaddock . f) paddocks)
  | otherwise = error "parseFarm: dimensions don't match"

-- Converts a character to a crop
toCrop :: Char -> Crop
toCrop char = case char of 
  'c' -> Livestock
  'w' -> Wheat
  'i' -> Intercropping
  otherwise -> Conservation
