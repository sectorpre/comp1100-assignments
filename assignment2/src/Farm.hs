module Farm where
import CodeWorld

-- | A 'Farm' has an 'Int describing its width,
-- an 'Int' describing its height, and a list of its 'Paddock's
-- in row-major order. 
data Farm = Farm Int Int [Paddock] 
    deriving (Show)

-- | Each 'Paddock' holds a 'Crop' and two 'Double's - its yield in the
-- current year, and its degradation rate.
data Paddock = Paddock Crop Double Double
    deriving (Show, Eq)

data Crop = Livestock | Wheat | Conservation | Intercropping
    deriving (Show, Eq)

-- Characters must be between A and Z inclusive, where A is 1
data Location = Lot Char Int
    deriving (Show , Eq)

-- Takes a Crop datatype and returns its respective Paddock
allocatePaddock :: Crop -> Paddock
allocatePaddock crop = case crop of 
    Livestock     -> Paddock Livestock 500 3
    Wheat         -> Paddock Wheat 1000 10
    Conservation  -> Paddock Conservation 0 (-10) 
    Intercropping -> Paddock Intercropping 750 5

-- Takes a Crop and returns the next Crop on a list
cycleCrop :: Crop -> Crop
cycleCrop crop = case crop of 
    Livestock -> Wheat
    Wheat -> Conservation
    Conservation -> Intercropping
    Intercropping -> Livestock

-- Function for displaying Paddocks on the screen
renderPaddock :: Paddock -> Picture
renderPaddock paddock = case paddock of 
    Paddock Livestock yield _     -> 
        coloured (RGBA 0 255 0 ((yield/30000)*20)) (solidRectangle 1 1)
    Paddock Wheat yield _         -> 
        coloured (RGBA 0 0 255 ((yield/30000)*20)) (solidRectangle 1 1)
    Paddock Conservation _ _  -> 
        coloured (white) (solidRectangle 1 1)
    Paddock Intercropping yield _ -> 
        coloured (RGBA 255 0 0 ((yield/30000)*20)) (solidRectangle 1 1)

-- Takes a farm and "steps" it forward one year into the simulation
updateFarm :: Farm -> Farm
updateFarm farm = case farm of 
    Farm width height paddocks -> Farm width height (helper 0 paddocks farm)
    where
        -- Function that calculates a new yield and degredation rate of each of the paddocks in a list
        helper :: Int -> [Paddock] -> Farm -> [Paddock]
        helper position paddocksLeft f = case paddocksLeft of 
            [] -> []
            (y:[]) -> case y of 
                Paddock crop yield degr -> 
                    [Paddock crop 
                    (yield*(1-degr/100)) 
                    (degr/2 + avgNeighbours 0 (allNeighbour f position 0)/2)]
            (y:ys) -> case y of
                Paddock crop yield degr -> 
                    [Paddock crop 
                    (yield*(1-degr/100)) 
                    (degr/2 + avgNeighbours 0 (allNeighbour f position 0)/2)] ++
                     helper (position+1) ys f

-- Returns the average degredation rate of a list of degredation rates
avgNeighbours :: Double -> [Double] -> Double
avgNeighbours num list| num == 0 = case list of
                        [] -> 0
                        (x:[]) -> x*(1/((fromIntegral (length list))))
                        (x:xs) -> x*(1/((fromIntegral (length list)))) + avgNeighbours (fromIntegral (length list)) xs 
                      | otherwise = case list of
                        [] -> 0
                        (x:[]) -> x*(1/(num))
                        (x:xs) -> x*(1/(num)) + avgNeighbours num xs

-- Returns the degradation rates of all neighbours of a Paddock.
allNeighbour:: Farm -> Int -> Int -> [Double]
allNeighbour (Farm width height paddocks) position current 
                                                    -- check for neighbour on left
                                                    | current == 0 && (rem position width) /= 0 = 
                                                        [(padToDegr (get (Lot (['a'..'z']!!(rem (position-1) width)) 
                                                        (quot position width)) 
                                                        (Farm width height paddocks)))] ++ 
                                                        allNeighbour (Farm width height paddocks) position (current+1)
                                                    -- check for neighbour on right
                                                    | current == 1 && (rem (position+1) width) /= 0 = 
                                                        [(padToDegr (get (Lot (['a'..'z']!!(rem (position+1) width)) 
                                                        (quot position width)) 
                                                        (Farm width height paddocks)))] ++ 
                                                        allNeighbour (Farm width height paddocks) position (current+1)
                                                    -- check for neighbour on up
                                                    | current == 2 && (quot position width) /= 0 = 
                                                        [(padToDegr (get (Lot (['a'..'z']!!(rem position width)) 
                                                        ((quot position width)-1)) 
                                                        (Farm width height paddocks)))] ++ 
                                                        allNeighbour (Farm width height paddocks) position (current+1)
                                                    -- check for neighbour on down
                                                    | current == 3 && (quot position width) /= (height-1) = 
                                                        [(padToDegr (get (Lot (['a'..'z']!!(rem position width)) 
                                                        ((quot position width)+1)) 
                                                        (Farm width height paddocks)))] 
                                                    -- end of check
                                                    | current == 3  = []
                                                    -- skip to next value of current
                                                    | otherwise = allNeighbour (Farm width height paddocks) position (current+1)

                                                    where
                                                        padToDegr :: Maybe Paddock -> Double
                                                        padToDegr x = case x of
                                                            Just (Paddock _ _ degradation) -> degradation
                                                            Nothing -> 0

-- Obtains a Paddock from a Farm                              
get :: Location -> Farm -> Maybe Paddock
get (Lot xPos yPos) (Farm width height paddocks) 
    | (((fromEnum xPos) - 97) < width && yPos < height) = 
        helper ((fromEnum xPos) - 97) (yPos*width) paddocks
    | otherwise = Nothing
    where 
        helper :: Int -> Int -> [Paddock] -> Maybe Paddock
        helper x y list = case (x,y,list) of 
            (_,_,[]) -> Nothing
            (0,0,c) -> case c of 
                [] -> Nothing
                (p:_) -> Just p
            (0,yLeft,c) -> case c of 
                [] -> Nothing
                (_:ps) -> helper 0 (yLeft-1) (ps)
            (xLeft,yLeft,c) -> case c of 
                [] -> Nothing
                (_:ps) -> helper (xLeft-1) yLeft (ps)


-- takes a width and height and returns a list of all possible Locations
allLocations :: Int -> Int -> [Location]
allLocations w h = helper w h 0
        where
            helper :: Int -> Int -> Int -> [Location]
            helper width height position | position == ((width * height) - 1) = 
                                            [Lot (['a'..'z']!!(rem position width)) (quot position width)] 
                                         | rem (position-1) width == 0 = 
                                            [Lot (['a'..'z']!!(rem position width)) (quot position width)] ++ 
                                         helper width height (position + 1)  
                                         | otherwise =  [Lot (['a'..'z']!!(rem position width)) (quot position width)] ++
                                         helper width height (position + 1)  


-- predicts the income of a farm across a number of years.
predictIncome :: Int -> Farm -> Double
predictIncome int farm | int > 0 = 
    (0.93^(int-1))*(farmtotal (repeatUpdate (int-1) farm)) + predictIncome (int-1) farm
                       | otherwise = 0
    where 
        -- Updates a farm a number of times
        repeatUpdate :: Int -> Farm -> Farm 
        repeatUpdate left f | left > 0 = (repeatUpdate (left-1) (updateFarm f))
                            | otherwise = f

        -- Adds together all the yields of all the paddocks
        farmtotal :: Farm -> Double
        farmtotal f = case f of
            Farm _ _ [] -> 0
            Farm _ _ (y:[]) -> case y of 
                Paddock _ yield _ -> yield
            Farm width height (y:ys) -> case y of
                Paddock _ yield _ -> yield + farmtotal (Farm width height (ys))
                