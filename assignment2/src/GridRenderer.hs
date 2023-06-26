{-# LANGUAGE ScopedTypeVariables #-}

{- |

Module      : GridRenderer
Copyright   : (c) 2022, The Australian National University

Functions to render a 'Grid' (Farm) to a CodeWorld 'Picture', and to convert
coordinates from CodeWorld screen space to and from grid space.

We need to represent transformations two ways:

* As a 3x3 matrix, to convert clicks on the screen into coordinates on
  the grid, and
  (https://en.wikipedia.org/wiki/Transformation_matrix#Affine_transformations)

* As a call to CodeWorld functions, to transform the rendered
  'Picture' of cells into something that will fit on the screen.

-}
module GridRenderer where

import           Farm
import           CodeWorld (Picture, Point)
import qualified CodeWorld as CW
import           Data.Text (pack)

-- | A type to represent transformations, which we will interpret into
--  matrices and CodeWorld calls.
--
-- NOTE: CodeWorld has no notion of reflection in its drawing API, and
-- its Y-axis goes up. We want to have the top row of the grid be the
-- top row drawn on the screen, so we will have to manually flip the
-- drawing and coordinate resolution calls independently.
data Transform
  = Translate Double Double
  | Scale Double
  deriving (Eq, Show)

-- | Like 'foldr', but for 'Transform'.
--
-- NB: transform Translate Scale = id (over Transforms)
--   , just like    foldr (:) [] = id (over lists)
transform :: (Double -> Double -> a) -> (Double -> a) -> Transform -> a
transform f _ (Translate x y) = f x y
transform _ g (Scale s) = g s

-- | Convert each 'Transform' into matrix form, then multiply them together.
toMatrix :: [Transform] -> M33 Double
toMatrix = foldr ((!*!) . transform translate scale) identity where
  translate x y = V3
    (V3 1 0 x)
    (V3 0 1 y)
    (V3 0 0 1)

  scale s = V3
    (V3 s 0 0)
    (V3 0 s 0)
    (V3 0 0 1)

-- | Convert each 'Transform' into a function over 'Picture's, then
-- compose them all.
toCodeWorld :: [Transform] -> Picture -> Picture
toCodeWorld = foldr ((.) . transform CW.translated (\s -> CW.scaled s s)) id

-- | Turn a 'Point' in CodeWorld screen space into an (x,y) coordinate.
fromPoint :: Farm -> Point -> Maybe (Int, Int)
fromPoint g@(Farm w h _) (x, y)
  | x' >= 0 && x' < w' && y' >= 0 && y' < h' = Just (x', y')
  | otherwise = Nothing
  where
    (w', h') = (fromIntegral w, fromIntegral h)
    (x', y') = (round invX, -(round invY))

    -- Compute the transformation matrix to send grid coordinates to
    -- the screen, then invert it to convert coordinates the other
    -- way.
    V3 invX invY _ = inv33 (toMatrix (gridTransforms g)) !* V3 x y 1

-- | Compute the transformations to fit the grid into the viewport.
gridTransforms :: Farm -> [Transform]
gridTransforms g =
  [ Scale (scaleFactor g) -- Shrink the grid to fit in the viewport
  , toCenter g -- Centre the grid around the origin
  ]

-- | Given a way to render one paddock, render the whole farm.
renderFarm :: Bool -> (Paddock -> Picture) -> Farm -> Picture
renderFarm b f g@(Farm w h ps) = toCodeWorld
  (gridTransforms g)
  (CW.pictures (grid' ++ xaxis ++ yaxis ++ grid))
  where
    grid = zipWith (draw f) ps (allLocations w h)
    grid' = if b then zipWith (draw info) ps (allLocations w h) else []
    xs = take w [Lot x (-1) | x <- ['a'..]]
    ys = take h [Lot '`' y | y <- [0..]]
    xaxis = zipWith (draw (\(Lot c _) -> drawAsLabel (:[]) c)) xs xs
    yaxis =  zipWith (draw (\(Lot _ i) -> drawAsLabel show i)) ys ys
    info = CW.dilated 0.2 . drawAsLabel 
      (\(Paddock _ v d) -> showDouble v ++ ";" ++ showDouble d)
    showDouble =  show . (/10) . (fromIntegral :: Int -> Double) . round . (*10)

    -- We flip the Y-axis here because our transformation DSL can't do
    -- reflection. We do all other transforms through
    -- 'gridTransfroms'.
    draw :: (c -> Picture) -> c -> Location -> Picture
    draw toPic c (Lot col row) = CW.translated (fromIntegral x) (fromIntegral y) (toPic c)
        where 
          (x,y) = (fromEnum col - fromEnum 'a', -row)

    drawAsLabel :: (a -> String) -> a -> Picture
    drawAsLabel s = CW.dilated 0.5 . CW.lettering . pack . s



-- | Compute the scaling to fit the grid in the screen. CodeWorld's
-- viewport is roughly 20 units tall (10..-10) and 30 units wide
-- (-15..15).
scaleFactor :: Farm -> Double
scaleFactor (Farm w h _) = min (30 / (w' + 2)) (20 / (h' + 2))
  where
    (w', h') = (fromIntegral w, fromIntegral h)

toCenter :: Farm -> Transform
toCenter (Farm w h _) = Translate offsetX offsetY
  where
    -- The top-left corner of the top-left cell is at (-0.5, 0.5)
    -- because CodeWorld centers new Pictures around the origin.
    offsetX = -w'/2 + 0.5
    offsetY = h'/2 - 0.5

    (w', h') = (fromIntegral w, fromIntegral h)

-- Implement a simple matrix library to avoid dependencies.
-- Operator and type names come from Ed Kmett's `linear` library:
-- https://hackage.haskell.org/package/linear-1.21/docs/Linear-Matrix.html

-- | Type of 3x3 matrices.
type M33 a = V3 (V3 a)

-- | Type of length-3 vectors.
data V3 a = V3 a a a

infixl 7 !*!
-- | Matrix multiplication.
(!*!) :: M33 Double -> M33 Double -> M33 Double
(!*!) (V3 (V3 a1 b1 c1)
          (V3 d1 e1 f1)
          (V3 g1 h1 i1))
      (V3 (V3 a2 b2 c2)
          (V3 d2 e2 f2)
          (V3 g2 h2 i2)) = V3 (V3 a3 b3 c3)
                              (V3 d3 e3 f3)
                              (V3 g3 h3 i3)
  where
    a3 = a1*a2 + b1*d2 + c1*g2
    b3 = a1*b2 + b1*e2 + c1*h2
    c3 = a1*c2 + b1*f2 + c1*i2
    d3 = d1*a2 + e1*d2 + f1*g2
    e3 = d1*b2 + e1*e2 + f1*h2
    f3 = d1*c2 + e1*f2 + f1*i2
    g3 = g1*a2 + h1*d2 + i1*g2
    h3 = g1*b2 + h1*e2 + i1*h2
    i3 = g1*c2 + h1*f2 + i1*i2

infixl 7 !*
-- | Matrix-vector multiplication.
(!*) :: M33 Double -> V3 Double -> V3 Double
(!*) (V3 (V3 a b c)
         (V3 d e f)
         (V3 g h i)) (V3 x y z) = V3 x' y' z'
  where
    x' = a*x + b*y + c*z
    y' = d*x + e*y + f*z
    z' = g*x + h*y + i*z

-- | Identity matrix.
identity :: M33 Double
identity = V3 (V3 1 0 0) (V3 0 1 0) (V3 0 0 1)

-- | Invert a non-singular matrix.
inv33 :: M33 Double -> M33 Double
inv33 (V3 (V3 a b c)
          (V3 d e f)
          (V3 g h i)) = V3 (V3 a' d' g')
                           (V3 b' e' h')
                           (V3 c' f' i')
  where
    a' =  (e*i - f*h) / det
    b' = -(d*i - f*g) / det
    c' =  (d*h - e*g) / det
    d' = -(b*i - c*h) / det
    e' =  (a*i - b*g) / det
    f' = -(a*h - b*g) / det
    g' =  (b*f - c*e) / det
    h' = -(a*f - c*d) / det
    i' =  (a*e - b*d) / det

    det = a*(e*i - f*h) - b*(d*i - f*g) + c*(d*h - e*g)
