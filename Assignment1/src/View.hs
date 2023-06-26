--- Copyright 2022 The Australian National University, All rights reserved

module View where

import CodeWorld
import Data.Text (pack)
import Model

-- | Render all the parts of a Model to a CodeWorld picture.
-- You do not need to understand this function other than that
-- it calls your functions colourShapeToPicture and toolToLabel.
modelToPicture :: Model -> Picture
modelToPicture (Model ss t c)
  = translated 0 8 toolText
  & translated 0 7 colourText
  & translated 0 (-8) ellipseText
  & colourShapesToPicture ss
  where
    colourText = (stringToText . show) c
    toolText = (dilated 0.7 . stringToText . toolToLabel) t
    ellipseText = (dilated 0.5 . stringToText) (case t of
      EllipseTool e _ -> "Eccentricity: 0." ++
        (drop 2 . show $ (10000 + floor (1000*e) :: Int)) ++
        "\n press +/- to increase/decrease by 0.05;"
      _ -> "")
    stringToText = lettering . pack

toolToLabel :: Tool -> String
toolToLabel tool = case tool of
  LineTool _ -> "Line: click-drag-release"
  PolygonTool _ -> "Polygon: click at least three times, then spacebar"
  RectangleTool _ -> "Rectangle: click-drag release between opposite corners"
  CircleTool _ -> "Circle: click-drag-release between opposite points on the circumference"
  TriangleTool _ -> "Triangle: click-drag release along the hypotenuse"
  EllipseTool _ _ -> "Ellipse: click-drag-release along the major axis"


colourShapesToPicture :: [ColourShape] -> Picture
colourShapesToPicture list = case list of 
  [] -> blank
  (x:[]) -> colourShapeToPicture x
  (x:xs) -> colourShapeToPicture x & colourShapesToPicture xs 

colourShapeToPicture :: ColourShape -> Picture
colourShapeToPicture (x, y) = coloured (colourNameToColour x) (shapeToPicture y)

colourNameToColour :: ColourName -> Colour
colourNameToColour colourName = case colourName of 
  Black -> black
  Pink -> pink
  Red -> red
  Orange -> orange
  Brown -> brown
  Green -> green
  Blue -> blue
  White -> white

shapeToPicture :: Shape -> Picture
shapeToPicture shape = case shape of
  Line (point1) (point2) -> polyline [point1,point2]
  Polygon xs -> solidPolygon xs
  Rectangle (point1x,point1y) (point2x,point2y) -> translated ((point1x + point2x)/2) ((point1y + point2y)/2) (solidRectangle (point2x - point1x) (point2y - point1y))
  Circle (point1,point2) double -> translated point1 point2 (solidCircle double)
  Triangle (point1x,point1y) (point2x,point2y) ->  translated point1x point1y ((rotated (1.5708 + 2*(atan((point1y + (k) - point2y)/(point1x + (k) - point2x))))) (solidPolygon [(0, 0), (k, k), (0, k)]))
    where k = ((sqrt ((point1x-point2x)**2 + (point1y-point2y)**2))/2)*(1/(cos 0.785398))
  Ellipse double (point1x, point1y) (point2x, point2y) -> translated ((point1x + point2x)/2) ((point1y + point2y)/2) (rotated (atan((point1y-point2y)/(point1x-point2x))) (scaled 1 (sqrt(1-double**2)) (solidCircle ((sqrt ((point1x-point2x)**2 + (point1y-point2y)**2))/2))))

