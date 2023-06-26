module Main where

import Controller
import Model
import View
import Testing

-- | The list of all tests to run.
tests :: [Test]
tests = toolLabelTests ++ nextColourTests ++ nextToolTests

toolLabelTests :: [Test]
toolLabelTests =
  [ Test "LineTool"
      (assertEqual (toolToLabel (LineTool Nothing))
       "Line: click-drag-release")
  , Test "PolygonTool"
      (assertEqual (toolToLabel (PolygonTool []))
      "Polygon: click at least three times, then spacebar")
  , Test "CircleTool"
      (assertEqual (toolToLabel (CircleTool Nothing))
      "Circle: click-drag-release between opposite points on the circumference")
  , Test "TriangleTool"
      (assertEqual (toolToLabel (TriangleTool Nothing))
      "Triangle: click-drag release along the hypotenuse")
  , Test "RectangleTool"
      (assertEqual (toolToLabel (RectangleTool Nothing))
      "Rectangle: click-drag release between opposite corners")
  , Test "EllipseTool"
      (assertEqual (toolToLabel (EllipseTool 0.5 Nothing))
      "Ellipse: click-drag-release along the major axis")
  ]

nextColourTests :: [Test]
nextColourTests =
  [ Test "Black -> Pink" (assertEqual (nextColour Black) Pink)
  , Test "Pink -> Red" (assertEqual (nextColour Pink) Red)
  , Test "Red -> Orange" (assertEqual (nextColour Red) Orange)
  , Test "Orange -> Brown" (assertEqual (nextColour Orange) Brown)
  , Test "Brown -> Green" (assertEqual (nextColour Brown) Green)
  , Test "Green -> Blue" (assertEqual (nextColour Green) Blue)
  , Test "Blue -> White" (assertEqual (nextColour Blue) White)
  , Test "White -> Black" (assertEqual (nextColour White) Black)
  ]

-- | Tests for nextTool, including tests that it doesn't cycle tools
-- midway through a draw.
nextToolTests :: [Test]
nextToolTests =
  [ Test "Line -> Polygon"
      (assertEqual (nextTool (LineTool Nothing)) (PolygonTool []))
  , Test "Polygon -> Rectangle"
      (assertEqual (nextTool (PolygonTool [])) (RectangleTool Nothing))
  , Test "Rectangle -> Circle"
      (assertEqual (nextTool (RectangleTool Nothing)) (CircleTool Nothing))
  , Test "Circle -> Triangle"
      (assertEqual (nextTool (CircleTool Nothing)) (TriangleTool Nothing))
  , Test "Triangle -> Ellipse"
      (assertEqual (emptyEllipse (nextTool (TriangleTool Nothing))) True)
  , Test "Ellipse -> Line"
        (assertEqual (nextTool (EllipseTool 0.5 Nothing)) (LineTool Nothing))
  , Test "Line (in use) -> Line"
      (assertEqual (nextTool (LineTool (Just (0,1)))) (LineTool (Just (0,1))))
  , Test "Polygon (in use) -> Polygon"
      (assertEqual (nextTool (PolygonTool [(2,3)])) (PolygonTool [(2,3)]))
  , Test "Circle (in use) -> Circle"
      (assertEqual (nextTool (CircleTool (Just (4,5)))) (CircleTool (Just (4,5))))
  , Test "Triangle (in use) -> Triangle"
      (assertEqual (nextTool (TriangleTool (Just (6,7)))) (TriangleTool (Just (6,7))))
  , Test "Ellipse (in use) -> Ellipse (1)"
      (assertEqual (nextTool (EllipseTool 0.5 (Just (8,9)))) 
      (EllipseTool 0.5 (Just (8,9))))
  , Test "Ellipse (in use) -> Ellipse (2)"
      (assertEqual (nextTool (EllipseTool 0.88 (Just (0,1))))
      (EllipseTool 0.88 (Just (0,1))))
  , Test "Rectangle (in use) -> Rectangle"
      (assertEqual (nextTool (RectangleTool (Just (1,1)))) 
      (RectangleTool (Just (1,1))))
    ]

emptyEllipse :: Tool -> Bool
emptyEllipse (EllipseTool _ Nothing) = True
emptyEllipse _ = False

-- | A Haskell program starts by running the computation defined by
-- 'main'. We run the list of tests that we defined above.
main :: IO ()
main = runTests tests