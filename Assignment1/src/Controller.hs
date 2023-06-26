--- Copyright 2022 The Australian National University, All rights reserved

module Controller where

import CodeWorld
import Model

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event (Model shapes tool colour) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "Esc" -> startModel

      -- write the current model to the console
      | k == "D" -> trace (pack (show currentModel)) currentModel

      -- display the mystery image
      | k == "S" -> Model sample tool colour

      | k == "Backspace" || k == "Delete" -> case shapes of 
        [] -> Model [] tool colour
        _:xs -> Model xs tool colour  
        

      | k == " " ->  case tool of 
        PolygonTool list -> Model ([(colour, Polygon list)] ++ shapes) (PolygonTool []) colour
        _ -> currentModel
   
      | k == "T" -> Model shapes (nextTool tool) colour  

      | k == "C" -> Model shapes tool (nextColour colour) 

      | k == "+" || k == "=" -> case tool of
        EllipseTool ec _ | (ec + 0.05) < 1.00 -> Model shapes (EllipseTool (ec + 0.05) Nothing) colour
                         | otherwise -> Model shapes (EllipseTool (0.950) Nothing) colour
        _ -> Model shapes tool colour
      
      | k == "-" || k == "_" -> case tool of
        EllipseTool ec _ | ec > 0.00 -> Model shapes (EllipseTool (ec - 0.05) Nothing) colour
                         | otherwise -> Model shapes (EllipseTool (0.00) Nothing) colour
        _ -> Model shapes tool colour

      -- ignore other keys
      | otherwise -> currentModel
      
      where
        k = unpack key

    PointerPress p -> case tool of 
      LineTool _ -> Model shapes (LineTool (Just p)) colour
      PolygonTool list -> Model shapes (PolygonTool (list ++ [p])) colour
      RectangleTool _ -> Model shapes (RectangleTool (Just p)) colour
      CircleTool _ -> Model shapes (CircleTool (Just p)) colour
      TriangleTool _ -> Model shapes (TriangleTool (Just p)) colour
      EllipseTool ec _ -> Model shapes (EllipseTool ec (Just p)) colour

    PointerRelease p -> case tool of 
      LineTool (Just point) -> Model ([(colour, Line point p)] ++ shapes) (LineTool Nothing) colour
      PolygonTool _ -> currentModel
      RectangleTool (Just point) -> Model ([(colour, Rectangle point p)] ++ shapes) (RectangleTool Nothing) colour
      CircleTool (Just (pointx, pointy)) -> case p of
        (px,py) -> Model ([(colour, Circle ((pointx + px)/2,(pointy + py)/2) ((sqrt((py - pointy)**2+(px-pointx)**2))/2))] ++ shapes) (CircleTool Nothing) colour
      TriangleTool (Just point) -> Model ([(colour, Triangle point p)] ++ shapes) (TriangleTool Nothing) colour 
      EllipseTool ec (Just point) -> Model ([(colour, Ellipse (ec) p point)] ++ shapes) (EllipseTool ec Nothing) colour
      _ -> currentModel
    
    -- ignore other events
    _ -> currentModel

    where
     currentModel = Model shapes tool colour

nextColour :: ColourName -> ColourName
nextColour x = case x of
  Black -> Pink
  Pink -> Red
  Red -> Orange
  Orange -> Brown
  Brown -> Green
  Green -> Blue
  Blue -> White
  White -> Black

nextTool :: Tool -> Tool
nextTool tool = case tool of 
  LineTool Nothing -> PolygonTool []
  PolygonTool [] -> RectangleTool Nothing
  RectangleTool Nothing -> CircleTool Nothing
  CircleTool Nothing -> TriangleTool Nothing
  TriangleTool Nothing -> EllipseTool 0.5 Nothing
  EllipseTool _ Nothing -> LineTool Nothing
  _ -> tool