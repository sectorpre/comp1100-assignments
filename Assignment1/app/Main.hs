--- Copyright 2022 The Australian National University, All rights reserved
module Main where

import CodeWorld
import Controller
import Model
import View

main :: IO ()
main = activityOf startModel handleEvent modelToPicture