module Main where

import BT
import Grid
import SVG

import Control.Monad.State
import Data.Graph.Inductive
import Data.List
import System.Random
import Text.Blaze.Svg.Renderer.String (renderSvg)
                 
defaults :: Config
defaults = Config { width   = 400  -- Hardcoded 20x20
                  , height  = 400
                  , padding = 10
                  , wall    = 10.0
                  , lineW   = 1
                  , lineC   = "blue"
                  }
                  
main :: IO ()
main = do
  let grid = gridNE 40 40
  gen <- getStdGen
  let unWalls = evalState (binTree grid) gen
  let walls = getWalls grid \\ unWalls
  let svg = render walls
  let str = renderSvg $ svg defaults
  writeFile "maze.svg" str
  