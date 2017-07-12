module Main where

import BT
import Grid
import SVG

import Control.Monad.State
import Data.Graph.Inductive
import Data.List
import System.Environment
import System.Random
import Text.Blaze.Svg.Renderer.String (renderSvg)
                          
makeDefaults :: Int -> Int -> Config
makeDefaults imageSize mazeSize = Config { width = imageSize
                                         , height = imageSize
                                         , padding = 10
                                         , wall = fromIntegral imageSize / fromIntegral mazeSize
                                         , lineW = 1
                                         , lineC = "blue"
                                         }
                                       
mazeToFile :: String -> Int -> Int -> IO ()
mazeToFile fname imageSize mazeSize = do
  let grid = gridNE mazeSize mazeSize
  gen <- getStdGen
  let removedWalls = evalState (binTree grid) gen
  let mazeWalls = getWalls grid \\ removedWalls
  let svg = render mazeWalls
  let defaults = makeDefaults imageSize mazeSize
  writeFile fname $ renderSvg $ svg defaults
  
main :: IO ()
main = do
  [fname, imageSize, mazeSize] <- getArgs
  mazeToFile fname (read imageSize) (read mazeSize)