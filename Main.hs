module Main where

import BT
import Grid
import SVG

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
  let g = gridNE 40 40
  gen <- getStdGen
  let (nw, _) = binTree g gen
  let w = walls g \\ nw
  let svg = render w
  let str = renderSvg $ svg defaults
  writeFile "grid.svg" str
  