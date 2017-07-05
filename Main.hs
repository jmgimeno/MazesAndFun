module Main where

import SVG
import Text.Blaze.Svg.Renderer.String (renderSvg)

main :: IO ()
main = do
  let a = renderSvg diagram1
  putStrLn a