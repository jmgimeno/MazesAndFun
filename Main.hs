module Main where

import Grid
import SVG

import qualified Data.ByteString as BS
import Text.Blaze.Renderer.Utf8 (renderMarkupToByteStringIO)

main :: IO ()
main = do
  let g = gridNE 6 6
  let w = walls g
  let svg = render w
  renderMarkupToByteStringIO (BS.writeFile "grid.svg") (svg defaults)
  