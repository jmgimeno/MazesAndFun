{-# LANGUAGE OverloadedStrings #-}

module SVG where
 
import Control.Monad

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Grid

data Config = Config { width   :: Int
                     , height  :: Int
                     , padding :: Int
                     , wall    :: Float
                     , lineW   :: Int
                     , lineC   :: String 
                     }
                     
defaults :: Config
defaults = Config { width   = 300
                  , height  = 300
                  , padding = 10
                  , wall    = 50.0
                  , lineW   = 2
                  , lineC   = "blue"
                  }
                  
render :: [(Int, Int, Int, Int)] -> Config -> S.Svg
render walls = do
  svg <- mkSvg
  ext <- exterior
  int <- interior walls
  return $ svg $ do {ext; int}

mkSvg :: Config -> S.Svg -> S.Svg
mkSvg = do
  w <- width
  h <- height
  vB <- mkViewBox
  return $  S.docTypeSvg ! A.version "1.1" 
                         ! A.width (S.toValue w)
                         ! A.height (S.toValue h)
                         ! A.viewbox (S.toValue vB)    

mkViewBox :: Config -> String
mkViewBox = do
  w <- width
  h <- height
  p <- padding
  return $ show (-p) ++ " " ++ show (-p) ++ " " 
                     ++ show (w + 2*p) ++ " " ++ show (h + 2*p)
                                  
line :: Float -> Float -> Float -> Float -> Config -> S.Svg 
line  x1 y1 x2 y2  = do
  lW <- lineW
  lC <- lineC
  return $ S.line ! A.x1 (S.toValue x1)
                  ! A.y1 (S.toValue y1)
                  ! A.x2 (S.toValue x2)
                  ! A.y2 (S.toValue y2)
                  ! A.strokeWidth (S.toValue lW)
                  ! A.stroke (S.toValue lC)
                                 
exterior :: Config -> S.Svg
exterior = do
  w <- fromIntegral . width
  h <- fromIntegral . height
  top    <- line 0 0 w 0
  right  <- line w 0 w h 
  bottom <- line 0 h w h
  left   <- line 0 0 0 h
  return $ sequence_ [top,right,bottom,left]
        
interior :: [(Int, Int, Int, Int)] -> Config -> S.Svg
interior walls = do 
  svgs <- traverse wall2svg walls
  return $ sequence_ svgs
  where wall2svg (x, y, x', y') = do
          w <- wall
          line (fromIntegral x  * w) 
               (fromIntegral y  * w) 
               (fromIntegral x' * w) 
               (fromIntegral y' * w)
                             
                             