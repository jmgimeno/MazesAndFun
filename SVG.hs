{-# LANGUAGE OverloadedStrings #-}

module SVG where
 
import Control.Monad
import Control.Monad.Reader

import Text.Blaze.Svg11 ((!), mkPath, rotate, l, m)
import qualified Text.Blaze.Svg11 as S
import qualified Text.Blaze.Svg11.Attributes as A

import Grid

diagram1 :: S.Svg
diagram1 = S.docTypeSvg ! A.version "1.1" ! A.width "100" ! A.height "100" $
  S.circle ! A.cx "50" ! A.cy "50" ! A.r "40" ! A.stroke "green"
           ! A.strokeWidth "4" ! A.fill "yellow"

diagram2 :: S.Svg
diagram2 = S.docTypeSvg ! A.version "1.1" ! A.width "100" ! A.height "100" $ S.g $ do
  S.line ! A.x1 "50" ! A.y1 "50" ! A.x2 "90" !  A.y2 "90" ! A.stroke "green"
           ! A.strokeWidth "4"
  S.line ! A.x1 "50" ! A.y1 "90" ! A.x2 "90" !  A.y2 "50" ! A.stroke "blue"
           ! A.strokeWidth "4"

diagram3 :: S.Svg
diagram3 = S.docTypeSvg ! A.version "1.1" ! A.width "150" ! A.height "100" ! A.viewbox "0 0 3 2" $ do
    S.g ! A.transform makeTransform $ do
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#008d46"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#ffffff"
      S.rect ! A.width "1" ! A.height "2" ! A.fill "#d2232c"
      S.path ! A.d makePath
           
makePath :: S.AttributeValue
makePath = mkPath $ do
  l 2 3
  m 4 5

makeTransform :: S.AttributeValue
makeTransform = rotate 50


data Config = Config { width  :: Int
                     , height :: Int
                     , wall   :: Int
                     , lineW  :: Int
                     , lineC  :: String                     }
                     
defaults :: Config
defaults = Config { width  = 300
                  , height = 300
                  , wall   = 50
                  , lineW  = 2
                  , lineC  = "blue"
                  }
                  

render :: Config -> [(Int, Int, Int, Int)] -> S.Svg
render config walls = S.docTypeSvg ! A.version "1.1" 
                               ! A.width "300"
                               ! A.height "300"
                               ! A.viewbox "-10 -10 320 320"
                               $ S.g $ do
                                 exterior config
                                 interior walls config
                          
                             
line :: Config -> Int -> Int -> Int -> Int -> S.Svg 
line config x1 y1 x2 y2 = S.line ! A.x1 (S.toValue x1)
                                 ! A.y1 (S.toValue y1)
                                 ! A.x2 (S.toValue x2)
                                 ! A.y2 (S.toValue y2)
                                 ! A.strokeWidth (S.toValue (lineW config))
                                 ! A.stroke (S.toValue (lineC config))
                                 
exterior :: Config -> S.Svg
exterior config = S.g $ do
  line config 0 0 w 0 
  line config 0 0 0 h 
  line config w 0 w h 
  line config 0 h w h 
  where w = width config
        h = height config
        
interior :: [(Int, Int, Int, Int)] -> Config -> S.Svg
interior walls config = forM_ walls $ \(x, y, x', y') -> 
  wall2svg config x y x' y' 
  
wall2svg :: Config -> Int -> Int -> Int -> Int -> S.Svg
wall2svg config x y x' y' = line config x1 y1 x2 y2 
                       where x1 = x * w
                             y1 = y * w
                             x2 = x' * w
                             y2 = y' * w
                             w = wall config
                             
                             