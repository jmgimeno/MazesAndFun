{-# LANGUAGE ViewPatterns #-}

{-

  Binary Tree Algorithm for maze generation.
  
  From Chapter 1 of Mazes for Programmers by Jamis Buck
  
  (Original code in Ruby)
  
-}

module BT where
  
import qualified Data.Graph.Inductive as G

import Control.Monad.State
import System.Random

import Grid
    
randomRSt :: (RandomGen g, Random a) => (a, a) -> State g a
randomRSt range = state $ \gen -> randomR range gen

chooseRandom :: RandomGen g => [a] -> State g a
chooseRandom as = do
  i <- randomRSt (0, length as - 1)
  return $ as !! i

binTree :: RandomGen g => Grid -> State g [Wall]
binTree grid = binTree' grid [0..]
               where binTree' grid _ | G.isEmpty grid = return []
                     binTree' grid (n : ns) 
                       = case G.match n grid of
                               (Nothing, _) -> return []
                               (Just (_,_,_,[]), _) -> binTree' grid ns
                               (Just (_,_,_,outs), _) -> (do (wall, _) <- chooseRandom outs
                                                             walls <- binTree' grid ns
                                                             return $ wall : walls)
                                                                
                             
                             
