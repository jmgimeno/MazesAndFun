{-# LANGUAGE ViewPatterns #-}

module BT where
  
import qualified Data.Graph.Inductive as G

import System.Random

import Grid

selectRandom :: RandomGen g => [a] -> g -> (a, g)
selectRandom as gen = let (i, gen') = randomR (0, length as - 1) gen
                      in (as !! i, gen')
                    
binTree :: RandomGen g => Grid -> g -> ([Wall], g)
binTree grid gen | G.isEmpty grid = ([], gen)
binTree (G.matchAny -> ((ins, _, _, outs), grid')) gen
  = if null walls
    then binTree grid' gen
    else let ((wall, _), gen') = selectRandom walls gen  
             (walls', gen'') = binTree grid' gen'
         in (wall : walls', gen'')
    where walls = ins ++ outs