module Astar where

import Data.List as L
import Data.Maybe
import Data.Set
import qualified Data.Vector as V
import Data.Functor

import Data.Graph.AStar

import Common

mkIndexToSet g =
  let indexToSet i = fromList (fst <$> (g V.! i)) in
  indexToSet

mkDist :: Graph -> (Int -> Int -> Int)
mkDist g =
  let dist i j =
        let adjL = g V.! i in
        let (_, (c, _)) = fromJust $ find (\ (k, _) -> k == j) adjL in c
                                                                       in dist
sq :: Double -> Double
sq x = x * x

mkHeuristic gNodes iGoal =
  let (goalX, goalY) = gNodes V.! iGoal
      heuristic i =
        let (iX, iY) = gNodes V.! i
            dist = sqrt (sq (iX - goalX) + sq (iY - goalY)) in floor dist

      in heuristic

findAStar g gNodes i goal =
  fromJust $ aStar (mkIndexToSet g) (mkDist g) (mkHeuristic gNodes goal) (== goal) i
        
aStarSol :: Graph -> GraphNodes -> Int -> Int -> (Int, [Int])
aStarSol g gNodes i goal =
  let path = findAStar g gNodes i goal
      (_, cost) = L.foldl' (\ (l, c) j -> (j, c + mkDist g l j)) (i, 0) path
  in (cost, i : path)
