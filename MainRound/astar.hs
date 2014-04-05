module Astar where

import Data.Graph.AStar


mkIndexToSet g =
  let indexToSet i = fromList (fst <$> (g ! i)) in
  indexToSet

mkDist g =
  let dist i j =
        let adjL = g ! i in
        let (_, (c, _)) = fromJust $ find (\ (k, _) -> k == j) adjL in c
                                                                       in dist

sq x = x * x

mkHeuristic gNodes iGoal =
  let (goalX, goalY) = gNodes ! iGoal
      heuristic i =
        let (iX, iY) = gNodes ! i
            dist = sqrt (sq (ix - goalX) + sq (iY - goalY)) in dist

findAStar g gNodes i goal =
  fromJust $ aStar (mkIndexToSet g) (mkDist g) (mkHeuristic gNodes goal) (== goal) i
        
aStarSol :: Graph -> GraphNodes -> Int -> Int -> (Int, [Int])
aStarSol g gNodes i goal =
  let path = findAStar g gNodes i goal
      (last, cost) = foldl (\ (l, c) j -> (j, c + mkDist g l j) (i, 0) path
  (foldl' (), i : path)
