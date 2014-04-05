{-# LANGUAGE TupleSections #-}

import Control.Applicative
import Control.Monad
import Control.Monad.Random
import qualified Data.IntMap as IM
import Data.Function
import Data.List
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as V
import System.IO

import Common
import Astar

mapAccumLM :: Monad m
            => (acc -> x -> m (acc, y)) -- ^ combining funcction
            -> acc                      -- ^ initial state
            -> [x]                      -- ^ inputs
            -> m (acc, [y])             -- ^ final state, outputs
mapAccumLM _ s []     = return (s, [])
mapAccumLM f s (x:xs) = do
    (s1, x')  <- f s x
    (s2, xs') <- mapAccumLM f s1 xs
    return    (s2, x' : xs')


-- state: [(position, time remaining, history in *reverse* (stack))] , set of taken edges
type History = [Int]
type CarState = (Int, Int, History)
data CarState' = Stuck History | Progress CarState

isStuck (Stuck _) = True
isStuck (Progress _) = False

-- t: time limit
-- c: # of cars
-- s: starting point

extractSolution = map f
  where f (Stuck history) = reverse history
        f _ = error "not all stuck"

score :: Edge -> Double
score (_, (time, length)) = fromIntegral length / fromIntegral time

solution :: Datum -> Rand StdGen (Set (Int, Int), Solution)
solution ((t, c, s), graph, nodes) =
  f init2ndPhase (Set.fromList (concat alreadyTakenEdgeLists))
  where startingPoints = [k * 1000 | k <- [1..8]]
        firstPhase = zip startingPoints $ map (aStarSol graph nodes s) startingPoints
        (init2ndPhase, alreadyTakenEdgeLists) = unzip . map h $ firstPhase
          where h (point, (cost, path)) =
                  let path' = s:path in
                  (Progress (point, t - cost, reverse path'),
                   map (uncurry unorderedEdge) (zip path' path))
        
        f state takenEdges
          | all isStuck state = return $ (takenEdges, extractSolution state)
          | otherwise = do
            (takenEdges', state') <- mapAccumLM g takenEdges state
            f state' takenEdges'
        g takenEdges (Stuck x) = return (takenEdges, Stuck x)
        g takenEdges (Progress (pos, remaining, history)) =
          let reachable = filter (\(_, (cost, _)) -> cost <= remaining) $ graph V.! pos in
          if reachable == [] then return (takenEdges, Stuck history)
          else let sorted = sortBy (compare `on` score) reachable
                   unvisited = filter (not . (`Set.member` takenEdges)
                                       . unorderedEdge pos . fst)
                               $ sorted
               in case unvisited of
                 _:_ -> do
                   ix <- getRandomR (0, length unvisited - 1)
                   let (i, (cost,_)) = unvisited !! ix
                   return (Set.insert (unorderedEdge pos i) takenEdges, moveTo i cost)
                 [] -> do
                   ix <- getRandomR (0, length reachable - 1)
                   let (i, (cost,_)) = sorted !! ix
                   return (takenEdges, moveTo i cost)
          where moveTo i cost = Progress (i, remaining - cost, i:history)

unorderedEdge i j = (min i j, max i j)

totalScore takenEdges graph = sum . map f . Set.toList $ takenEdges
  where f (i, j) = let (Just (_, length)) =
                         (lookup j $ graph V.! i) <|> (lookup i $ graph V.! j)
                   in length

main = do
  datum@(_, graph, _) <- getGraph
  let f (bestScore, bestSol) () = do
        (takenEdges, sol) <- evalRandIO . solution $ datum
        let score = totalScore takenEdges graph
        return $ if score > bestScore then (score, sol) else (bestScore, bestSol)
  (score, sol) <- foldM f (0, [[]]) $ replicate 100 ()
  hPutStrLn stderr $ show score
  printSolution sol

