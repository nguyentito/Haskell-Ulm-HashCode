module Common where

import Control.Monad.Random
import Control.Monad
import Control.Applicative
import Data.List
import Data.Function
import Data.Vector (Vector)
import qualified Data.Vector as V


type Edge = (Int, (Int, Int)) -- index, (time, length)
type Graph = Vector [Edge] -- adjacency list
type GraphNodes = Vector (Double, Double)

type Datum = ((Int, Int, Int), Graph, GraphNodes)

getGraph :: IO Datum
getGraph = parse <$> readFile "paris_54000.txt"

pairFromList (i : j : []) = (i, j)
pairFromList _ = error "pairFromList"

parse :: String -> Datum
parse str =
  let (header:rest) = lines str
      [n, m, t, c, s] = map (read :: String -> Int) . words $ header
      nodes = take n rest
      edges = take m . drop n $ rest -- drop nodes
      graph = V.fromList . parcours 0 . groupBy (\x y -> fst x == fst y)
              . sortBy (compare `on` fst) . concatMap parseEdge $ edges
      graphNodes = V.fromList $ map (pairFromList . map read . words) nodes
  in ((t, c, s), graph, graphNodes)
  where parseEdge s =
          let [a, b, d, c, l] = map (read :: String -> Int) . words $ s in
          if d == 1 then [(a, (b, (c, l)))]
          else [(a, (b, (c, l))), (b, (a, (c, l)))]
        parcours _ [] = []
        parcours n (x:xs) = let ((m,_):_) = x in
          if n == m then map snd x : parcours (n+1) xs
          else [] : parcours (n+1) (x:xs)

type Solution = [[Int]]


printSolution :: Solution -> IO ()
printSolution sol = do
  print c
  mapM_ printTraj sol
  where
    c = length sol
    printTraj t = do
      print (length t)
      mapM_ print t
    
testSol = [[1, 5, 6], [1, 7], [1, 2, 3]]

chooseIn :: RandomGen g => Vector a -> Rand g Int
chooseIn v = (getRandomR (0, (V.length v - 1)))
 
getEight :: RandomGen g => Vector a -> Rand g [Int]
getEight v = replicateM 8 (chooseIn v)
 
sq x = x * x
 
variance gN cs = sum [let (i1, j1) = gN V.! c1 in let (i2, j2) = gN V.! c2 in sq (i1 - i2) + sq (j1 - j2)
                   | c1 <- cs, c2 <- cs]



