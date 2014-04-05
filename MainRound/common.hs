import Control.Applicative
import Data.List
import Data.Function
import Data.Vector (Vector)
import qualified Data.Vector as V

type Node = (Int, (Int, Int)) -- index, (time, length)
type Graph = Vector [Node] -- adjacency list

getGraph :: IO ((Int, Int, Int), Graph)
getGraph = parse <$> readFile "paris_54000.txt"

parse :: String -> ((Int, Int, Int), Graph)
parse str =
  let (header:rest) = lines str
      [n, m, t, c, s] = map (read :: String -> Int) . words $ header
      edges = take m . drop n $ rest -- drop nodes
      graph = V.fromList . parcours 0 . groupBy (\x y -> fst x == fst y)
              . sortBy (compare `on` fst) . concatMap parseEdge $ edges
  in ((t, c, s), graph)
  where parseEdge s =
          let [a, b, d, c, l] = map (read :: String -> Int) . words $ s in
          if d == 1 then [(a, (b, (c, l)))]
          else [(a, (b, (c, l))), (b, (a, (c, l)))]
        parcours _ [] = []
        parcours n (x:xs) = let ((m,_):_) = x in
          if n == m then map snd x : parcours (n+1) xs
          else [] : parcours (n+1) (x:xs)
      
main = undefined

