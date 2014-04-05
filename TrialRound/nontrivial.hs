{-# LANGUAGE ParallelListComp #-}

import qualified Data.Vector.Unboxed as U
import Debug.Trace

data Instr = PaintSQ (Int, Int) Int
           | EraseCell (Int, Int)
type Solution = [Instr]

printInstr :: Instr -> String
printInstr (PaintSQ (x, y) z) = "PAINTSQ " ++ (unwords . map show $ [x, y, z])
printInstr (EraseCell (x, y)) = "ERASECELL " ++ (unwords . map show $ [x, y])

printSolution :: Solution -> IO ()
printSolution sol =
  putStrLn $ show (length sol) ++ "\n" ++ unlines (map printInstr sol)

parse :: String -> (Int, Int, U.Vector Bool)
parse file =
  let (header:l) = lines file
      [strh, strw] = words header
      h :: Int
      h = read strh
      w :: Int
      w = read strw
      t = U.fromList . map (== '#') . concat $ l
  in (h, w, t)
   
main :: IO ()      
main = printSolution . solveQuad . parse =<< readFile "doodle.txt"

log2 :: Integer -> Integer
log2 = ceiling . log . fromIntegral

data QuadTree = Node ![QuadTree] !Int {- density -} (Int, Int {- x, y -}) Int {- n -} | Leaf Bool (Int, Int)
getDensity (Leaf x _) = if x then 1 else 0
getDensity (Node _ d _ _) = d

toQuad i j = i*3 + j

log3 :: Int -> Int
log3 = ceiling . logBase 3 . fromIntegral

makeIJ :: Int -> (Int, Int) -> [(Int, Int)]
makeIJ n (i, j) = [(i + n*k1, j + n*k2) | k1 <- [0..2], k2 <- [0..2]]

makeQuadTree :: (Int, Int, U.Vector Bool) -> (Int, Int) -> Int -> QuadTree
makeQuadTree (h, w, t) (i, j) 1 | i >= h || j >= w = Leaf False (i, j)
makeQuadTree (h, w, t) (i, j) 1 = Leaf (t U.! (i*w+j)) (i, j)
makeQuadTree (h, w, t) (i, j) n =
	let n2 = n `div` 3 in
	let ls = map (\ p -> makeQuadTree (h, w, t) p n2) (makeIJ n2 (i, j)) in
	Node ls (sum . map getDensity $ ls) (i, j) n

fuseSols :: [(Int, Solution)] -> (Int, Solution)
fuseSols ss =
	let (costs, sols) = unzip ss in
	(sum costs, concat sols)

solveQuadTree :: U.Vector Bool -> Int -> Int -> QuadTree -> (Int, Solution)
solveQuadTree t _ _ (Leaf True (i, j)) = (1, [PaintSQ (i, j) 0])
solveQuadTree t _ _ (Leaf False _) = (0, [])
solveQuadTree t w h (Node qs d (i, j) n) | i + n >= h || j + n >= w =
	fuseSols $ [solveQuadTree t w h subnode | subnode <- qs]
solveQuadTree t w h (Node qs d (i, j) n) =
	let (subC, subSol) = fuseSols $ [solveQuadTree t w h subnode | subnode <- qs]
	    (c, sol) = (1 + n*n - d, PaintSQ (i + n `div` 2, j + n `div` 2) (n `div` 2) : [EraseCell (i+k1, j+k2) | k1 <- [0..(n-1)], k2 <- [0..(n-1)], not (t U.!((i+k1)*w +j+k2))])
	in if subC /= length subSol then error "x" else (if c > subC then (subC, subSol) else (c, sol))

solveQuad :: (Int, Int, U.Vector Bool) -> Solution
solveQuad (h, w, t) =
    let q = makeQuadTree (h, w, t) (0, 0) (3^(log3 (max h w))) in
    q `seq` (trace "foo" . snd $ solveQuadTree t w h q)

solve :: (Int, Int, U.Vector Bool) -> Solution
solve (h, w, t) = map paintCell . filter (t !) $ indices
  where t ! (i, j) = t U.! (i*w+j)
        indices = [(i,j) | i <- [0..(h-1)], j <- [0..(w-1)]]
        paintCell (i, j) = PaintSQ (i, j) 0
