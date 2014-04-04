import qualified Data.Vector.Unboxed as U

data Instr = PaintSQ Int Int Int
           | EraseCell Int Int
type Solution = [Instr]

printInstr :: Instr -> String
printInstr (PaintSQ x y z) = "PAINTSQ " ++ (unwords . map show $ [x, y, z])
printInstr (EraseCell x y) = "ERASECELL " ++ (unwords . map show $ [x, y])

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
main = printSolution . solve . parse =<< readFile "doodle.txt"

solve :: (Int, Int, U.Vector Bool) -> Solution
solve (h, w, t) = undefined
  where t ! (i, j) = t U.! (i*h+j)

