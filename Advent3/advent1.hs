import Prelude
import Data.List.Split

type Triangle = (Int, Int, Int)

isValidTriangle :: Triangle -> Bool
isValidTriangle (a,b,c) =
    (a + b > c) && (b + c > a) && (c + a > b)

validTriangles :: [Triangle] -> [Triangle]
validTriangles triangles = filter isValidTriangle triangles

linesToTriangles :: [String] -> [Triangle]
linesToTriangles lines = map (wordsToTriangle . words) lines
  where
    wordsToTriangle :: [String] -> Triangle
    wordsToTriangle (a:b:c:[]) = (read a :: Int, read b :: Int, read c :: Int)

main = do
  c <- getContents
  putStrLn . show . length . validTriangles . linesToTriangles $ lines c
