import Prelude
import Data.List.Split
import Data.List

type Triangle = (Int, Int, Int)

isValidTriangle :: Triangle -> Bool
isValidTriangle (a,b,c) =
    (a + b > c) && (b + c > a) && (c + a > b)

validTriangles :: [Triangle] -> [Triangle]
validTriangles = filter isValidTriangle

linesToTriangles :: [[String]] -> [Triangle]
linesToTriangles = map wordsToTriangle
  where
    wordsToTriangle :: [String] -> Triangle
    wordsToTriangle (a:b:c:[]) = (read a :: Int, read b :: Int, read c :: Int)

reorderLines :: [String] -> [[String]]
reorderLines = chunksOf 3 . concat . transpose . map words

main = do
  c <- getContents
  putStrLn . show . length . validTriangles . linesToTriangles . reorderLines $ lines c
