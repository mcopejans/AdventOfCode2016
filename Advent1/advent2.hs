import Prelude
import Data.List.Split
import Data.List
import Debug.Trace

data Direction = North | East | South | West deriving (Eq, Enum, Show)
nextDirection :: Direction -> Direction
nextDirection West = North
nextDirection t = succ t

prevDirection :: Direction -> Direction
prevDirection North = West
prevDirection t = pred t

nextX :: Direction -> Int -> Int -> Int
nextX dir x length
  | dir == North = x
  | dir == South = x
  | dir == West = x - length
  | dir == East = x + length

nextY :: Direction -> Int -> Int -> Int
nextY dir y length
  | dir == East = y
  | dir == West = y
  | dir == North = y + length
  | dir == South = y - length

expandInstructions :: [String] -> [String]
expandInstructions instructions = foldl expand ([] :: [String]) instructions
  where
    expand :: [String] -> String -> [String]
    expand acc (dir:xs) =
      let instruction0 = ([dir] ++ "1") :: String
          instructionN = ("N" ++ "1") :: String
      in acc ++ instruction0:((replicate ((read xs :: Int) - 1) (instructionN :: String)) ++ [])

calcCoordinate :: [String] -> [(Direction, Int, Int)]
calcCoordinate instructions = scanl (processInstruction) ((North, 0, 0) :: (Direction, Int, Int)) (expandInstructions instructions)
  where
    processInstruction :: (Direction, Int, Int) -> String -> (Direction, Int, Int)
    processInstruction (dir, x, y) (dirInstruction:xs) =
      (nextDir, nextX nextDir x (read xs :: Int), nextY nextDir y (read xs :: Int))
      where
      nextDir' :: Char -> Direction
      nextDir' rln
        | rln == 'R' = nextDirection dir
        | rln == 'L' = prevDirection dir
        | rln == 'N' = dir
      nextDir = nextDir' dirInstruction

toP :: [(Direction, Int, Int)] -> [(Int, Int)]
toP = map (\(_, x, y) -> (x, y))

f :: [(Int, Int)] -> Maybe (Int, Int)
f [] = Nothing
f (a:tail)
  | a `elem` tail = Just a
  | otherwise = f tail

main = do
  let path = calcCoordinate $ (splitOn ", " "R3, L5, R1, R2, L5, R2, R3, L2, L5, R5, L4, L3, R5, L1, R3, R4, R1, L3, R3, L2, L5, L2, R4, R5, R5, L4, L3, L3, R4, R4, R5, L5, L3, R2, R2, L3, L4, L5, R1, R3, L3, R2, L3, R5, L194, L2, L5, R2, R1, R1, L1, L5, L4, R4, R2, R2, L4, L1, R2, R53, R3, L5, R72, R2, L5, R3, L4, R187, L4, L5, L2, R1, R3, R5, L4, L4, R2, R5, L5, L4, L3, R5, L2, R1, R1, R4, L1, R2, L3, R5, L4, R2, L3, R1, L4, R4, L1, L2, R3, L1, L1, R4, R3, L4, R2, R5, L2, L3, L3, L1, R3, R5, R2, R3, R1, R2, L1, L4, L5, L2, R4, R5, L2, R4, R4, L3, R2, R1, L4, R3, L3, L4, L3, L1, R3, L2, R2, L4, L4, L5, R3, R5, R3, L2, R5, L2, L1, L5, L1, R2, R4, L5, R2, L4, L5, L4, L5, L2, L5, L4, R5, R3, R2, R2, L3, R3, L2, L5")
  putStrLn . show . last $ path
  putStrLn . show . f. toP $ path
