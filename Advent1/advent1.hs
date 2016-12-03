import Prelude
import Data.List.Split

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


--nextPosition :: (Direction, Int, Int) -> [] -> (Direction, Int, Int)
--nextPosition :: 

calcCoordinate :: [String] -> (Direction, Int, Int)
calcCoordinate instructions = foldl (processInstruction) ((North, 0,0) :: (Direction, Int, Int)) instructions
  where
    processInstruction :: (Direction, Int, Int) -> String -> (Direction, Int, Int)
    processInstruction (dir, x, y) (rOrL:xs) =
      (nextDir, nextX nextDir x (read xs :: Int), nextY nextDir y (read xs :: Int))
      where nextDir = if (rOrL == 'R') then (nextDirection dir) else (prevDirection dir)

main = putStrLn . show . calcCoordinate $ (splitOn ", " "R3, L5, R1, R2, L5, R2, R3, L2, L5, R5, L4, L3, R5, L1, R3, R4, R1, L3, R3, L2, L5, L2, R4, R5, R5, L4, L3, L3, R4, R4, R5, L5, L3, R2, R2, L3, L4, L5, R1, R3, L3, R2, L3, R5, L194, L2, L5, R2, R1, R1, L1, L5, L4, R4, R2, R2, L4, L1, R2, R53, R3, L5, R72, R2, L5, R3, L4, R187, L4, L5, L2, R1, R3, R5, L4, L4, R2, R5, L5, L4, L3, R5, L2, R1, R1, R4, L1, R2, L3, R5, L4, R2, L3, R1, L4, R4, L1, L2, R3, L1, L1, R4, R3, L4, R2, R5, L2, L3, L3, L1, R3, R5, R2, R3, R1, R2, L1, L4, L5, L2, R4, R5, L2, R4, R4, L3, R2, R1, L4, R3, L3, L4, L3, L1, R3, L2, R2, L4, L4, L5, R3, R5, R3, L2, R5, L2, L1, L5, L1, R2, R4, L5, R2, L4, L5, L4, L5, L2, L5, L4, R5, R3, R2, R2, L3, R3, L2, L5")
