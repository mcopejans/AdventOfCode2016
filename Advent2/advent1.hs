import Prelude
import Control.Monad
import Data.Char

data Pos = Edge1 | Middle | Edge2 deriving (Enum, Eq, Ord, Show)

nextCoord :: (Pos, Pos) -> String -> (Pos, Pos)
nextCoord (x, y) instruction = foldl processInstruction (x, y) instruction
  where
    processInstruction :: (Pos, Pos) -> Char -> (Pos, Pos)
    processInstruction (x, y) lrud
      | lrud == 'D' = (x, toEnum (min ((fromEnum y) + 1) 2))
      | lrud == 'U' = (x, toEnum (max ((fromEnum y) - 1) 0))
      | lrud == 'L' = (toEnum (max ((fromEnum x) - 1) 0), y)
      | lrud == 'R' = (toEnum (min ((fromEnum x) + 1) 2), y)

coordToCodeNumber :: (Pos, Pos) -> Int
coordToCodeNumber (x, y) = (fromEnum x) + 3 * (fromEnum y) + 1

codeNumberToCoord :: Int -> (Pos, Pos)
codeNumberToCoord code =
  let x = ((code - 1) `mod` 3)
  in (toEnum x, toEnum (((code - 1) - x) `div` 3))

getCode :: [String] -> [Int]
getCode instructions = scanl nextCode 5 instructions
  where
    nextCode :: Int -> String -> Int
    nextCode code instruction = coordToCodeNumber $ nextCoord (codeNumberToCoord code) instruction

main = do
  c <- getContents
  putStrLn . show . getCode $ lines c
