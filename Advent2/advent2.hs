import Prelude
import Data.Char
import Data.List
import Data.Maybe

field :: [String]
field = ["XX1XX", "X234X", "56789", "XABCX", "XXDXX"]

type Coord = (Int, Int)

isInField :: Coord -> Bool
isInField (x, y) = (y >= 0) && (x >= 0) && (y < length field) && (x < length (field !! y)) && (((field !! y) !! x) /= 'X')

nextCoordInField :: Coord -> String -> Coord
nextCoordInField (x, y) instruction = foldl processInstruction (x, y) instruction
  where
    processInstruction :: Coord -> Char -> Coord
    processInstruction (x, y) lrud
      | lrud == 'D' = (x, if isInField (x, y + 1) then y + 1 else y)
      | lrud == 'U' = (x, if isInField (x, y - 1) then y - 1 else y)
      | lrud == 'L' = (if isInField (x - 1, y) then x - 1 else x, y)
      | lrud == 'R' = (if isInField (x + 1, y) then x + 1 else x, y)

coordToCodeNumber :: Coord -> Char
coordToCodeNumber (x, y) = field !! y !! x

codeNumberToCoord' :: Char -> [String] -> Int -> Coord
codeNumberToCoord' code (x:xs) y =
      if code `elem` x
        then (fromJust (code `elemIndex` x), y)
        else codeNumberToCoord' code xs (y + 1)

codeNumberToCoord :: Char -> Coord
codeNumberToCoord code = codeNumberToCoord' code field 0

getCode :: [String] -> [Char]
getCode instructions = scanl nextCode '5' instructions
  where
    nextCode :: Char -> String -> Char
    nextCode code instruction = coordToCodeNumber $ nextCoordInField (codeNumberToCoord code) instruction

main = do
  c <- getContents
  putStrLn . show . getCode $ lines c
