import Prelude
import Data.List
import Data.List.Split

splitipv6 :: String -> [String]
splitipv6 = splitOneOf "[]"

hasAbbaSequence :: String -> Bool
hasAbbaSequence (a:b:c:d:xs) = if ((a /= b) && (a == d) && (b == c)) then True else hasAbbaSequence (b:c:d:xs)
hasAbbaSequence other = False

getOutBrackets :: [String] -> [String]
getOutBrackets [] = [] :: [String]
getOutBrackets (x:[]) = x:getOutBrackets []
getOutBrackets (o:i:xs) = o:(getOutBrackets xs)

getInBrackets :: [String] -> [String]
getInBrackets [] = [] :: [String]
getInBrackets (x:[]) = [] :: [String]
getInBrackets (o:i:xs) = i:(getInBrackets xs)

isValid :: String -> Bool
isValid ipv6 = hasAbbaSequenceOut && (not hasAbbaSequenceIn)
   where splitted = splitipv6 ipv6
         outBrackets = getOutBrackets splitted
         inBrackets = getInBrackets splitted
         hasAbbaSequenceOut = (length $ filter hasAbbaSequence outBrackets) /= 0
         hasAbbaSequenceIn = (length $ filter hasAbbaSequence inBrackets) /= 0

main = do
  c <- getContents
  putStrLn . show . foldl (\i v -> if v then i + 1 else i) 0 . map isValid $ lines c
