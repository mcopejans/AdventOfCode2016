import Prelude
import Data.List
import Data.List.Split

splitipv6 :: String -> [String]
splitipv6 = splitOneOf "[]"

getOutBrackets :: [String] -> [String]
getOutBrackets [] = [] :: [String]
getOutBrackets (x:[]) = x:getOutBrackets []
getOutBrackets (o:i:xs) = o:(getOutBrackets xs)

getInBrackets :: [String] -> [String]
getInBrackets [] = [] :: [String]
getInBrackets (x:[]) = [] :: [String]
getInBrackets (o:i:xs) = i:(getInBrackets xs)

getAbaSequences :: String -> [String]
getAbaSequences (a:b:c:xs) = if ((a /= b) && (a == c))
                             then [a,b,c]:(getAbaSequences (b:c:xs))
                             else (getAbaSequences (b:c:xs))
getAbaSequences other = [] :: [String]

getAllAbaSequences :: [String] -> [String]
getAllAbaSequences = concat . map getAbaSequences

hasBabSequence :: [String] -> Bool -> String -> Bool
hasBabSequence allAbaSequences acc (a:b:c:[]) = if (b:a:b:[] `elem` allAbaSequences) then True else acc
hasBabSequence _ acc _ = acc

hasAnyBabSequence :: [String] -> [String] -> Bool
hasAnyBabSequence allAbaSequences babSequences = foldl (hasBabSequence allAbaSequences) False babSequences

supportsSSL :: String -> Bool
supportsSSL ipv6 = hasAnyBabSequence (getAllAbaSequences outBrackets) (getAllAbaSequences inBrackets)
   where splitted = splitipv6 ipv6
         outBrackets = getOutBrackets splitted
         inBrackets = getInBrackets splitted

main = do
  c <- getContents
  putStrLn . show . foldl (\i v -> if v then i + 1 else i) 0 . map supportsSSL $ lines c
