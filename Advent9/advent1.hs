import Data.Maybe
import Prelude
import Text.Regex

getRegexMatch :: String -> Maybe [String]
getRegexMatch = matchRegex (mkRegex "(([0-9]+)x([0-9]+))(.+)")

decompressWord :: String -> String
decompressWord [] = []
decompressWord word = if (regexMatch == Nothing)
                      then word
                      else beforeMarker ++ repeatedSequence ++ (decompressWord rest)
   where beforeMarker = takeWhile (/= '(') word
         regexMatch = getRegexMatch (drop (length beforeMarker) word)
         number = read ((fromJust regexMatch) !! 1)::Int
         repeat = read ((fromJust regexMatch) !! 2)::Int
         afterMarker = tail ((fromJust regexMatch) !! 3)
         repeatedSequence = concat . replicate repeat $ take number afterMarker
         rest = drop number afterMarker

decompressWords :: [String] -> [String]
decompressWords = map decompressWord

getWordLengths :: [String] -> [Int]
getWordLengths = map length

main = do
  c <- getContents
  putStrLn . show . map length . decompressWords $ lines c
