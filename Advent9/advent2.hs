import Data.Maybe
import Prelude
import Text.Regex

getRegexMatch :: String -> Maybe [String]
getRegexMatch = matchRegex (mkRegex "([a-zA-Z]*+)\\(([0-9]+)x([0-9]+)\\)(.+)")

duplicate n str = [1..n] >> str

decompressWord :: String -> Int
decompressWord [] = 0
decompressWord word = if (regexMatch == Nothing)
                      then length word
                      else (length beforeMarker) + repeat * (decompressWord $ take number afterMarker) + (decompressWord rest)
   where regexMatch = getRegexMatch word
         regexMatch' = (fromJust regexMatch) :: [String]
         beforeMarker = regexMatch' !! 0
         number = read (regexMatch' !! 1)::Int
         repeat = read (regexMatch' !! 2)::Int
         afterMarker = regexMatch' !! 3
         rest = drop number afterMarker

main = do
  c <- getContents
  putStrLn . show $ decompressWord c
