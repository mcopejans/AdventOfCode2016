import Prelude
import Data.Function (on)
import Data.List

getMostCommonLetter :: String -> Char
getMostCommonLetter = head . maximumBy (compare `on` length) . group . sort

getErrorCorrectedWord :: [String] -> String
getErrorCorrectedWord = map getMostCommonLetter

main = do
  c <- getContents
  putStrLn . show . getErrorCorrectedWord . transpose $ lines c
