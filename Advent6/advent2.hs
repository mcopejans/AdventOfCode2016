import Prelude
import Data.Function (on)
import Data.List

getLeastCommonLetter :: String -> Char
getLeastCommonLetter = head . minimumBy (compare `on` length) . group . sort

getErrorCorrectedWord :: [String] -> String
getErrorCorrectedWord = map getLeastCommonLetter

main = do
  c <- getContents
  putStrLn . show . getErrorCorrectedWord . transpose $ lines c
