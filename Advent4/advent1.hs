import Prelude
import Data.Function
import Data.List

calcCheckSum' :: String -> [(Char, Int)] -> [(Char, Int)]
calcCheckSum' [] code = code
calcCheckSum' (a:xs) code =
      let  spanRes = span (== a) xs
           n = length (fst spanRes) + 1
      in calcCheckSum' (snd spanRes) ((a, n):code)

sortCodeList :: [(Char, Int)] -> [(Char, Int)]
sortCodeList = reverse . sortBy (compare `on` snd $)

toCode :: [(Char, Int)] -> String
toCode = map fst

calcCheckSum :: String -> String
calcCheckSum letters = take 5 . toCode . sortCodeList $ calcCheckSum' letters ([] :: [(Char, Int)])

isRealRoom :: String -> Bool
isRealRoom room =
  let checkSum =  init . reverse . take 6 $ reverse room
      letters = sort . filter (`elem` ['a'..'z']) $ takeWhile (/= '[') room
  in checkSum == calcCheckSum letters

getSectorId :: String -> Int
getSectorId room = read (filter (`elem` ['0'..'9']) room) :: Int

getRealRooms :: [String] -> [String]
getRealRooms = filter isRealRoom

getSectorIds :: [String] -> [Int]
getSectorIds = map getSectorId

getSumSectorIdsRealRooms :: [String] -> Int
getSumSectorIdsRealRooms = sum . getSectorIds . getRealRooms

main = do
  c <- getContents
  putStrLn . show . getSumSectorIdsRealRooms $ lines c
