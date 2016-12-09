import Prelude
import Data.Function
import Data.List

shiftLetter :: Int -> Char -> Char
shiftLetter _ '-' = ' '
shiftLetter shift letter = toEnum ((fromEnum 'a') + (((fromEnum letter) - (fromEnum 'a') + shift) `mod` 26))

shiftRoomLetters :: String -> (String, Int)
shiftRoomLetters room =
  let shift = getSectorId room
      letters = filter (`elem` '-':['a'..'z']) (takeWhile (/= '[') room)
  in (map (shiftLetter shift) (takeWhile (/= '[') letters), shift)

getSectorId :: String -> Int
getSectorId room = read (filter (`elem` ['0'..'9']) room) :: Int

getSectorIds :: [String] -> [Int]
getSectorIds = map getSectorId

getSectorIdOfNorthPoleObjectsRoom :: [String] -> [(String, Int)]
getSectorIdOfNorthPoleObjectsRoom rooms = (map shiftRoomLetters rooms)

main = do
  c <- getContents
  putStrLn . show . getSectorIdOfNorthPoleObjectsRoom $ lines c
