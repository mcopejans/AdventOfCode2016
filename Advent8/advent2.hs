import Data.List
import Data.Maybe
import Prelude
import Text.Regex

type Screen = [[Int]]

initScreen :: Screen
initScreen = replicate 6 (replicate 50 0)

processRectangleInstruction :: Screen -> Int -> Int -> Screen
processRectangleInstruction screen a b = (map (set a) (take b screen)) ++ (drop b screen)
  where set :: Int -> [Int] -> [Int]
        set a row = ((replicate a 1) :: [Int]) ++ (drop a row)

processRotateRowInstruction :: Screen -> Int -> Int -> Screen
processRotateRowInstruction screen y r = before ++ [((reverse $ take r (reverse row))) ++ (reverse $ (drop r (reverse row)))] ++ after
    where
        before = take y screen
        after = drop (y+1) screen
        row = (screen !! y)

processInstruction :: Screen -> String -> Screen
processInstruction screen ('r':'e':'c':'t':xs) = processRectangleInstruction screen a b
        where regexMatches = matchRegex (mkRegex " ([0-9]+)x([0-9]+)") xs
              a = read ((fromJust regexMatches) !! 0)::Int
              b = read ((fromJust regexMatches) !! 1)::Int
processInstruction screen ('r':'o':'t':'a':'t':'e':' ':'r':'o':'w':xs) = processRotateRowInstruction (screen :: Screen) y r
        where regexMatches = matchRegex (mkRegex " y=([0-9]+) by ([0-9]+)") xs
              y = read ((fromJust regexMatches) !! 0)::Int
              r = read ((fromJust regexMatches) !! 1)::Int
processInstruction screen ('r':'o':'t':'a':'t':'e':' ':'c':'o':'l':'u':'m':'n':xs) = (transpose $ processRotateRowInstruction (transpose (screen :: Screen)) x r) :: Screen
        where regexMatches = matchRegex (mkRegex " x=([0-9]+) by ([0-9]+)") xs
              x = read ((fromJust regexMatches) !! 0)::Int
              r = read ((fromJust regexMatches) !! 1)::Int

processInstructions :: Screen -> [String] -> Screen
processInstructions screen [] = screen
processInstructions screen (instruction:instructions) = processInstructions (processInstruction screen instruction) instructions

main = do
  c <- getContents
  putStrLn . show $ processInstructions initScreen (lines c)
