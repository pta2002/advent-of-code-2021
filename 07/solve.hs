import Data.List.Split
import Data.List (find)
import Data.Maybe (fromMaybe)

sampleInput :: [Int]
sampleInput = [16, 1, 2, 0, 4, 2, 7, 1, 2, 14]

totalCost :: Int -> [Int] -> Int
totalCost n = sum . map (abs . (-) n)

window :: [Int] -> [[Int]]
window (a:b:c:[]) = [[a,b,c]]
window (a:b:c:xs) = [a, b, c] : window (b:c:xs)

findLowest :: [Int] -> Int
findLowest = (!! 1) . fromMaybe undefined . find (\[a,b,c] -> b < a && b < c) . window

part1 :: [Int] -> Int
part1 i = findLowest $ map (flip totalCost i) [0..]

fuelCost :: Int -> Int -> Int
fuelCost x y = let n = abs $ x - y in n * (n + 1) `div` 2

totalCost2 :: Int -> [Int] -> Int
totalCost2 n = sum . map (fuelCost n)

part2 :: [Int] -> Int
part2 i = findLowest $ map (flip totalCost2 i) [0..]

parseInput :: String -> [Int]
parseInput = map read . splitOn ","

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

