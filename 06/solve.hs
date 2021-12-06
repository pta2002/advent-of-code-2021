import qualified Data.Map as Map
import Data.List.Split

sampleInput :: Map.Map Int Int
sampleInput = Map.fromList [(3,2), (4,1), (1,1), (2, 1)]

nextStep :: (Int, Int) -> Map.Map Int Int
nextStep (0, y) = Map.fromList [(6, y), (8, y)]
nextStep (x, y) = Map.fromList [(x - 1, y)]

combine :: Map.Map Int Int -> Map.Map Int Int -> Map.Map Int Int
combine = Map.unionWith (+)

stepInput :: Map.Map Int Int -> Map.Map Int Int
stepInput = foldl combine Map.empty . map nextStep . Map.toList

totalFish :: Map.Map Int Int -> Int
totalFish = sum . Map.elems

part1 :: Map.Map Int Int -> Int
part1 = totalFish . (!! 80) . iterate stepInput

part2 :: Map.Map Int Int -> Int
part2 = totalFish . (!! 256) . iterate stepInput

parseInput :: String -> Map.Map Int Int
parseInput =
  foldl combine Map.empty .
  map (\x -> Map.fromList $ (x, 1):[]) .
  map read .
  splitOn ","

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)

