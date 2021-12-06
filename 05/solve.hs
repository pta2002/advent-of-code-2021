import qualified Data.Map as M
import Data.List.Split (splitOn)

type Vent = ((Int, Int), (Int, Int))

sampleInput :: [Vent]
sampleInput =
  [ ((0,9), (5,9))
  , ((8,0), (0,8))
  , ((9,4), (3,4))
  , ((2,2), (2,1))
  , ((7,0), (7,4))
  , ((6,4), (2,0))
  , ((0,9), (2,9))
  , ((3,4), (1,4))
  , ((0,0), (8,8))
  , ((5,5), (8,2)) ]

isDiagonal :: Vent -> Bool
isDiagonal ((x1,y1), (x2,y2)) = x1 /= x2 && y1 /= y2

pt1Filter :: [Vent] -> [Vent]
pt1Filter = filter (not . isDiagonal)

nsBetween :: Int -> Int -> [Int]
nsBetween x y
  | x == y = [x]
  | x < y = [x..y]
  | x > y = [x,x-1..y]

getPointsStraight :: Vent -> [(Int, Int)]
getPointsStraight ((x1,y1), (x2,y2)) =
  let xs = nsBetween x1 x2
      ys = nsBetween y1 y2
  in [(x,y) | x <- xs, y <- ys]

getPointsDiagonal :: Vent -> [(Int, Int)]
getPointsDiagonal ((x1,y1), (x2,y2)) =
  let xs = nsBetween x1 x2
      ys = nsBetween y1 y2
  in [(xs !! i, ys !! i) | i <- [0..length xs - 1]]

getPointsBetween :: Vent -> [(Int, Int)]
getPointsBetween v = if isDiagonal v then getPointsDiagonal v else getPointsStraight v

addPointToMap :: (Int, Int) -> M.Map (Int, Int) Int -> M.Map (Int, Int) Int
addPointToMap (x,y) = M.insertWith (+) (x,y) 1

addVentToMap :: M.Map (Int, Int) Int -> Vent -> M.Map (Int, Int) Int
addVentToMap m v = foldr addPointToMap m (getPointsBetween v)

part1 :: [Vent] -> Int
part1 = M.size . M.filter (>1) . foldl addVentToMap M.empty . pt1Filter

part2 :: [Vent] -> Int
part2 = M.size . M.filter (>1) . foldl addVentToMap M.empty

parseTuple :: String -> (Int, Int)
parseTuple s = (read x, read y)
  where
    [x,y] = splitOn "," s

parseVent :: String -> Vent
parseVent s = (parseTuple x, parseTuple y)
  where
    [x,y] = splitOn " -> " s

parseLines :: String -> [Vent]
parseLines = map parseVent . lines

main :: IO ()
main = do
  input <- parseLines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
