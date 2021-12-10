{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.Maybe
import Data.List
import Control.Arrow
import Data.Char (digitToInt)
import Control.Monad.State.Lazy
import Debug.Trace
import GHC.IO (unsafePerformIO)

type VentMap = M.Map (Int, Int) Int

sampleInput :: VentMap
sampleInput = parseLines "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"

getAdjacent :: VentMap -> (Int, Int) -> (Int, [Int])
getAdjacent m point@(x, y) = (fromJust $ M.lookup point m, adjacents)
  where
    adjacents = fromJust <$> filter isJust [adjacent1, adjacent2, adjacent3, adjacent4]
    adjacent1 = M.lookup (x-1, y) m
    adjacent2 = M.lookup (x+1, y) m
    adjacent3 = M.lookup (x, y+1) m
    adjacent4 = M.lookup (x, y-1) m

isLowPoint :: (Int, [Int]) -> Bool
isLowPoint (val, vals) = all (>val) vals

riskLevel :: Int -> Int
riskLevel = (+1)

part1 :: VentMap -> Int
part1 m = sum $ map (riskLevel . snd) (M.toList $ M.filterWithKey (\k _ -> isLowPoint $ getAdjacent m k) m)

hasKey :: Ord a => M.Map a b -> a -> Maybe a
hasKey m k = if isJust $ M.lookup k m then Just k else Nothing

getAdjacentPoints :: VentMap -> (Int, Int) -> [(Int, Int)]
getAdjacentPoints m (x, y) = catMaybes pts
  where
    pts = hasKey m <$> [(x,y), adjacent1, adjacent2, adjacent3, adjacent4]
    adjacent1 = (x - 1, y)
    adjacent2 = (x + 1, y)
    adjacent3 = (x, y - 1)
    adjacent4 = (x, y + 1)

takeAdjacents :: (Int, Int) -> State VentMap [(Int, Int)]
takeAdjacents pt = do
  map <- get
  let adjacent = filter ((/= Just 9) . flip M.lookup map) (getAdjacentPoints map pt)
  put $ foldr M.delete map adjacent
  return adjacent

takePoints :: (Int, Int) -> State VentMap [(Int, Int)]
takePoints pt = do
  map <- get
  pts <- takeAdjacents pt
  adjs <- mapM takePoints pts
  return $ pts ++ concat adjs

takeBasin :: State VentMap Int
takeBasin = do
  map <- get

  let startingPoint = fst $ M.findMin $ M.filter (<9) map
  points <- takePoints startingPoint

  return $ length points

takeBasins :: State VentMap [Int]
takeBasins = do
  map <- get
  if M.size map == M.size (M.filter (==9) map) then
    return []
  else do
    basin <- takeBasin
    basins <- takeBasins
    return $ basin:basins

getBasins :: VentMap -> [Int]
getBasins = evalState takeBasins

getMax3 :: [Int] -> [Int]
getMax3 = take 3 . sortOn (0-)

part2 :: VentMap -> Int
part2 = product . getMax3 . getBasins

-- Parsing, etc.
parseLine :: Int -> String -> [((Int, Int), Int)]
parseLine i = map (second digitToInt) . zipWith (curry $ first (,i)) [0..]

parseLines :: String -> VentMap
parseLines = M.fromList . concat . zipWith parseLine [0..] . lines

main :: IO ()
main = do
  input <- parseLines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)