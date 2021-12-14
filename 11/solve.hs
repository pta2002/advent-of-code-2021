{-# LANGUAGE TupleSections #-}

{-# LANGUAGE FlexibleInstances #-}
import qualified Data.Map as M
import Data.Char
import Control.Arrow
import Data.Maybe (catMaybes)
import Control.Monad.State.Lazy
import qualified Control.Arrow as M
import Debug.Trace (trace, traceShowM)
import Data.List (sort, groupBy)

data Octopus = Blinked | Powering Int deriving (Eq, Ord)
type OctoGrid = M.Map (Int, Int) Octopus

newtype WrappedOcto = WO OctoGrid

instance Show Octopus where
  show Blinked = "."
  show (Powering n) = show n

instance Show WrappedOcto where
  show (WO g) = unlines . map (concatMap (show . snd)) . groupBy (\((a,_),_) ((b,_),_) -> a == b) . sort . map (\((a,b), c) -> ((b,a), c)) . M.toList $ g

-- Sample Input
sampleInput :: OctoGrid
sampleInput = parseLines "5483143223\n\
\2745854711\n\
\5264556173\n\
\6141336146\n\
\6357385478\n\
\4167524645\n\
\2176841721\n\
\6882881134\n\
\4846848554\n\
\5283751526"

sampleInput2 :: OctoGrid
sampleInput2 = parseLines "11111\n\
\19991\n\
\19191\n\
\19991\n\
\11111"

printState :: State OctoGrid a -> OctoGrid -> IO ()
printState s = print . WO . execState s

-- Part 1
stepOctopus :: Octopus -> Octopus
stepOctopus Blinked = Blinked
stepOctopus (Powering n) = Powering (n + 1)

stepOctopus2 :: Octopus -> Octopus
stepOctopus2 Blinked = Powering 1
stepOctopus2 (Powering n) = Powering (n + 1)

bringOutMaybe :: ((a, b), Maybe c) -> Maybe ((a, b), c)
bringOutMaybe ((a, b), Nothing) = Nothing
bringOutMaybe ((a, b), Just c) = Just ((a, b), c)

getAdjacent :: M.Map (Int, Int) Octopus -> (Int, Int) -> [((Int, Int), Octopus)]
getAdjacent grid (x, y) = catMaybes $ bringOutMaybe <$>
  [ ((x + dx, y + dy), M.lookup (x + dx, y + dy) grid)
  | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0
  ]

shouldBlink :: Octopus -> Bool
shouldBlink (Powering n) = n > 9
shouldBlink _ = False

blinkOne :: (Int, Int) -> State OctoGrid ()
blinkOne (x, y) = do
  grid <- get

  if grid M.! (x,y) == Blinked
    then return ()
    else do
      let adjacent = map (second stepOctopus) $ filter ((/= Blinked) . snd) $ getAdjacent grid (x, y)
      modify $ M.insert (x, y) Blinked
      mapM_ (\((x, y), o) -> modify (M.insert (x, y) o)) adjacent

      let blinkable = filter (shouldBlink . snd) adjacent
      mapM_ (blinkOne . fst) blinkable

step :: State OctoGrid Int
step = do
  modify $ M.map stepOctopus2
  grid <- get
  let filtered = M.filter shouldBlink grid
  mapM_ (blinkOne . fst) $ M.toList filtered

  M.size . M.filter (== Blinked) <$> get

stepN :: Int -> State OctoGrid Int
stepN n = sum <$> replicateM n step

part1 :: OctoGrid -> Int
part1 = evalState (stepN 100)

-- Part 2
findSimultaneous :: State OctoGrid Int
findSimultaneous = do
  grid <- get
  let nBlinks = M.size $ M.filter (== Blinked) grid
  if nBlinks == M.size grid then return 0
  else succ <$> (step >> findSimultaneous)

part2 :: OctoGrid -> Int
part2 = evalState findSimultaneous

-- Parsing
parseLine :: Int -> String -> [((Int, Int), Octopus)]
parseLine i = map (second $ Powering . digitToInt) . zipWith (curry $ first (,i)) [0..]

parseLines :: String -> OctoGrid
parseLines = M.fromList . concat . zipWith parseLine [0..] . lines

main :: IO ()
main = do
  input <- parseLines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
