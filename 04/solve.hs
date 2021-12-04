import Data.List.Split (splitOn)

data BingoNumber = Taken Int | Untaken Int deriving (Show, Eq)
type BingoBoard = [[BingoNumber]]

isTaken :: BingoNumber -> Bool
isTaken (Taken _) = True
isTaken _ = False

getNumber :: BingoNumber -> Int
getNumber (Taken i) = i
getNumber (Untaken i) = i

board1 =
  [ [22, 13, 17, 11, 0]
  , [8, 2, 23, 4, 24]
  , [21, 9, 14, 16, 7]
  , [6, 10, 3, 18, 5]
  , [1, 12, 20, 15, 19] ]

board2 =
  [ [3, 15, 0, 2, 22]
  , [9, 18, 13, 17, 5]
  , [19, 8, 7, 25, 23]
  , [20, 11, 10, 24, 4]
  , [14, 21, 16, 12, 6] ]

board3 =
  [ [14, 21, 17, 24, 4]
  , [10, 16, 15, 9, 19]
  , [18, 8, 23, 26, 20]
  , [22, 11, 13, 6, 5]
  , [2, 0, 12, 3, 7] ]

sampleInputNumbers :: [Int]
sampleInputNumbers = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
sampleInputBoards =
  [ (map Untaken) <$> board1
  , (map Untaken) <$> board2
  , (map Untaken) <$> board3 ]

takeNumber :: Int -> BingoNumber -> BingoNumber
takeNumber _ (Taken n) = Taken n
takeNumber n (Untaken t)
  | n == t = Taken t
  | otherwise = Untaken t

takeBoard :: Int -> BingoBoard -> BingoBoard
takeBoard i = map $ map (takeNumber i)

checkRow :: BingoBoard -> Bool
checkRow = any (all isTaken)

transposeBoard :: BingoBoard -> BingoBoard
transposeBoard board = map (\i -> map (!! i) board) [0..4]

checkColumn :: BingoBoard -> Bool
checkColumn = checkRow . transposeBoard

checkBoard :: BingoBoard -> Bool
checkBoard b = checkRow b || checkColumn b

getUnmarkedSum :: BingoBoard -> Int
getUnmarkedSum = sum . map getNumber . concat . map (filter (not . isTaken))

getScore :: Int -> BingoBoard -> Int
getScore i b = i * (getUnmarkedSum b)

stepBoard :: Int -> BingoBoard -> Either Int BingoBoard
stepBoard i b = if checkBoard p then Left (getScore i p) else Right p
  where
    p = takeBoard i b

stepBoards :: Int -> [BingoBoard] -> Either Int [BingoBoard]
stepBoards i = traverse (stepBoard i)

part1 :: [Int] -> [BingoBoard] -> Either Int [BingoBoard]
part1 (h:t) = helper . stepBoards h
  where
    helper (Right bs) = part1 t bs
    helper e = e

-- Part 2
removeWinners :: [BingoBoard] -> [BingoBoard]
removeWinners = filter (not . checkBoard)

stepBoards2 :: Int -> [BingoBoard] -> [BingoBoard]
stepBoards2 i = removeWinners . map (takeBoard i)

-- Oh god, this is an absolute mess, 100% not proud of this, but it works
getLastBoard :: [Int] -> [BingoBoard] -> (Int, BingoBoard)
getLastBoard (h:t) b =
  case stepBoards2 h b of
    [w] -> if stepBoards2 (head t) [w] == [] then (head t, takeBoard (head t) w) else getLastBoard t [w]
    bs -> getLastBoard t bs

part2 :: [Int] -> [BingoBoard] -> Int
part2 a b = uncurry getScore (getLastBoard a b)

-- Parsing, etc

parseList :: String -> [Int]
parseList s = read <$> splitOn "," s

parseBoard :: String -> BingoBoard
parseBoard s = fmap (Untaken <$> read) <$> words <$> lines s

parseInput :: String -> ([Int], [BingoBoard])
parseInput i = (parseList h, parseBoard <$> t)
  where
    (h:t) = splitOn "\n\n" i

main :: IO ()
main = do
  input <- parseInput <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ (show $ (uncurry part1) input)
  putStrLn $ "Part 2: " ++ (show $ (uncurry part2) input)
