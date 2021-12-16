data Command = Forward Int | Down Int | Up Int deriving (Eq, Show)
type Position = (Int, Int)
type Position2 = (Int, Int, Int)

sampleInput =
  [ Forward 5
  , Down 5
  , Forward 8
  , Up 3
  , Down 8
  , Forward 2 ]

stepForward :: Position -> Command -> Position
stepForward (x, y) (Forward n) = (x + n, y)
stepForward (x, y) (Down n) = (x, y + n)
stepForward (x, y) (Up n) = (x, y - n)

calculatePosition :: Position -> [Command] -> Position
calculatePosition = foldl stepForward

part1 :: [Command] -> Int
part1 = uncurry (*) . calculatePosition (0, 0)

stepForward2 :: Position2 -> Command -> Position2
stepForward2 (x, y, aim) (Forward n) = (x + n, y + aim * n, aim)
stepForward2 (x, y, aim) (Down n) = (x, y, aim + n)
stepForward2 (x, y, aim) (Up n) = (x, y, aim - n)

part2 :: [Command] -> Int
part2 = uncurry (*) . (\(a,b,c) -> (a,b)) . foldl stepForward2 (0, 0, 0)

parseLine :: String -> Command
parseLine s = case words s of
  ["forward", n] -> Forward (read n)
  ["down", n] -> Down (read n)
  ["up", n] -> Up (read n)
  _ -> error "Invalid input"

main :: IO ()
main = do
  input <- fmap parseLine . lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
