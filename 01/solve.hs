part1 :: [Int] -> Int
part1 = snd . foldl help (9999999999,0)
  where
    help (prev, sum) x = (x, if x > prev then sum + 1 else sum)

slidingWindow :: [Int] -> [Int]
slidingWindow (a:b:c:t) = (a+b+c) : slidingWindow (b:c:t)
slidingWindow _ = []

part2 :: [Int] -> Int
part2 = part1 . slidingWindow

main :: IO ()
main = do
  input <- (fmap (read :: String -> Int)) <$> lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
