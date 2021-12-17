{-# LANGUAGE TupleSections #-}
import Control.Arrow
import Data.List.Split
import Debug.Trace
type Target = ((Int, Int), (Int, Int))

sampleInput :: Target
sampleInput = ((20, 30), (-10, -5))

-- We can make an equation to find out the position!
posy :: Integral a => a -> a -> a
posy i t = i * t - (t - 1) * t `div` 2

-- When we go up, we come back to start at the same velocity we went up.
-- So, we can just find the largest downward velocity which will go into the
-- target.

maxYVel :: Target -> Int
maxYVel (_, (a, b)) = -(min a b + 1)

minYVel :: Target -> Int
minYVel = (-1-) . maxYVel

maxY :: Int -> Int
maxY i = i * (i+1) `div` 2

maxXVel :: Target -> Int
maxXVel ((a, b), _) = max a b

part1 :: Target -> Int
part1 = maxY . maxYVel

stepX :: (Int, Int) -> (Int, Int)
stepX (0, x) = (0, x)
stepX (v, x) = (v - 1, x + v)

reachesTargetX :: Target -> (Int, Int) -> Bool
reachesTargetX ((a, b), _) (0, x)
    | x < min a b = False
    | x > max a b = False
    | otherwise = True
reachesTargetX t@((a, b), _) (v, x)
    | x >= min a b && x <= max a b = True
    | otherwise = reachesTargetX t $ stepX (v, x)

reachesTargetY :: Target -> (Int, Int) -> Bool
reachesTargetY t@(_, (a, b)) (v, y)
    | y < min a b = False
    | y >= min a b && y <= max a b = True
    | otherwise = reachesTargetY t (v - 1, y + v)

reachesTarget :: Target -> ((Int, Int), (Int, Int)) -> Bool
reachesTarget ((a, b), (c, d)) ((xv, x), (yv, y))
    | y < min c d = False
    | x > max a b = False
    | x >= min a b && x <= max a b && y >= min c d && y <= max c d = True
    | otherwise = reachesTarget ((a, b), (c, d)) (stepX (xv, x), (yv - 1, y + yv))

possibleSolutions :: Target -> [(Int, Int)]
possibleSolutions t = [(x,y) | x <- validXVels t, y <- [minYVel t..maxYVel t]]

validXVels :: Target -> [Int]
validXVels t = filter (reachesTargetX t . (,0)) r
    where r = [0..maxXVel t]

vReachesTarget :: Target -> (Int, Int) -> Bool
vReachesTarget t (a, b) = reachesTarget t ((a, 0), (b, 0))

part2 :: Target -> Int
part2 t = length $ filter (vReachesTarget t) $ possibleSolutions t

parseRange :: String -> (Int, Int)
parseRange = (read . head &&& read . last) . splitOn ".."

parseTarget :: String -> Target
parseTarget = (parseRange *** parseRange) . (drop 2 . init . head &&& drop 2 . last) . tail . tail . words

main :: IO ()
main = do
    input <- parseTarget <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)