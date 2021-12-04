import Numeric (readInt)
import Data.Char (digitToInt)

sampleInput =
  [ "00100"
  , "11110"
  , "10110"
  , "10111"
  , "10101"
  , "01111"
  , "00111"
  , "11100"
  , "10000"
  , "11001"
  , "00010"
  , "01010" ]

updateFreq :: Int -> (Int, Int) -> String -> (Int, Int)
updateFreq n (zeros, ones) s
  | s !! n == '0' = (zeros + 1, ones)
  | otherwise = (zeros, ones + 1)

getFreqs :: Int -> [String] -> (Int, Int)
getFreqs n = foldl (updateFreq n) (0, 0)

getCommonBit :: [String] -> Int -> Char
getCommonBit s n = if zeros > ones then '0' else '1'
  where
    (zeros, ones) = getFreqs n s

invertBit :: Char -> Char
invertBit '0' = '1'
invertBit '1' = '0'

invert :: String -> String
invert = map invertBit

readBinary :: String -> Int
readBinary = fst . head . readInt 2 (`elem` "01") digitToInt

rates :: [String] -> (Int, Int)
rates s = (readBinary gamma, readBinary $ invert gamma)
  where
    gamma = map (getCommonBit s) [0..((length $ s !! 0) - 1)]

part1 :: [String] -> Int
part1 = uncurry (*) . rates

filterPosition :: Char -> Int -> [String] -> [String]
filterPosition c i = filter ((== c) . (!! i))

stepFilterOxygen :: Int -> [String] -> [String]
stepFilterOxygen _ [a] = [a]
stepFilterOxygen i list = filterPosition common i list
  where
    common = getCommonBit list i

stepFilterCO2 :: Int -> [String] -> [String]
stepFilterCO2 _ [a] = [a]
stepFilterCO2 i list = filterPosition least i list
  where
    least = invertBit $ getCommonBit list i

findOxygen :: Int -> [String] -> String
findOxygen _ [a] = a
findOxygen i xs = findOxygen (i+1) (stepFilterOxygen i xs)

findCO2 :: Int -> [String] -> String
findCO2 _ [a] = a
findCO2 i xs = findCO2 (i+1) (stepFilterCO2 i xs)

findRatings :: [String] -> (Int, Int)
findRatings s = (readBinary $ findOxygen 0 s, readBinary $ findCO2 0 s)

part2 :: [String] -> Int
part2 = uncurry (*) . findRatings

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
