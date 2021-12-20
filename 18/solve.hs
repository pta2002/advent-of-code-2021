import Data.Maybe
import Text.Parsec

data Number = Value Int | Pair Number Number deriving (Show, Eq)

magnitude :: Number -> Int
magnitude (Value n) = n
magnitude (Pair n1 n2) = 3 * magnitude n1 + 2 * magnitude n2

canExplode :: Int -> Number -> Maybe [Int]
canExplode nest (Value _) = Nothing
canExplode nest (Pair n1 n2)
  | nest < 4 = if isJust l then (0:) <$> l else (1:) <$> r
  | otherwise = Just []
    where
      l = canExplode (nest + 1) n1
      r = canExplode (nest + 1) n2

navigateTo :: [Int] -> Number -> Number
navigateTo (0:xs) (Pair n1 _) = navigateTo xs n1
navigateTo (1:xs) (Pair _ n2) = navigateTo xs n2
navigateTo _ n = n

addLeft :: Int -> Number -> Number
addLeft i (Value n) = Value $ n + i
addLeft i (Pair n1 n2) = Pair (addLeft i n1) n2

addRight :: Number -> Int -> Number
addRight (Value n) i = Value $ n + i
addRight (Pair n1 n2) i = Pair n1 (addRight n2 i)

explode :: Int -> Number -> (Bool, Number, Int, Int)
explode d s@(Value n) = (False, s, 0, 0)
explode d s@(Pair (Value a) (Value b)) = if d >= 4 then (True, Value 0, a, b) else (False, s, 0, 0)
explode d s@(Pair a b) = let
    (l_exploded, l, ll, lr) = explode (d+1) a
    (r_exploded, r, rl, rr) = explode (d+1) b
  in if l_exploded then (True, Pair l (addLeft lr b), ll, 0)
     else if r_exploded then (True, Pair (addRight a rl) r, 0, rr)
     else (False, s, 0, 0)

split :: Number -> (Bool, Number)
split (Value n)
  | n >= 10 = let
      a = n `div` 2
      b = a + n `mod` 2
    in (True, Pair (Value a) (Value b))
  | otherwise = (False, Value n)
split (Pair n1 n2) = let
    (l_split, l) = split n1
    (r_split, r) = split n2
  in if l_split then (True, Pair l n2)
     else if r_split then (True, Pair n1 r)
     else (False, Pair n1 n2)

reduceNumber' :: Number -> (Bool, Number)
reduceNumber' n = let
    (exploded, en, _, _) = explode 0 n
    (splitted, sn) = split en
  in if exploded then (True, en)
     else if splitted then (True, sn)
     else (False, n)

reduce :: Number -> Number
reduce n = if r then reduce rn else rn
  where
    (r, rn) = reduceNumber' n

add :: Number -> Number -> Number
add a b = reduce $ Pair a b

part1 :: [Number] -> Int
part1 = magnitude . foldl1 add

part2 :: [Number] -> Int
part2 ns = maximum . map magnitude $ concat $ [[add a b, add b a] | a <- ns, b <- ns, a /= b]

showNumber :: Number -> String
showNumber (Value n) = show n
showNumber (Pair n1 n2) = "[" ++ showNumber n1 ++ "," ++ showNumber n2 ++ "]"

number :: Parsec String st Number
number = do
  n <- many1 digit
  return $ Value (read n)

pair :: Parsec String st Number
pair = do
  char '['
  n1 <- snailfishNum
  char ','
  n2 <- snailfishNum
  char ']'
  return $ Pair n1 n2

snailfishNum :: Parsec String st Number
snailfishNum = number <|> pair

parseNumber :: String -> Number
parseNumber = either (error . show) id . parse snailfishNum ""

main :: IO ()
main = do
  input <- map parseNumber . lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)