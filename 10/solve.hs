import Data.Either ( fromRight, isRight )
import Data.List ( sort )
import qualified Data.Map as M

data Brace = Opening Char | Closing Char deriving (Show, Eq, Ord)

sampleInput =
  [ "[({(<(())[]>[[{[]{<()<>>",
    "[(()[<>])]({[<{<<[]>>(",
    "{([(<{}[<>[]}>{[]{[(<()>",
    "(((({<>}<{<{<>}{[]{[]{}",
    "[[<[([]))<([[{}[[()]]]",
    "[{[{({}]{}}([{[{{{}}([]",
    "{<[[]]>}<{[{[{[]{()[[[]",
    "[<(<(<(<{}))><([]([]()",
    "<{([([[(<>()){}]>(<<{{",
    "<{([{{}}[<[[[<>{}]]]>[]]"
  ]

toBrace :: Char -> Brace
toBrace '<' = Opening '<'
toBrace '(' = Opening '('
toBrace '{' = Opening '{'
toBrace '[' = Opening '['
toBrace x = Closing x

fromBrace :: Brace -> Char
fromBrace (Opening c) = c
fromBrace (Closing c) = c

matching :: Brace -> Brace
matching (Opening '{') = Closing '}'
matching (Opening '[') = Closing ']'
matching (Opening '<') = Closing '>'
matching (Opening '(') = Closing ')'
matching (Closing '}') = Opening '{'
matching (Closing ']') = Opening '['
matching (Closing '>') = Opening '<'
matching (Closing ')') = Opening '('
matching _ = error "Not a brace"

type Braces = [Brace]

headIs :: Eq a => [a] -> a -> Bool
headIs (a : xs) b = a == b
headIs _ _ = False

getBrace :: Char -> Braces -> Either Int Braces
getBrace x m =
  let brace = toBrace x
   in case brace of
        Opening _ -> Right $ brace : m
        Closing _ ->
          if headIs m (matching brace)
            then Right $ tail m
            else Left 0

getBraces :: String -> Braces -> Either (Char, Int) Braces
getBraces [] m = Right m
getBraces (x : xs) m = case getBrace x m of
  Left i -> Left (x, i)
  Right bs -> case getBraces xs bs of
    Left (x, i) -> Left (x, i + 1)
    Right bs' -> Right bs'

getErrorScore :: Char -> Int
getErrorScore ')' = 3
getErrorScore ']' = 57
getErrorScore '}' = 1197
getErrorScore '>' = 25137
getErrorScore _ = error "Not a bracket"

getLineScore :: String -> Int
getLineScore s = case getBraces s [] of
  Left (x, i) -> getErrorScore x
  Right _ -> 0

part1 :: [String] -> Int
part1 = sum . map getLineScore

-- Part 2
getIncompleteLines :: [String] -> [Braces]
getIncompleteLines = map (fromRight (error "No!")) . filter isRight . map (`getBraces` [])

closeBraces :: Braces -> Braces
closeBraces = map matching

closeLines :: [String] -> [String]
closeLines = map (map fromBrace . closeBraces) <$> getIncompleteLines

autocompleteCharScore :: Char -> Int
autocompleteCharScore ')' = 1
autocompleteCharScore ']' = 2
autocompleteCharScore '}' = 3
autocompleteCharScore '>' = 4
autocompleteCharScore _ = error "Not a bracket"

autocompleteScore :: String -> Int
autocompleteScore [] = 0
autocompleteScore (x : xs) = autocompleteCharScore x + 5 * autocompleteScore xs

middle :: [a] -> a
middle s = s !! (length s `div` 2)

part2 :: [String] -> Int
part2 s = middle $ sort $ autocompleteScore <$> (reverse <$> closeLines s)

main :: IO ()
main = do
  input <- lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)