import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow

type Paper = S.Set (Int, Int) -- X, Y
data Fold = FoldY Int | FoldX Int deriving (Show, Eq)

sampleInput :: Paper
sampleInput = S.fromList
    [ (6,10), (0,14), (9,10), (0,3), (10,4)
    , (4,11), (6,0), (6,12), (4,1), (0,13)
    , (10,12), (3,4), (3,0), (8,4), (1,10)
    , (2,14), (8,10), (9,0)]

sampleInputFolds :: [Fold]
sampleInputFolds =  [FoldY 7, FoldX 5]

foldPoint :: Fold -> (Int, Int) -> (Int, Int)
foldPoint (FoldY n) (x, y) = (x, min y $ 2 * n - y)
foldPoint (FoldX n) (x, y) = (min x $ 2 * n - x, y)

filterAtFold :: Fold -> Paper -> (Paper, Paper)
filterAtFold (FoldX n) = S.partition (\(x, y) -> x < n)
filterAtFold (FoldY n) = S.partition (\(x, y) -> y < n)

executeFold :: Fold -> Paper -> Paper
executeFold f = uncurry S.union . second (S.map (foldPoint f)) . filterAtFold f

part1 :: (Paper, [Fold]) -> Int
part1 (p, fs) = S.size $ executeFold (head fs) p

readFold :: (Char, String) -> Fold
readFold ('y', n) = FoldY (read n)
readFold ('x', n) = FoldX (read n)
readFold _ = error "Invalid fold"

simplifyFold :: String -> (Char, String)
simplifyFold = (head *** tail) . break (== '=') . last . words

parseFold :: String -> Fold
parseFold = readFold . simplifyFold

parsePoint :: String -> (Int, Int)
parsePoint = (read *** read . tail) . break (== ',')

parseInput :: String -> (Paper, [Fold])
parseInput = (S.fromList . map parsePoint *** map parseFold . tail) . break (== "") . lines

showPaperLine :: [Int] -> String
showPaperLine [] = ""
showPaperLine xs = map (\x -> if x `elem` xs then block else ' ') [0..maximum xs]
    where block = '█'

paperLines :: Paper -> [Int]
paperLines = S.toList . S.map snd

paperLineList :: Paper -> [(Int, [Int])]
paperLineList p = map (second (S.toList . S.map fst) . (\y -> (y, S.filter (\x -> snd x == y) p))) (paperLines p)

showPaper :: [(Int, [Int])] -> [String]
showPaper p = map showPaperLine myLines
    where
        p' = M.fromList p
        myLines = map (\x -> M.findWithDefault [] x p') [0..maximum (map fst p)]

part2 :: (Paper, [Fold]) -> String
part2 (p, fs) = unlines $ showPaper $ paperLineList $ foldr executeFold p (reverse fs)

main :: IO ()
main = do
    input <- parseInput <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2:\n" ++ part2 input