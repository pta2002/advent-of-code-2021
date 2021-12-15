import qualified Data.Map as M
import Data.Char (isLetter)
import Control.Arrow

type Rule = ((Char, Char), Char)
type Polymer = M.Map (Char, Char) Int

sampleInputPolymer :: Polymer
sampleInputPolymer = M.fromList [(('N', 'N'), 1), (('N', 'C'), 1), (('C', 'B'), 1)]

sampleInputRules :: [Rule]
sampleInputRules =
    [ (('C', 'H'), 'B'), (('H', 'H'), 'N'), (('C', 'B'), 'H')
    , (('N', 'H'), 'C'), (('H', 'B'), 'C'), (('H', 'C'), 'B')
    , (('H', 'N'), 'C'), (('N', 'N'), 'C'), (('B', 'H'), 'H')
    , (('N', 'C'), 'B'), (('N', 'B'), 'B'), (('B', 'N'), 'B')
    , (('B', 'B'), 'N'), (('B', 'C'), 'B'), (('C', 'C'), 'N')
    , (('C', 'N'), 'C') ]

stepRule :: Rule -> [(Char, Char)]
stepRule ((a, b), c) = [(a, c), (c, b)]

noRules :: [Rule] -> Polymer -> Polymer
noRules rs = M.filterWithKey (\k a -> not $ M.member k $ M.fromList rs)

stepRules :: [Rule] -> Polymer -> Polymer
stepRules rs p = foldr (M.unionWith (+) . insertRule p) (noRules rs p) rs

iterateRules :: Int -> [Rule] -> Polymer -> Polymer
iterateRules i rs ps = (!! i) $ iterate (stepRules rs) ps

insertRule :: Polymer -> Rule -> Polymer
insertRule p r
    | M.member (fst r) p = foldl (\m a -> M.insertWith (+) a (p M.! fst r) m) M.empty $ stepRule r
    | otherwise = M.empty

parsePolymer :: String -> Polymer
parsePolymer [] = M.empty
parsePolymer [x] = M.empty
parsePolymer (a:b:xs) = M.insertWith (+) (a, b) 1 $ parsePolymer (b:xs)

parseRule :: String -> Rule
parseRule s = ((c1, c2), c3)
    where
        [c1, c2, c3] = filter isLetter s

parseRules :: String -> ([Rule], Polymer)
parseRules = (map parseRule *** parsePolymer) . (tail . tail &&& head) . lines

letterFreq :: Polymer -> M.Map Char Int
letterFreq = M.map helper . M.fromListWith (+) . concatMap (\((a, b), i) -> [(a, i), (b, i)]) . M.toList
    where
        helper x
            | odd x = x `div` 2 + 1
            | otherwise = x `div` 2

part :: Int -> [Rule] -> Polymer -> Int
part i r p = maximum l - minimum l
    where l = map snd $ M.toList $ letterFreq $ iterateRules i r p

main :: IO ()
main = do
    (rules, polymer) <- parseRules <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part 10 rules polymer)
    putStrLn $ "Part 2: " ++ show (part 40 rules polymer)