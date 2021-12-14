import Data.Char (isLower)
import qualified Data.Map as M
import Data.Maybe
import qualified Data.Set as S

data Cave = Small String | Large String | Start | End deriving (Eq, Show, Ord)
type CaveMap = M.Map Cave [Cave]

-- Parsing

parseCave :: String -> Cave
parseCave "start" = Start
parseCave "end" = End
parseCave n@(s:ss)
    | isLower s = Small n
    | otherwise = Large n
parseCave [] = error "Empty string"

parseConnection :: String -> (Cave, Cave)
parseConnection s = (parseCave a, parseCave $ tail b)
    where (a, b) = break (== '-') s

addToMap :: CaveMap -> (Cave, Cave) -> CaveMap
addToMap m (a, b) = M.insertWith (++) a [b] $ M.insertWith (++) b [a] m

parseCaveMap :: String -> CaveMap
parseCaveMap = foldl (\m l -> addToMap m (parseConnection l)) M.empty . lines

sampleInput1 :: CaveMap
sampleInput1 = parseCaveMap $ unlines
    [ "start-A"
    , "start-b"
    , "A-c"
    , "A-b"
    , "b-d"
    , "A-end"
    , "b-end" ]

allPaths :: CaveMap -> Cave -> [Cave] -> [[Cave]]
allPaths m c skip = concatMap (map (c:)) (mapMaybe helper (m M.! c))
    where
        helper :: Cave -> Maybe [[Cave]]
        helper Start = Nothing
        helper End = Just [[End]]
        helper cave = if cave `elem` skip then Nothing else case cave of
            Small _ -> Just $ allPaths m cave (cave:skip)
            Large _ -> Just $ allPaths m cave skip
            _ -> undefined

part1 :: CaveMap -> Int
part1 m = length $ allPaths m Start []

allPaths2 :: CaveMap -> Cave -> Maybe Cave -> S.Set Cave -> [[Cave]]
allPaths2 m c small skip = concatMap (map (c:)) (mapMaybe helper (m M.! c))
    where
        helper :: Cave -> Maybe [[Cave]]
        helper Start = Nothing
        helper End = Just [[End]]
        helper cave = if S.member cave skip then Nothing else case cave of
            Small _ -> if isJust small
                then Just $ allPaths2 m cave small (S.insert cave skip)
                else Just $ allPaths2 m cave (Just cave) skip ++ allPaths2 m cave Nothing (S.insert cave skip)
            Large _ -> Just $ allPaths2 m cave small skip
            _ -> undefined

showCaves :: [Cave] -> String
showCaves = concatMap $ (++ "->") . showCave
    where
        showCave :: Cave -> String
        showCave c = case c of
            Small s -> s
            Large s -> s
            Start -> "start"
            End -> "end"

-- Not proud of this but idk why I'm getting repeated stuff
part2 :: CaveMap -> Int
part2 m = S.size $ S.fromList $ allPaths2 m Start Nothing S.empty

main :: IO ()
main = do
    input <- parseCaveMap <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)