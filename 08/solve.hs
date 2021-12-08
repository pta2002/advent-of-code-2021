import Control.Arrow
import qualified Data.Map as M
import Data.List
import Control.Monad.State.Lazy
import Data.Maybe

data Segment = A | B | C | D | E | F | G deriving (Eq, Show, Ord)
type Note = ([[Segment]], [[Segment]])

sampleNote = parseLine "acedgfb cdfbe gcdfa fbcad dab cefabd cdfgeb eafb cagedb ab | cdfeb fcadb cdfeb cdbaf"

parseSegment :: Char -> Segment
parseSegment 'a' = A
parseSegment 'b' = B
parseSegment 'c' = C
parseSegment 'd' = D
parseSegment 'e' = E
parseSegment 'f' = F
parseSegment 'g' = G

parseSegments :: String -> [Segment]
parseSegments = map parseSegment

parseSegmentList :: String -> [[Segment]]
parseSegmentList = map parseSegments . words

parseLine :: String -> Note
parseLine = (parseSegmentList *** parseSegmentList) . (second $ filter (/= '|')) . span (/= '|')

part1 :: [Note] -> Int
part1 = length . filter ((`elem` [2,3,4,7]) . length) . concat . map snd

-- Part 2
occurrences :: Eq a => a -> [a] -> Int
occurrences x = length . filter (== x)

byOccurrences :: Int -> [Segment]
byOccurrences 4 = [E]
byOccurrences 6 = [B]
byOccurrences 7 = [D, G]
byOccurrences 8 = [A, C]
byOccurrences 9 = [F]

type SegmentMap = M.Map Segment Segment

split :: a -> (a, a)
split a = (a, a)

allSegments = [A, B, C, D, E, F, G]

singleMaybe :: [a] -> Maybe a
singleMaybe [x] = Just x
singleMaybe _ = Nothing

justOccurrences :: [[Segment]] -> [(Segment, Int)]
justOccurrences segs = second (flip occurrences $ concat segs) . split <$> allSegments

flipList :: [(a, Maybe b)] -> [(b, a)]
flipList [] = []
flipList ((a, Just b):xs) = (b, a) : flipList xs
flipList ((a, Nothing):xs) = flipList xs

occurrenceMap :: [[Segment]] -> SegmentMap
occurrenceMap s = M.fromList $ flipList $ (id *** singleMaybe . byOccurrences) <$> justOccurrences s

initialState :: State ([[Segment]], SegmentMap) ()
initialState = do
  (segs, _) <- get
  put $ second occurrenceMap $ split segs

getA :: State ([[Segment]], SegmentMap) ()
getA = do
  (segs, occs) <- get
  let seven = head $ filter ((==3) . length) segs
  let one = head $ filter ((==2) . length) segs
  let a = head $ seven \\ one
  put (segs, M.insert A a occs)

getC :: State ([[Segment]], SegmentMap) ()
getC = do
  (segs, occs) <- get
  let one = head $ filter ((==2) . length) segs
  let c = head $ filter (\a -> (==0) . M.size $ M.filter (==a) occs) one
  put (segs, M.insert C c occs)

getD :: State ([[Segment]], SegmentMap) ()
getD = do
  (segs, occs) <- get
  let four = head $ filter ((==4) . length) segs
  let d = head $ filter (\a -> (==0) . M.size $ M.filter (==a) occs) four
  put (segs, M.insert D d occs)

getG :: State ([[Segment]], SegmentMap) ()
getG = do
  (segs, occs) <- get
  let values = snd <$> M.toList occs
  let remaining = head $ allSegments \\ values
  put (segs, M.insert G remaining occs)

getMap :: State ([[Segment]], SegmentMap) () -> [[Segment]] -> SegmentMap
getMap s = snd . execState s . (id *** const M.empty) . split

solveSegments :: [[Segment]] -> SegmentMap
solveSegments = getMap $ initialState >> getA >> getC >> getD >> getG

mapSegments :: SegmentMap -> [Segment] -> [Segment]
mapSegments m = sort . map (fromJust . flip M.lookup m)

segmentsToNumber :: [Segment] -> Char
segmentsToNumber [A,B,C,E,F,G] = '0'
segmentsToNumber [C,F] = '1'
segmentsToNumber [A,C,D,E,G] = '2'
segmentsToNumber [A,C,D,F,G] = '3'
segmentsToNumber [B,C,D,F] = '4'
segmentsToNumber [A,B,D,F,G] = '5'
segmentsToNumber [A,B,D,E,F,G] = '6'
segmentsToNumber [A,C,F] = '7'
segmentsToNumber [A,B,C,D,E,F,G] = '8'
segmentsToNumber [A,B,C,D,F,G] = '9'
segmentsToNumber s = error $ "Invalid segments: " ++ show s

flipMap :: Ord b => M.Map a b -> M.Map b a
flipMap = M.fromList . map (snd &&& fst) . M.toList

decodeNumber :: SegmentMap -> [Segment] -> Char
decodeNumber m = segmentsToNumber . mapSegments (flipMap m)

decodeNote :: Note -> Int
decodeNote (segs, nums) = read $ map (decodeNumber m) nums
  where m = solveSegments segs

part2 :: [Note] -> Int
part2 = sum . map decodeNote

main :: IO ()
main = do
  input <- fmap parseLine <$> lines <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)
