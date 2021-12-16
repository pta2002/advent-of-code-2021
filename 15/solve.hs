import qualified Data.Set as S
import qualified Data.Map as M
import Control.Arrow
import Data.IntMap (mapMaybe)
import Data.Foldable (minimumBy, maximumBy)
import Data.Maybe (catMaybes, fromJust)
import Data.Function (on)
import Data.Char (digitToInt)
import Debug.Trace
import Data.Heap (MinHeap, viewHead, viewTail, null, MinPrioHeap)
import qualified Data.Heap as H

type CaveMap = M.Map (Int, Int) Int

sampleInput :: CaveMap
sampleInput = parseMap $ unlines
  [ "1163751742"
  , "1381373672"
  , "2136511328"
  , "3694931569"
  , "7463417111"
  , "1319128137"
  , "1359912421"
  , "3125421639"
  , "1293138521"
  , "2311944581" ]

sampleInput2 :: CaveMap
sampleInput2 = parseMap $ unlines
  [ "11637517422274862853338597396444961841755517295286"
  , "13813736722492484783351359589446246169155735727126"
  , "21365113283247622439435873354154698446526571955763"
  , "36949315694715142671582625378269373648937148475914"
  , "74634171118574528222968563933317967414442817852555"
  , "13191281372421239248353234135946434524615754563572"
  , "13599124212461123532357223464346833457545794456865"
  , "31254216394236532741534764385264587549637569865174"
  , "12931385212314249632342535174345364628545647573965"
  , "23119445813422155692453326671356443778246755488935"
  , "22748628533385973964449618417555172952866628316397"
  , "24924847833513595894462461691557357271266846838237"
  , "32476224394358733541546984465265719557637682166874"
  , "47151426715826253782693736489371484759148259586125"
  , "85745282229685639333179674144428178525553928963666"
  , "24212392483532341359464345246157545635726865674683"
  , "24611235323572234643468334575457944568656815567976"
  , "42365327415347643852645875496375698651748671976285"
  , "23142496323425351743453646285456475739656758684176"
  , "34221556924533266713564437782467554889357866599146"
  , "33859739644496184175551729528666283163977739427418"
  , "35135958944624616915573572712668468382377957949348"
  , "43587335415469844652657195576376821668748793277985"
  , "58262537826937364893714847591482595861259361697236"
  , "96856393331796741444281785255539289636664139174777"
  , "35323413594643452461575456357268656746837976785794"
  , "35722346434683345754579445686568155679767926678187"
  , "53476438526458754963756986517486719762859782187396"
  , "34253517434536462854564757396567586841767869795287"
  , "45332667135644377824675548893578665991468977611257"
  , "44961841755517295286662831639777394274188841538529"
  , "46246169155735727126684683823779579493488168151459"
  , "54698446526571955763768216687487932779859814388196"
  , "69373648937148475914825958612593616972361472718347"
  , "17967414442817852555392896366641391747775241285888"
  , "46434524615754563572686567468379767857948187896815"
  , "46833457545794456865681556797679266781878137789298"
  , "64587549637569865174867197628597821873961893298417"
  , "45364628545647573965675868417678697952878971816398"
  , "56443778246755488935786659914689776112579188722368"
  , "55172952866628316397773942741888415385299952649631"
  , "57357271266846838237795794934881681514599279262561"
  , "65719557637682166874879327798598143881961925499217"
  , "71484759148259586125936169723614727183472583829458"
  , "28178525553928963666413917477752412858886352396999"
  , "57545635726865674683797678579481878968159298917926"
  , "57944568656815567976792667818781377892989248891319"
  , "75698651748671976285978218739618932984172914319528"
  , "56475739656758684176786979528789718163989182927419"
  , "67554889357866599146897761125791887223681299833479"
  ]

ex2 :: CaveMap
ex2 = parseMap $ unlines
  [ "1122334455"
  , "1122334455"
  , "2233445566"
  , "2233445566"
  , "3344556677"
  , "3344556677"
  , "4455667788"
  , "4455667788"
  , "5566778899"
  , "5566778899"
  ]

getNeighbours :: (Int, Int) -> CaveMap -> [(Int, Int)]
getNeighbours (x, y) caveMap =
  let
    neighbours = [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]
    validNeighbours = filter (\(x, y) -> M.member (x, y) caveMap) neighbours
  in
    validNeighbours

dijkstra :: CaveMap -> CaveMap -> [((Int, Int), Int)] -> S.Set (Int, Int) -> CaveMap
dijkstra caveMap costs heap visited
  | Prelude.null heap = costs
  | otherwise = let
      (current, startCost) = minimumBy (compare `on` snd) heap
      heap' = filter ((/= current) . fst) heap
      unvisitedNeighbours = filter (not . flip S.member visited) $ getNeighbours current caveMap
      neighbourCost = map (id &&& (startCost+) . (caveMap M.!)) unvisitedNeighbours

      -- We need to generate a new heap, because we need to update the costs
      -- of the neighbours of the current node
      newHeap = M.toList $ foldl (\m (k, v) -> M.insertWith min k v m) (M.fromList heap') neighbourCost
      newCosts = foldl (\m (k, v) -> M.insertWith min k v m) costs neighbourCost
      newVisited = S.insert current visited

    in dijkstra caveMap newCosts newHeap newVisited

getEdgeOfMap :: CaveMap -> (Int, Int)
getEdgeOfMap = fst . maximumBy (compare `on` (uncurry (+) . fst)) . M.toList

part1 :: CaveMap -> Int
part1 m = let
    edge = getEdgeOfMap m
    res = dijkstra m (M.fromList [((0, 0), 0)]) [((0,0), 0)] S.empty
  in res M.! edge

mkPart2Map :: CaveMap -> CaveMap
mkPart2Map m = let
    mapPositions = [ (x,y) | x <- [0..4], y <- [0..4], (x, y) /= (0,0) ]
    tileSize = floor . sqrt . fromIntegral $ M.size m

    changeMap :: CaveMap -> (Int, Int) -> CaveMap
    changeMap cave (x,y) = foldl (\m ((x1,y1), r) -> if x1 < tileSize && y1 < tileSize then M.insert (x1+x*tileSize, y1+y*tileSize) ((r+x+y-1) `mod` 9 + 1) m else m) cave (M.toList cave)
  in foldl changeMap m mapPositions

mapDiff :: CaveMap -> CaveMap -> CaveMap
mapDiff a b
  | M.null a = b
  | M.null b = a
  | otherwise = let
      minA = fst $ fromJust $ M.lookupMin a
      minB = fst $ fromJust $ M.lookupMin b
      a' = if minA `M.member` b && b M.! minA == a M.! minA then M.delete minA a else a
      b' = if minB `M.member` a && a M.! minB == b M.! minB then M.delete minB b else b
    in mapDiff a' b'


part2 :: CaveMap -> Int
part2 = part1 . mkPart2Map

parseMap :: String -> CaveMap
parseMap = M.fromList . concatMap ((\(y, bs) -> map (\(x, c) -> ((x, y), digitToInt c)) bs) . second (zip [0..])) . zip [0..] . lines

-- This is BY FAR my slowest AoC submission :(
-- Could DEFINITELY be improved by simply not implementing full dijkstra and only looking for the solution
main :: IO ()
main = do
  input <- parseMap <$> readFile "input.txt"
  putStrLn $ "Part 1: " ++ show (part1 input)
  putStrLn $ "Part 2: " ++ show (part2 input)