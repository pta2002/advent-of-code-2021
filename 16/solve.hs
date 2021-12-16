import Text.Parsec
import Text.ParserCombinators.Parsec (GenParser)
import Data.Either (fromRight)

data PacketType = LiteralT | OperatorT Int deriving (Eq, Show)
data OperatorType = Sum | Product | Minimum | Maximum | GreaterThan | LessThan | EqualTo deriving (Eq, Show)
data PacketContent = Literal Int | Operator OperatorType [Packet] deriving (Show, Eq)
type Packet = (Int, PacketContent)

intToPacketType :: Int -> PacketType
intToPacketType 4 = LiteralT
intToPacketType i = OperatorT i

hexToBits :: Char -> String
hexToBits '0' = "0000"
hexToBits '1' = "0001"
hexToBits '2' = "0010"
hexToBits '3' = "0011"
hexToBits '4' = "0100"
hexToBits '5' = "0101"
hexToBits '6' = "0110"
hexToBits '7' = "0111"
hexToBits '8' = "1000"
hexToBits '9' = "1001"
hexToBits 'A' = "1010"
hexToBits 'B' = "1011"
hexToBits 'C' = "1100"
hexToBits 'D' = "1101"
hexToBits 'E' = "1110"
hexToBits 'F' = "1111"
hexToBits _ = error "Invalid hex character"

bin2dec :: String -> Int
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
    where c2i c = if c == '0' then 0 else 1

packetVersion :: GenParser Char st Int
packetVersion = do
    bits <- count 3 anyToken
    return $ bin2dec bits

packetType :: GenParser Char st PacketType
packetType = do
    bits <- count 3 anyToken
    return $ intToPacketType $ bin2dec bits

packetHeader :: GenParser Char st (Int, PacketType)
packetHeader = do
    version <- packetVersion
    t <- packetType
    return (version, t)

literalPacketS :: GenParser Char st String
literalPacketS = do
    (c:num) <- count 5 anyToken
    if c == '1' then do
        rest <- literalPacketS
        return $ num ++ rest
    else
        return num

literalPacket :: GenParser Char st PacketContent
literalPacket = Literal . bin2dec <$> literalPacketS

operatorPacket :: Int -> GenParser Char st PacketContent
operatorPacket i = do
    lengthId <- oneOf ['1', '0']
    -- TODO: This is wrong! If it's 0, then we have 15 bits containing the total size of the subpackets
    -- If it's 1, we have 11 bits containing the **number** of subpackets!
    (if lengthId == '0' then totalBitsOperator else packetCountOperator) i

packetCountOperator :: Int -> GenParser Char st PacketContent
packetCountOperator i = do
    packetCount <- bin2dec <$> count 11 anyToken
    packets <- count packetCount packet
    return $ Operator (operatorTypeFromInt i) packets

totalBitsOperator :: Int -> GenParser Char st PacketContent
totalBitsOperator i = do
    totalLength <- bin2dec <$> count 15 anyToken
    subPackets <- count totalLength anyToken
    let r = parse (many1 packet) "" subPackets
    case r of
        Left _ -> fail "Oh no!"
        Right packets -> return $ Operator (operatorTypeFromInt i) packets

packet :: GenParser Char st Packet
packet = do
    (version, t) <- packetHeader
    content <- case t of
        LiteralT -> literalPacket
        OperatorT i -> operatorPacket i
    return (version, content)

parsePacket :: String -> Packet
parsePacket = fromRight (error "Invalid packet") . parse packet ""

versions :: Packet -> [Int]
versions (v, p) = v:case p of
    Operator _ ps -> concatMap versions ps
    _ -> []

parseHexPacket :: String -> Packet
parseHexPacket = parsePacket . concatMap hexToBits

part1 :: Packet -> Int
part1 = sum . versions

operatorTypeFromInt :: Int -> OperatorType
operatorTypeFromInt 0 = Sum
operatorTypeFromInt 1 = Product
operatorTypeFromInt 2 = Minimum
operatorTypeFromInt 3 = Maximum
operatorTypeFromInt 5 = GreaterThan
operatorTypeFromInt 6 = LessThan
operatorTypeFromInt 7 = EqualTo
operatorTypeFromInt _ = error "Invalid"

evaluatePacket :: PacketContent -> Int
evaluatePacket (Operator Sum ps) = sum $ map (evaluatePacket . snd) ps
evaluatePacket (Operator Product ps) = product $ map (evaluatePacket . snd) ps
evaluatePacket (Operator Minimum ps) = minimum $ map (evaluatePacket . snd) ps
evaluatePacket (Operator Maximum ps) = maximum $ map (evaluatePacket . snd) ps
evaluatePacket (Operator GreaterThan ps) = if p1 > p2 then 1 else 0
    where
        p1 = evaluatePacket (snd $ head ps)
        p2 = evaluatePacket (snd $ ps !! 1)
evaluatePacket (Operator LessThan ps) = if p1 < p2 then 1 else 0
    where
        p1 = evaluatePacket (snd $ head ps)
        p2 = evaluatePacket (snd $ ps !! 1)
evaluatePacket (Operator EqualTo ps) = if p1 == p2 then 1 else 0
    where
        p1 = evaluatePacket (snd $ head ps)
        p2 = evaluatePacket (snd $ ps !! 1)
evaluatePacket (Literal i) = i

part2 :: Packet -> Int
part2 = evaluatePacket . snd

main :: IO ()
main = do
    input <- parseHexPacket <$> readFile "input.txt"
    putStrLn $ "Part 1: " ++ show (part1 input)
    putStrLn $ "Part 2: " ++ show (part2 input)