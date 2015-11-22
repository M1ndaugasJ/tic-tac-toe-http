module Parse where

import Data.List
import Data.Char
import Data.List.Split

type ExternalMap = [Move]
type Coord = Int
data Move = Move  {x :: Coord  
                     , y :: Coord  
                     , value :: Char 
                     } deriving (Show)

instance Eq Move where
    (Move x1 y1 _) == (Move x2 y2 _) = (x1 == x2) && (y1 == y2)

readFullMap :: String -> ExternalMap 
readFullMap mexprmessage = map readInternalMap $ splitIntoEncodedMaps mexprmessage

readCoordinates :: String -> (String, Coord, Coord)
readCoordinates singleMap =
    let
       (rest, coord) = readTillPairEnd singleMap
       (rest', coord') = readTillPairEnd rest
    in (rest', coord, coord')

readTillPairEnd :: String -> (String, Coord)
readTillPairEnd encodedMap = 
    let
        string = dropWhile (\n -> n /= '0' && n /= '1' && n /= '2') encodedMap
        coord = digitToInt $ head string
    in (drop 1 string, coord)

readValue :: String -> Char
readValue restInternalMap = head $ dropWhile (\n -> n /= 'x' && n /= 'o') restInternalMap

readInternalMap :: String -> Move
readInternalMap [] = error "map is empty"
readInternalMap encodedMap =
    let 
        (rest, x, y) = readCoordinates encodedMap
        value = readValue rest
    in Move x y value

splitIntoEncodedMaps :: String -> [String]
splitIntoEncodedMaps [] = error "string is empty"
splitIntoEncodedMaps s = splitOn "m" (drop 3 (filter (/=' ') (s)))




