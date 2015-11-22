module Encode where

import Data.Char
import Parse

message = "l[m[\"x\"; 2; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 1; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 2; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 0; \"y\"; 0; \"v\"; \"x\"]; m[\"x\"; 1; \"y\"; 2; \"v\"; \"o\"]; m[\"x\"; 1; \"y\"; 1; \"v\"; \"x\"]; m[\"x\"; 0; \"y\"; 2; \"v\"; \"o\"]]"
templateMapItem = "m[\"x\"; _; \"y\"; _; \"v\"; \"_\"];"

getDecodedMap :: ExternalMap
getDecodedMap = Parse.readFullMap message

decodedMoves :: ExternalMap -> String
decodedMoves decoded = unwords $ map getStringFromMove decoded

encodedList :: ExternalMap -> String
encodedList moves = concat [init $ concat ["l[", decodedMoves moves], "]"]

getStringFromMove :: Move -> String
getStringFromMove move = 
	let
	   xCoord = intToDigit (x move)
	   string1 = replaceX templateMapItem '_' xCoord
	   yCoord = intToDigit (y move)
	   string2 = replaceX string1 '_' yCoord
	   moveValue = value move
	in replaceX string2 '_' moveValue


replaceX :: [Char] -> Char -> Char -> [Char]
replaceX items old new = mapOnce check items where
    check item  | item == old = Just new 
                | otherwise   = Nothing

mapOnce _ []     = []
mapOnce f (x:xs) = case f x of
        Nothing -> x : mapOnce f xs
        Just y  -> y : xs