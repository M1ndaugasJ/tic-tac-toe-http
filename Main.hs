{-# LANGUAGE OverloadedStrings #-}

import Network.HTTP.Client
import Network.HTTP
import Network.HTTP.Types.Status (statusCode)
import Data.Aeson (object, (.=), encode)
import Data.ByteString.Internal (unpackBytes)
import Data.ByteString.Char8 (pack, unpack)
import qualified Data.ByteString.Lazy as B
import GHC.Word (Word8)
import Parse
import Encode

format = "application/m-expr+list"

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings
  putStrLn "Please enter unique game name:"
  gameName <- getLine

  let gameUrl = "http://tictactoe.homedir.eu/game/" ++ gameName ++ "/player/1"

  postMove manager gameUrl (Encode.encodedList [(Move 1 1 'x')])
  moves <- getMove manager gameUrl

  postMove manager gameUrl (Encode.encodedList $ moves ++ [(Move 2 2 'x')])
  moves <- getMove manager gameUrl

  postMove manager gameUrl (Encode.encodedList $ moves ++ [(Move 0 1 'x')])
  moves <- getMove manager gameUrl

  postMove manager gameUrl (Encode.encodedList $ moves ++ [(Move 1 2 'x')])
  moves <- getMove manager gameUrl

  postMove manager gameUrl (Encode.encodedList $ moves ++ [(Move 2 0 'x')])

  print $ "Game finished"
  

getMove :: Manager -> String -> IO ExternalMap
getMove manager url = do
  request <- parseUrl url
  let request' = request { method = "GET"
                         , requestHeaders = [("Accept", format)]}
  response <- httpLbs request' manager
  let moves = Parse.readFullMap $ unpack (B.toStrict (responseBody response))
  Prelude.putStr $ "Moves received: "
  print moves
  return $ moves

getPureMove :: Manager -> String -> IO ()
getPureMove manager url = do
  moves <- getMove manager url
  let pureResult = Encode.encodedList moves
  print $ pureResult

postMove :: Manager -> String -> String -> IO ()
postMove manager url move = do
  initialRequest <- parseUrl url
  let request = initialRequest { method = "POST"
                               , requestBody = RequestBodyLBS $ word8ToByteString $ strToWord8s $ move
                               , requestHeaders = [("Content-Type", format)]}
  response <- httpLbs request manager
  Prelude.putStrLn $ "The status code was: " ++ (show $ statusCode $ responseStatus response)

strToWord8s :: String -> [Word8]
strToWord8s = unpackBytes . pack

word8ToByteString :: [Word8] -> B.ByteString
word8ToByteString a = B.pack a
