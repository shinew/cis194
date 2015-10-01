{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString (ByteString)
import Data.Bits

import Parser

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret original modified =
  let
    findSecret a b = (BS.filter (/= 0) . BS.pack) $ BS.zipWith xor a b
  in do
    originalContents <- BS.readFile original
    modifiedContents <- BS.readFile modified
    return $ findSecret originalContents modifiedContents

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file =
  let
    processKey a b = BS.pack $ zipWith xor a b
  in do
    json <- BS.readFile $ file ++ ".enc"
    let longKey = (cycle . BS.unpack) key
    let unpackedJson = BS.unpack json
    BS.writeFile file $ processKey unpackedJson longKey

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile file =
  do
    contents <- BSL.readFile file
    return $ decode contents

filterTransactions :: Maybe [TId] -> Maybe [Transaction] -> Maybe [Transaction]
filterTransactions (Just vs) (Just ts) =
  Just $ filter (\t -> any (\v -> tid t == v) vs) ts
filterTransactions _ _ = Nothing

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile =
  do
    victims <- parseFile victimFile :: IO (Maybe [TId])
    transactions <- parseFile transactionFile :: IO (Maybe [Transaction])
    return $ filterTransactions victims transactions

main :: IO ()
main =
  do
    secret <- getSecret "dog-original.jpg" "dog.jpg"
    BS.Char8.putStrLn secret
    decryptWithKey secret "victims.json"
    _ <- getBadTs "victims.json" "transactions.json"
    putStrLn "Enough"
