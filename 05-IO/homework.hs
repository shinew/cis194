{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map.Strict as DMS
import Data.ByteString (ByteString)
import System.Environment (getArgs)
import Data.Bits
import Data.List
import Data.Function

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


getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimFile transactionFile =
  let filterT a b = case (a, b) of (Just vs, Just ts) -> Just $ filter (\t -> any (\v -> tid t == v) vs) ts
                                   _                  -> Nothing
  in do
    victims <- parseFile victimFile :: IO (Maybe [TId])
    transactions <- parseFile transactionFile :: IO (Maybe [Transaction])
    return $ filterT victims transactions

getFlow :: [Transaction] -> DMS.Map String Integer
getFlow transactions =
  let
    newVal val x = case x of (Just a) -> Just (a + val)
                             _        -> Just val
    updateT t m =
      let
        money = amount t
        fromP = from t
        toP = to t
      in
        (DMS.alter (newVal (-money)) fromP . DMS.alter (newVal money) toP) m

  in foldr updateT DMS.empty transactions

getCriminal :: DMS.Map String Integer -> String
getCriminal = fst . maximumBy (compare `on` snd) . DMS.toList

pairTrans :: [(String, Integer)] -> [(String, Integer)] -> [TId] -> [Transaction]
pairTrans [] [] _ = []
pairTrans (a:as) (b:bs) (i:is)
  | diff == 0  = buildTransaction payerName payeeName positive i   : pairTrans as bs is
  | diff > 0  = buildTransaction payerName payeeName (-negative) i: pairTrans ((payerName, diff):as) bs is
  | otherwise = buildTransaction payerName payeeName positive i   : pairTrans as ((payeeName, diff):bs) is
  where
    positive = snd a
    negative = snd b
    diff = positive + negative
    payerName = fst a
    payeeName = fst b
    buildTransaction payer payee money newId = Transaction { from = payer,
                                                             to = payee,
                                                             amount = money,
                                                             tid = newId
                                                           }
pairTrans _ _ _ = []

undoTs :: DMS.Map String Integer -> [TId] -> [Transaction]
undoTs m tids =
  let
    lst = DMS.toList m
    reverseSort = sortBy (flip compare `on` snd)
    payers = (reverseSort . filter (\(_, i) -> i > 0)) lst
    payees = (reverseSort . filter (\(_, i) -> i < 0)) lst
  in
    pairTrans payers payees tids

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON file a = BSL.writeFile file $ encode a

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim
