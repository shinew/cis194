{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS.Char8
import Data.ByteString (ByteString)
import Data.Bits

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

main :: IO ()
main =
  do
    secret <- getSecret "dog-original.jpg" "dog.jpg"
    BS.Char8.putStrLn secret
    decryptWithKey secret "victims.json"
