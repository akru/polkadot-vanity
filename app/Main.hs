{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake2 (hash)
import           Crypto.Sign.Ed25519        (PublicKey (..),
                                             createKeypairFromSeed)
import           Data.ByteString            as BS
import qualified Data.ByteString.Base16     as BS16 (encode)
import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import qualified Data.ByteString.Char8      as C8 (pack, putStrLn)
import           Data.ByteString.Random.MWC (random)
import           Data.Monoid                ((<>))
import           System.Environment         (getArgs)
import           Text.Regex.PCRE.Light

encode58ss :: PublicKey -> ByteString
encode58ss (PublicKey key) = encodeBase58 bitcoinAlphabet $ version <> key <> checksum
  where
    checksum = BS.take 2 $ Blake2.hash 64 "" (version <> key)
    version = BS.singleton 42

addressFromSeed :: ByteString -> ByteString
addressFromSeed = encode58ss . fst . createKeypairFromSeed

randomAddress :: IO (ByteString, ByteString)
randomAddress = do
    seed <- random 32
    return (addressFromSeed seed, "0x" <> BS16.encode seed)

addressPattern :: Regex -> IO ()
addressPattern reg = do
    (addr, seed) <- randomAddress
    case match reg addr [] of
      Just matches -> C8.putStrLn (addr <> " " <> seed)
      Nothing      -> addressPattern reg

main :: IO ()
main = do
    (arg : _) <- getArgs
    let pat = flip compile [] $ C8.pack arg
    addressPattern pat

