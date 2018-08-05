{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Address
import           Control.Parallel.Strategies
import           Data.Binary                 (encode)
import           Data.ByteString             (ByteString)
import qualified Data.ByteString.Base16      as BS16 (encode)
import qualified Data.ByteString.Char8       as C8
import qualified Data.ByteString.Lazy        as LBS
import           Data.Maybe                  (catMaybes)
import           System.Environment          (getArgs)

contains :: ByteString -> Seed -> Maybe ByteString
contains subs seed =
    case C8.breakSubstring subs addr of
      (_, y) | C8.null y -> Nothing
             | otherwise -> Just $ addr <> " " <> BS16.encode seed
  where addr = toAddress seed

seeds :: [Seed]
seeds = fmap LBS.toStrict seeds' `using` parBuffer 20 rdeepseq
  where
    seeds' = go $ LBS.concat [ encode x | x <- [seed0..] ]
    go seed = LBS.take 32 seed : go (LBS.drop 32 seed)
    seed0 = 4242 :: Integer

main :: IO ()
main = do
    (w : _) <- getArgs
    let result = contains (C8.pack w) <$> seeds
     in mapM_ C8.putStrLn $ take 2 $ catMaybes (result `using` parBuffer 20 rseq)

