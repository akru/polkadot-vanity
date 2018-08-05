module Address where

import qualified Crypto.Hash.BLAKE2.BLAKE2b as Blake2 (hash)
import           Crypto.Sign.Ed25519        (PublicKey (..),
                                             createKeypairFromSeed)
import           Data.ByteString            as BS
import           Data.ByteString.Base58     (bitcoinAlphabet, encodeBase58)
import           Data.Monoid

type Seed = ByteString
type Address = ByteString

encode58ss :: PublicKey -> Address
encode58ss (PublicKey key) = encodeBase58 bitcoinAlphabet $ BS.cons version key <> checksum
  where
    checksum = BS.take 2 $ Blake2.hash 64 mempty $ BS.cons version key
    version = 42

toAddress :: Seed -> Address
toAddress = encode58ss . fst . createKeypairFromSeed
