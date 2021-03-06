{-# LANGUAGE CPP              #-}
{-# LANGUAGE TypeApplications #-}

-- | 'HashFunction' instances for 'Cryptonite' hashes

module Dscp.Crypto.Hash.Cryptonite
       ( CryptoniteFunc
       ) where

import Crypto.Hash (Digest, HashAlgorithm)
import qualified Crypto.Hash as Crypto

import Data.ByteArray (ByteArrayAccess, Bytes, convert)
import Data.Hashable (Hashable (hash))

import Dscp.Crypto.Hash.Class (AbstractHash (..), HasAbstractHash (..), HashFunc (..))
import Dscp.Crypto.Signing.Class (AbstractPK, AbstractSK, AbstractSig)

-- | Tag for choosing a particular hash algorithm from 'Crypto.Hash'
-- for hashing.
data CryptoniteFunc algo

-- | Default implementation is for 'ByteArrayAccess'.
instance HashAlgorithm algo => HashFunc (CryptoniteFunc algo) where
    type HashResult (CryptoniteFunc algo) = Digest algo
    unsafeHashBytes = AbstractHash . Crypto.hash

-- | Instances for interesting types with 'ByteArrayAccess'.
instance HashAlgorithm algo =>
         HasAbstractHash (CryptoniteFunc algo) ByteString
instance HashAlgorithm algo =>
         HasAbstractHash (CryptoniteFunc algo) Bytes

#define BA_INSTANCE_HASH(t)                         \
instance (HashAlgorithm algo, ByteArrayAccess t) => \
         HasAbstractHash (CryptoniteFunc algo) t    \

BA_INSTANCE_HASH((AbstractHash hf a))
BA_INSTANCE_HASH((AbstractPK ss))
BA_INSTANCE_HASH((AbstractSK ss))
BA_INSTANCE_HASH((AbstractSig ss a))

-- | Also hash lazy bytestrings in a special way.
instance HashAlgorithm algo =>
         HasAbstractHash (CryptoniteFunc algo) LByteString where
    unsafeAbstractHash = AbstractHash . Crypto.hashlazy

instance Hashable (Digest alg) where
    hash              = hash              . (convert :: Digest alg -> ByteString)
    hashWithSalt salt = hashWithSalt salt . (convert :: Digest alg -> ByteString)
