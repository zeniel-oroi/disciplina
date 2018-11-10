module Dscp.DB.SQLite.BlockData
    ( BlockIdx (..)
    , TxWithinBlockIdx (..)
    , TxBlockIdx (..)
    , txBlockIdxToInt
    , txBlockIdxFromInt
    ) where

-- | Index of block (difficulty).
newtype BlockIdx = BlockIdx { unBlockIdx :: Word32 }
    deriving (Show, Eq, Ord, Num, Generic)

-- | Index of transaction within block, if any block contains it.
newtype TxWithinBlockIdx = TxWithinBlockIdx { unTxWithinBlockIdx :: Word32 }
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Generic)

-- | Index of transaction within block.
data TxBlockIdx
    = TxBlockIdx TxWithinBlockIdx
    | TxInMempool
    deriving (Eq, Show)

txBlockIdxToInt :: TxBlockIdx -> Int
txBlockIdxToInt = \case
    TxBlockIdx idx -> fromIntegral idx
    TxInMempool    -> -1

txBlockIdxFromInt :: Int -> Either Text TxBlockIdx
txBlockIdxFromInt idx
    | idx >= 0 = Right $ TxBlockIdx (fromIntegral idx)
    | idx == -1 = Right $ TxInMempool
    | otherwise = Left $ "Bad transaction index within block: " <> pretty idx
