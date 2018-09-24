{-# LANGUAGE GADTs      #-}
{-# LANGUAGE StrictData #-}

-- | Trnsaction fees processing and calculation.
module Dscp.Core.Fees
       ( Fees (..)
       , _Fees
       , FeeCoefficients (..)
       , FeePolicy (..)
       , FeeConfig (..)
       , calcLinearFees
       , calcFee
       , calcFeePub
       , calcFeeG
       , noFees
       , noFeesConfig
       , fixFees
       ) where

import Codec.Serialise (Serialise (..))
import Control.Lens (makePrisms)

import Dscp.Core.Foundation
import Dscp.Crypto
import Dscp.Util

-- | Amount of transaction fees.
newtype Fees = Fees { unFees :: Coin }
    deriving (Eq, Ord, Show, Generic, Hashable, Bounded)

makePrisms ''Fees

instance Serialise Fees

-- | Setup for fee calculation by linear rule.
data FeeCoefficients = FeeCoefficients
    { fcMinimal    :: Coin  -- ^ How much is the base fee?
    , fcMultiplier :: Float -- ^ How much tokens taken per item?
    } deriving (Eq, Show)

-- | General setup for fee calculation
data FeePolicy tx where
    LinearFeePolicy :: FeeCoefficients -> FeePolicy tx
    UnknownFeePolicy :: FeePolicy Void

-- | All fee policies.
data FeeConfig = FeeConfig
    { fcMoney       :: FeePolicy Tx
    , fcPublication :: FeePolicy PublicationTx
    }

-- | Calculate fees from tx size.
calcLinearFees :: Integral i => FeeCoefficients -> i -> Fees
calcLinearFees FeeCoefficients{..} size =
    Fees $ Coin $ unCoin fcMinimal + round (fcMultiplier * fromIntegral size)

-- | Calculates fees of money transaction.
calcFee :: FeePolicy Tx -> TxWitnessed -> Fees
calcFee policy tx = case policy of
    LinearFeePolicy coeffs ->
        calcLinearFees coeffs $ unSize $ sizeSerialised (GMoneyTxWitnessed tx)

-- | Calculates fees of publication transaction.
calcFeePub :: FeePolicy PublicationTx -> PrivateBlockHeader -> Fees
calcFeePub policy header = case policy of
    LinearFeePolicy coeffs ->
        calcLinearFees coeffs $ mrSize $ header ^. pbhBodyProof

-- | Calculates fees of 'GTxWitnessed'.
calcFeeG :: FeeConfig -> GTxWitnessed -> Fees
calcFeeG FeeConfig{..} = \case
    (GMoneyTxWitnessed tw) ->
        calcFee fcMoney tw
    (GPublicationTxWitnessed PublicationTxWitnessed { ptwTx }) ->
        calcFeePub fcPublication (ptHeader ptwTx)

-- | "No fee" setup.
noFees :: FeePolicy tx
noFees = LinearFeePolicy FeeCoefficients
    { fcMinimal       = Coin 0
    , fcMultiplier    = 0
    }

noFeesConfig :: FeeConfig
noFeesConfig = FeeConfig noFees noFees

-- | Pick fees which would fit for given transaction. Fits only the
-- case when transaction size grows monotonically with fees.
fixLinearFees :: FeeCoefficients -> (Fees -> TxWitnessed) -> TxWitnessed
fixLinearFees coeffs buildTx = go (Fees $ fcMinimal coeffs)
  where
    go fees =
        let tx = buildTx fees
            Size size = sizeSerialised (GMoneyTxWitnessed tx)
            fees' = calcLinearFees coeffs size
        in case fees `compare` fees' of
            EQ -> tx
            LT -> go fees'
            GT -> error "fixLinearFees: fees unexpectedly decreased"

-- | Pick fees which would fit for given transaction.
fixFees :: FeePolicy Tx -> (Fees -> TxWitnessed) -> TxWitnessed
fixFees feePolicy = case feePolicy of
    LinearFeePolicy coeffs -> fixLinearFees coeffs
