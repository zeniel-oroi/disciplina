module Dscp.Witness.Web.Types
    ( Balances (..)
    , BlockInfo (..)
    , AccountInfo (..)
    , TxInfo (..)
    , TxList (..)
    , HashIs (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, withObject, withText, (.=), (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (Options (..), deriveJSON)
import Fmt (build, genericF, (+|), (|+))

import Dscp.Core
import Dscp.Util.Servant (ForResponseLog (..))

-- | All balances related to account.
data Balances = Balances
    { bConfirmed :: !Coin
      -- ^ Only looking at blocks

    -- , bTotal     :: !Coin
      -- ^ From blocks + mempool
    }

data BlockInfo = BlockInfo
    { biHeaderHash :: HeaderHash
    , biNextHash :: HeaderHash
    , biMerkleRootHash :: Text
    , biHeader :: Header
    , biIsGenesis :: Bool
    , biSince :: Word64  -- µs since UNIX epoch start
    , biSize :: Int64  -- bytes
    , biTransactionCount :: Int
    , biTotalOutput :: Coin
    , biTotalFees :: Coin
    , biTransactions :: Maybe [TxInfo]
    }

-- | All what user may wish to know about an account.
data AccountInfo = AccountInfo
    { aiBalances  :: Balances
    , aiNextNonce :: Integer
    , aiTransactionCount :: Integer
    , aiTransactions :: Maybe [TxInfo]
    }

data TxInfo = TxInfo
    { tiBlock :: Maybe BlockInfo
    , tiTx :: GTx
    }

data TxList = TxList
    { tlTransactions :: [TxInfo]
    , tlNextId :: Maybe GTxId
    }

data HashIs
    = HashIsUnknown
    | HashIsBlock
    | HashIsAddress
    | HashIsMoneyTx
    | HashIsPublicationTx
    deriving (Eq, Show, Generic)

---------------------------------------------------------------------------
-- Buildable instances
---------------------------------------------------------------------------


instance Buildable (ForResponseLog BlockInfo) where
    build (ForResponseLog BlockInfo{..}) =
        "{ headerHash = " +| biHeaderHash |+
        ", header = " +| biHeader |+
        " }"
instance Buildable (ForResponseLog [BlockInfo]) where
    build (ForResponseLog blocks) = "" +| length blocks |+ " blocks"

instance Buildable Balances where
    build Balances{..} = "{ confirmed = " +| bConfirmed |+ " }"

instance Buildable (ForResponseLog AccountInfo) where
    build (ForResponseLog AccountInfo{..}) =
        "{ balances = " +| aiBalances |+
        ", next nonce = " +| aiNextNonce |+
        " }"

instance Buildable (ForResponseLog TxInfo) where
    build (ForResponseLog TxInfo{..}) =
        "{ txId = " +| toGTxId tiTx |+
        ", headerHash = " +| biHeaderHash <$> tiBlock |+
        " }"

instance Buildable (ForResponseLog TxList) where
    build (ForResponseLog TxList{..}) = "" +| length tlTransactions |+ " transactions"

instance Buildable (ForResponseLog HashIs) where
    build (ForResponseLog hashIs) = genericF hashIs

---------------------------------------------------------------------------
-- JSON instances
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Balances
deriveJSON defaultOptions{ omitNothingFields = True } ''BlockInfo
deriveJSON defaultOptions{ omitNothingFields = True } ''AccountInfo
deriveJSON defaultOptions{ omitNothingFields = True } ''TxList

instance ToJSON TxInfo where
    toJSON TxInfo{..} = object $
        maybe [] (\block -> ["block" .= block]) tiBlock ++
        case tiTx of
            GMoneyTx tx ->
                [ "txId" .= toTxId tx
                , "txType" .= ("money" :: Text)
                , "money" .= tx
                , "outValue" .= (foldr sumCoins (Coin 0) . map txOutValue . txOuts $ tx)
                ]
            GPublicationTx pTx ->
                [ "txId" .= toPtxId pTx
                , "txType" .= ("publication" :: Text)
                , "publication" .= pTx
                ]

instance FromJSON TxInfo where
    parseJSON = withObject "tx info" $ \o -> do
        txType :: Text <- o .: "txType"
        block <- o .: "block"
        TxInfo block <$> case txType of
            "money" -> GMoneyTx <$> o .: "money"
            "publication" -> GPublicationTx <$> o .: "publication"
            other -> fail $ "invalid transaction type: " ++ toString other

instance ToJSON HashIs where
    toJSON = String . \case
        HashIsUnknown -> "unknown"
        HashIsBlock -> "block"
        HashIsAddress -> "address"
        HashIsMoneyTx -> "money-transaction"
        HashIsPublicationTx -> "publication-transaction"

instance FromJSON HashIs where
    parseJSON = withText "hash type" $ \case
        "unknown" -> pure HashIsUnknown
        "block" -> pure HashIsBlock
        "address" -> pure HashIsAddress
        "money-transaction" -> pure HashIsMoneyTx
        "publication-transaction" -> pure HashIsPublicationTx
        _ -> fail "Invalid hash type"
