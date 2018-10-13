-- | JSON insances for core types

module Dscp.Core.Aeson () where

import Data.Aeson (FromJSON (..), FromJSONKey (..), ToJSON (..), ToJSONKey, Value (..), withObject,
                   withScientific, withText, (.:))
import Data.Aeson.Options (defaultOptions)
import Data.Aeson.TH (deriveFromJSON, deriveJSON)
import Data.Typeable (gcast)
import Data.Fixed (Micro, Fixed(MkFixed), showFixed)
import qualified Data.Text as T

import Dscp.Core.Config
import Dscp.Core.Fees
import Dscp.Core.Foundation
import Dscp.Core.Genesis
import Dscp.Core.Governance
import Dscp.Crypto
import Dscp.Util (Base (Base16), leftToFail, nothingToFail)
import Dscp.Util.Aeson (parseJSONSerialise, toJSONSerialise)

---------------------------------------------------------------------------
-- Manual instances
---------------------------------------------------------------------------

-- | TODO: make a generic instance generation for these enum-like instances
instance ToJSON AssignmentType where
    toJSON Regular     = String "regular"
    toJSON CourseFinal = String "courseFinal"
instance FromJSON AssignmentType where
    parseJSON = withText "AssignmentType" $ \case
        "regular"     -> pure Regular
        "courseFinal" -> pure CourseFinal
        other -> fail $ "invalid constructor: " ++ toString other

instance ToJSON DocumentType where
    toJSON Offline = String "offline"
    toJSON Online  = String "online"
instance FromJSON DocumentType where
    parseJSON = withText "DocumentType" $ \case
        "online" -> pure Online
        "offline" -> pure Offline
        other -> fail $ "invalid constructor: " ++ toString other

instance ToJSON Address where
    toJSON = String . toText
instance FromJSON Address where
    parseJSON = withText "Address" $ leftToFail . addrFromText

instance ToJSON Grade where
    toJSON = Number . fromIntegral . getGrade
instance FromJSON Grade where
    parseJSON = withScientific "Grade" $
        maybe (fail "value is outside [0, 100] range") pure .
        mkGrade . round

instance ToJSON SubmissionWitness where
    toJSON = toJSONSerialise Base16
instance FromJSON SubmissionWitness where
    parseJSON = parseJSONSerialise Base16

instance ToJSON ATGSubjectChange where
    toJSON = String . \case
        ATGAdded -> "added"
        ATGRemoved -> "removed"

instance FromJSON ATGSubjectChange where
    parseJSON = withText "ATG subject change" $ \case
        "added" -> pure ATGAdded
        "removed" -> pure ATGRemoved
        _ -> fail "Invalid ATG subject change"

instance FromJSON Governance where
  parseJSON = withObject "governance" $ \o -> do
    (governanceType :: Text) <- o .: "type"
    case governanceType of
        "committeeOpen" -> do
            commSecret <- o .: "secret"
            commN <- o .: "n"
            return $ GovCommittee (CommitteeOpen {..})
        "committeeClosed" -> do
            commParticipants <- o .: "participants"
            return $ GovCommittee (CommitteeClosed {..})
        "governanceOpen" -> return GovOpen
        _ -> fail "Governance type is invalid"

---------------------------------------------------------------------------
-- Standalone derivations for newtypes
---------------------------------------------------------------------------

instance ToJSON Coin where
    toJSON (Coin c) = String $ T.pack $
                      showFixed True (MkFixed $ fromIntegral c :: Micro)

instance FromJSON Coin where
    parseJSON = withText "Coin" $ leftToFail . parseCoin

deriving instance ToJSON Nonce
deriving instance FromJSON Nonce

deriving instance ToJSON GTxId
deriving instance FromJSON GTxId

deriving instance ToJSON Course
deriving instance FromJSON Course

deriving instance ToJSON Subject
deriving instance FromJSON Subject

deriving instance ToJSON SlotDuration
deriving instance FromJSON SlotDuration

deriving instance ToJSON SlotId
deriving instance FromJSON SlotId

deriving instance ToJSON Difficulty
deriving instance FromJSON Difficulty

instance FromJSONKey Subject
instance ToJSONKey Subject

deriving instance ToJSON ATGDelta
deriving instance FromJSON ATGDelta

instance FromJSONKey Address
instance ToJSONKey Address

deriving instance FromJSON GenAddressMap
deriving instance ToJSON GenAddressMap
instance FromJSON CommitteeSecret where
    parseJSON = withText "CommitteeSecret" $ leftToFail . mkCommitteeSecret . encodeUtf8

deriving instance ToJSON GenesisDistribution
deriving instance FromJSON GenesisDistribution

---------------------------------------------------------------------------
-- TH derivations for data
---------------------------------------------------------------------------

deriveJSON defaultOptions ''Assignment
deriveJSON defaultOptions ''Submission
deriveJSON defaultOptions ''SignedSubmission
deriveJSON defaultOptions ''PrivateTx
deriveJSON defaultOptions ''PrivateBlockHeader
deriveJSON defaultOptions ''Header
deriveJSON defaultOptions ''TxInAcc
deriveJSON defaultOptions ''TxOut
deriveJSON defaultOptions ''Tx
deriveJSON defaultOptions ''TxWitness
deriveJSON defaultOptions ''TxWitnessed
deriveJSON defaultOptions ''GTx
deriveJSON defaultOptions ''GTxWitnessed
deriveJSON defaultOptions ''PublicationTxWitness
deriveJSON defaultOptions ''PublicationTxWitnessed
deriveJSON defaultOptions ''PublicationTx
deriveJSON defaultOptions ''FeeCoefficients
deriveFromJSON defaultOptions ''FeeConfig

deriveFromJSON defaultOptions ''Committee
deriveJSON defaultOptions ''GenesisDistributionElem
deriveFromJSON defaultOptions ''GenesisConfig

instance FromJSON GenesisInfo where
    parseJSON = error "FromJSON GenesisInfo should never be called"

instance Typeable tx => FromJSON (FeePolicy tx) where
    parseJSON = asum . sequence
        [ withObject "linear fee policy" $ \o -> do
            "linear" :: Text <- o .: "type"
            coeffs           <- o .: "coeffs"
            return $ LinearFeePolicy coeffs
        , withObject "unknown fee policy" $ \o -> do
            "unknown" :: Text <- o .: "type"
            nothingToFail unallowedPolicy $ gcast UnknownFeePolicy
        ]
      where
        unallowedPolicy = "This fees policy is not applicable"
