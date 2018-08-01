{-# LANGUAGE QuasiQuotes #-}

module Dscp.DB.SQLite.Instances () where

import Codec.Serialise as Codec (Serialise, deserialise, serialise)
import Control.Lens (matching, review)

import Database.SQLite.Simple.FromField (FromField (..))
import Database.SQLite.Simple.FromRow (FromRow (..), field)
import Database.SQLite.Simple.ToField (ToField (..))
import Database.SQLite.Simple.ToRow (ToRow (..))

import Dscp.Core (ATGDelta, Address (..), Assignment (..), AssignmentType, Course (..),
                  DocumentType, Grade (..), PrivateTx (..), SignedSubmission (..), Subject (..),
                  Submission (..), SubmissionWitness (..))
import Dscp.Crypto (EmptyMerkleTree, Hash, MerkleSignature, PublicKey, Signature, hash)
import Dscp.DB.SQLite.BlockData (BlockData (..), TxInBlock (..), TxWithIdx (..))
import Dscp.DB.SQLite.Types (TxBlockIdx, intTxBlockIdx)
import Dscp.Util (leftToPanic)


instance FromField (Hash a)            where fromField f = Codec.deserialise <$> fromField f
instance FromField (Signature a)       where fromField f = Codec.deserialise <$> fromField f
instance FromField (MerkleSignature a) where fromField f = Codec.deserialise <$> fromField f
instance Serialise a =>
         FromField (EmptyMerkleTree a) where fromField f = Codec.deserialise <$> fromField f

-- TODO(kir): use #define to generate macros
instance FromField Address           where fromField f = Codec.deserialise <$> fromField f
instance FromField PublicKey         where fromField f = Codec.deserialise <$> fromField f
instance FromField Subject           where fromField f = Subject           <$> fromField f
instance FromField Course            where fromField f = Course            <$> fromField f
instance FromField Grade             where fromField f = UnsafeGrade       <$> fromField f
instance FromField AssignmentType    where fromField f = Codec.deserialise <$> fromField f
instance FromField SubmissionWitness where fromField f = Codec.deserialise <$> fromField f
instance FromField DocumentType      where fromField f = toEnum <$> fromField f
instance FromField ATGDelta          where fromField f = Codec.deserialise <$> fromField f
instance FromField TxBlockIdx        where
    fromField f = leftToPanic . first mkError . matching intTxBlockIdx <$> fromField f
      where
        mkError idx = "Bad transaction index within block: " <> pretty idx

instance ToField   (Hash a)          where toField = toField . Codec.serialise
instance ToField   (Signature a)     where toField = toField . Codec.serialise
instance ToField (MerkleSignature a) where toField = toField . Codec.serialise
instance Serialise a =>
         ToField (EmptyMerkleTree a) where toField = toField . Codec.serialise

instance ToField   Address           where toField = toField . Codec.serialise
instance ToField   Course            where toField = toField . getCourseId
instance ToField   Subject           where toField = toField . getSubjectId
instance ToField   AssignmentType    where toField = toField . Codec.serialise
instance ToField   Grade             where toField = toField . getGrade
instance ToField   SubmissionWitness where toField = toField . Codec.serialise
instance ToField   DocumentType      where toField = toField . fromEnum
instance ToField   TxBlockIdx        where toField = toField . review intTxBlockIdx
instance ToField   ATGDelta          where toField = toField . Codec.serialise

instance FromRow   Course            where fromRow = field
instance FromRow   Grade             where fromRow = field

instance FromRow   Assignment        where fromRow = Assignment       <$> field   <*> field <*> field <*> field
instance FromRow   Submission        where fromRow = Submission       <$> field   <*> field <*> fromRow
instance FromRow   SignedSubmission  where fromRow = SignedSubmission <$> fromRow <*> field
instance FromRow   PrivateTx         where fromRow = PrivateTx        <$> fromRow <*> field <*> field

instance FromRow   TxInBlock         where fromRow = TxInBlock        <$> fromRow <*> field
instance FromRow   TxWithIdx         where fromRow = TxWithIdx        <$> fromRow <*> field

instance ToRow Assignment where
    toRow task@ (Assignment course contentsHash ty text) =
        [toField (hash task), toField course, toField contentsHash, toField ty, toField text]

instance FromRow BlockData where
    fromRow = BlockData <$> field <*> field <*> field <*> field <*> field <*> field <*> field