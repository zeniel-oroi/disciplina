
-- | Utils for roundtrip tests for binary serialisation.

module Test.Dscp.Serialise
    ( serialiseRoundtrip
    , serialiseRoundtripProp
    , aesonRoundtrip
    , aesonRoundtripProp
    ) where

import Codec.Serialise (Serialise, deserialiseOrFail, serialise)
import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.Typeable (typeRep)

import Test.Common

serialiseRoundtrip
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a)
    => Property
serialiseRoundtrip = property $ \(s :: a) ->
    first (show @Text) (deserialiseOrFail (serialise s)) === Right s

serialiseRoundtripProp
    :: forall a. (Arbitrary a, Serialise a, Eq a, Show a, Typeable a)
    => Spec
serialiseRoundtripProp =
    it (show $ typeRep $ Proxy @a) $ serialiseRoundtrip @a

aesonRoundtrip
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a)
    => Property
aesonRoundtrip = property $ \(s :: a) -> do
    eitherDecode (encode s) === Right s

aesonRoundtripProp
    :: forall a. (Arbitrary a, ToJSON a, FromJSON a, Eq a, Show a, Typeable a)
    => Spec
aesonRoundtripProp =
    it (show (typeRep $ Proxy @a)) $ aesonRoundtrip @a