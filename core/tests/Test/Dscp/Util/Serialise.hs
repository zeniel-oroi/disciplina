module Test.Dscp.Util.Serialise
    ( spec_Serialisation
    ) where

import Dscp.Util.Aeson (AsByteString, Base64Encoded, HexEncoded, Versioned)
import Dscp.Util.Test

spec_Serialisation :: Spec
spec_Serialisation = describe "Serialisation" $ do
    describe "Aeson" $ do
        describe "roundtrip" $ do
            aesonRoundtripProp @(AsByteString HexEncoded ByteString)
            aesonRoundtripProp @(AsByteString Base64Encoded ByteString)
            aesonRoundtripProp @(Versioned ())
