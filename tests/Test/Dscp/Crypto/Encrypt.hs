module Test.Dscp.Crypto.Encrypt where

import qualified Data.ByteString as BS
import Test.Common

import Dscp.Crypto (DecryptionError (..), PassPhrase, decrypt, encrypt, maxPassPhraseLength,
                    minPassPhraseLength, mkPassPhrase)

instance Arbitrary PassPhrase where
    arbitrary = either (error . show) identity . mkPassPhrase <$> arbitrary `suchThat`
        (\bs -> BS.length bs >= minPassPhraseLength && BS.length bs <= maxPassPhraseLength)

spec_encryption :: Spec
spec_encryption = describe "AES encryption functions" $ do
    it "should stay the same after encryption and decryption" $ property $
        \(pp, bs :: ByteString) -> decrypt pp (encrypt pp bs) === Right bs
    it "should yield different ciphertexts for different plaintexts" $ property $
        \(pp, bs :: ByteString, bs' :: ByteString) -> bs /= bs' ==> encrypt pp bs /= encrypt pp bs'
    it "should encrypt differently for different passphrases" $ property $
        \(pp, pp', bs :: ByteString) -> pp /= pp' ==> encrypt pp bs /= encrypt pp' bs
    it "should fail when trying to decrypt a ciphertext with wrong passphrase" $ property $
        \(pp, pp', bs :: ByteString) -> pp /= pp' ==> decrypt pp (encrypt pp' bs) === Left PassPhraseInvalid