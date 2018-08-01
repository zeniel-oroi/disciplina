module Dscp.Wallet.Backend
       ( WalletFace(..)
       , createWalletFace
       ) where

import Control.Exception (throwIO)

import Dscp.Core (Tx (..), TxInAcc (..), TxWitness (..), TxWitnessed (..), mkAddr, toTxId)
import Dscp.Crypto (decrypt, emptyPassPhrase, encrypt, keyGen, sign, toPublic)
import Dscp.Wallet.Client
import Dscp.Wallet.Face
import Dscp.Wallet.KeyStorage
import Dscp.Web
import Dscp.Witness.Web.Types

createWalletFace :: NetworkAddress -> (WalletEvent -> IO ()) -> IO WalletFace
createWalletFace serverAddress sendEvent = do
    sendStateUpdateEvent sendEvent
    wc <- createWalletClient serverAddress
    return WalletFace
        { walletRefreshState = sendStateUpdateEvent sendEvent
        , walletGenKeyPair = genKeyPair sendEvent
        , walletRestoreKey = restoreKey sendEvent
        , walletListKeys = listKeys
        , walletSendTx = sendTx wc
        , walletGetBalance = getBalance wc
        }

sendStateUpdateEvent :: (WalletEvent -> IO ()) -> IO ()
sendStateUpdateEvent sendEvent = getAccounts >>= sendEvent . WalletStateUpdateEvent

genKeyPair :: (WalletEvent -> IO ()) -> Maybe Text -> Maybe PassPhrase -> IO Account
genKeyPair sendEvent mName mPassPhrase = do
    (sk, pk) <- keyGen
    let account = Account
            { accountName = mName
            , accountSecretKey = encrypt (fromMaybe emptyPassPhrase mPassPhrase) sk
            , accountPublicKey = pk
            , accountAddress = mkAddr pk
            }
    addAccount account
    sendStateUpdateEvent sendEvent
    return account

restoreKey :: (WalletEvent -> IO ()) -> Maybe Text -> Encrypted SecretKey -> Maybe PassPhrase -> IO ()
restoreKey sendEvent mName eSecretKey mPassPhrase = do
    secretKey <- either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        account = Account
            { accountName = mName
            , accountSecretKey = eSecretKey
            , accountPublicKey = publicKey
            , accountAddress = mkAddr publicKey
            }
    addAccount account
    sendStateUpdateEvent sendEvent

listKeys :: IO [Account]
listKeys = getAccounts

sendTx :: WalletClient -> Encrypted SecretKey -> Maybe PassPhrase -> NonEmpty TxOut -> IO Tx
sendTx wc eSecretKey mPassPhrase (toList -> outs) = do
    secretKey <- either throwIO return . decrypt (fromMaybe emptyPassPhrase mPassPhrase) $ eSecretKey
    let publicKey = toPublic secretKey
        address = mkAddr publicKey

    -- TODO: request nonce for a given address from witness node
    nonce <- asNextNonce <$> wGetAccountState wc address

    let inAcc   = TxInAcc { tiaNonce = nonce, tiaAddr   = address }
        tx      = Tx      { txInAcc  = inAcc, txInValue = inValue, txOuts = outs }
        inValue = Coin $ sum $ unCoin . txOutValue <$> outs

        signature   = sign secretKey (toTxId tx, publicKey, ())
        witness     = TxWitness   { txwSig = signature, txwPk = publicKey }
        txWitnessed = TxWitnessed { twTx   = tx, twWitness = witness }

    void $ wSubmitTx wc txWitnessed
    return tx

getBalance :: WalletClient -> Address -> IO Coin
getBalance wc address = do
    AccountState{..} <- wGetAccountState wc address
    return (bConfirmed asBalances)