-- | DB-related actions.

module Dscp.Snowdrop.Actions
    ( SDActions
    , SDActionsM (..)
    , initSDActions
    ) where

import Control.Monad.Free (Free (..))
import qualified Data.Map as M
import qualified Data.Tree.AVL as AVL
import Loot.Log.Rio (LoggingIO)
import Snowdrop.Model.Execution (DbModifyActions (..), SumChangeSet)
import Snowdrop.Util (RIO, gett)

import Dscp.Core
import Dscp.Crypto
import Dscp.Snowdrop.Configuration (Ids (..), Values (..))
import Dscp.Snowdrop.Serialise ()
import Dscp.Snowdrop.Storage.Avlp (AVLChgAccum, ClientError (..), RememberForProof, RootHash,
                                   avlClientDbActions, avlServerDbActions, deserialiseM,
                                   initAVLPureStorage)
import Dscp.Snowdrop.Storage.Pure (blockDbActions)
import Dscp.Snowdrop.Types
import Dscp.Witness.AVL (AvlHash, AvlProof)
import Dscp.Witness.Config

-- It should be something more complex than IO.
type SDActions = SDActionsM (RIO LoggingIO)

-- Parameter m will be instantiated with RIO Context when the context is defined.
data SDActionsM m = SDActionsM
    { nsStateDBActions :: RememberForProof
                       -> DbModifyActions (AVLChgAccum Ids Values) Ids Values m AvlProof
    , nsBlockDBActions :: DbModifyActions (SumChangeSet Ids Values) Ids Values m ()
    }

initSDActions ::
       forall m n.
       (MonadIO m, MonadCatch m, MonadIO n, MonadCatch n, HasWitnessConfig)
    => m (SDActionsM n)
initSDActions = do
    avlInitState <- initAVLPureStorage @Ids @Values initAccounts
    (serverStDba, serverLookupHash) <- avlServerDbActions avlInitState
    serverBlkDba <- blockDbActions
    let sdActions = SDActionsM serverStDba serverBlkDba

    -- This is something to be used by AVL client (???)
    let retrieveF :: AvlHash -> n (Maybe ByteString)
        retrieveF h = serverLookupHash h >>= \resp -> do
          whenJust resp $ \resp' -> do
            (respV :: AVL.MapLayer AvlHash Ids Values AvlHash) <- deserialiseM resp'
            let respProof = AVL.Proof . Free $ pure <$> respV
            when (not $ AVL.checkProof h respProof) $ throwM BrokenProofError
          pure resp
    let (rootHash :: RootHash) = gett avlInitState

    -- What am I supposed to do with it?
    _clientStDba <- avlClientDbActions @Ids @Values retrieveF rootHash

    pure sdActions
  where

    genesisBlockAddress = mkAddr $ toPublic genesisSk
    totalCoins = totalCoinsAddrMap $ giAddressMap genesisInfo

    initAccounts :: Map Ids Values
    initAccounts = M.fromList
        [
          ( AccountInIds (AccountId genesisBlockAddress)
          , AccountOutVal (Account (coinToInteger totalCoins) 0)
          )
        ]
