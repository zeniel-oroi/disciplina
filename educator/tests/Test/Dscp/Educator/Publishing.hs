module Test.Dscp.Educator.Publishing where

import qualified GHC.Exts as Exts

import Dscp.Core
import Dscp.Crypto
import Dscp.DB.SQLite
import Dscp.Educator
import Dscp.Resource.Keys
import Dscp.Util
import Dscp.Util.Test
import Dscp.Witness

import Test.Dscp.Educator.Mode
import Test.Dscp.Educator.Web.Scenarios

spec_Publishing :: Spec
spec_Publishing = describe "Private blocks publishing" $ do
    it "Single block is successfully published by worker" $ educatorPropertyM $ do
        sk <- lift $ ourSecretKeyData @EducatorNode
        env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
        let txs = ordNub . tiList $ ctePrivateTxs env

        block <- lift $ do
            transactW $ prepareAndCreateSubmissions env
            transactW $ forM_ txs createTransaction
            block <- nothingToPanic "No block created" <$> dumpPrivateBlock
            updateMempoolWithPublications
                >>= bool (error "No mempool update??") pass
            updateMempoolWithPublications
                >>= bool pass (error "Extra mempool update")
            return block

        tip <- lift . runSdMempoolLocked $ getPrivateTipHash (skAddress sk)
        return $ tip === hash block

    it "Several blocks are successfully published by worker" $ educatorPropertyM $ do
        sk <- lift $ ourSecretKeyData @EducatorNode
        env <- pickSmall $ genCoreTestEnv simpleCoreTestParams
        let txs = Exts.fromList . ordNub . tiList $ ctePrivateTxs env

        blocks <- lift $ do
            transactW $ prepareAndCreateSubmissions env
            blocks <- forM txs $ \tx -> do
                _ <- transactW $ createTransaction tx
                nothingToPanic "No block created" <$> dumpPrivateBlock
            _ <- updateMempoolWithPublications
            return blocks

        -- we have tests on publication separately, so just comparing tips
        tip <- lift . runSdMempoolLocked $ getPrivateTipHash (skAddress sk)
        return $ tip === hash (last blocks)
