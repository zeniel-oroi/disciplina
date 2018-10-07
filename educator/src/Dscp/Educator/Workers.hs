{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedLists  #-}

module Dscp.Educator.Workers
       ( educatorWorkers
       ) where

import Fmt (build, fmt, nameF)
import Fmt ((+||), (||+))
import Loot.Base.HasLens (lensOf)
import Loot.Config (option, sub)
import Loot.Log (logInfo, logWarning)
import Time (ms, toUnit)

import Dscp.Core
import Dscp.DB.SQLite
import Dscp.Educator.Config
import Dscp.Educator.Launcher.Mode
import Dscp.Network
import Dscp.Resource.Keys
import Dscp.Util.Timing
import Dscp.Witness.Mempool
import Dscp.Witness.SDLock

educatorWorkers
    :: CombinedWorkMode ctx m
    => [Worker m]
educatorWorkers =
    [privateBlockPublishingWorker]

----------------------------------------------------------------------------
-- Private blocks publishing
----------------------------------------------------------------------------

makePublicationTx
    :: CombinedWorkMode ctx m
    => PrivateBlockHeader -> m PublicationTxWitnessed
makePublicationTx header = do
    sk <- view $ lensOf @(KeyResources EducatorNode) . krSecretKey
    pk <- view $ lensOf @(KeyResources EducatorNode) . krPublicKey
    let tx = PublicationTx
            { ptAuthor = mkAddr pk
            , ptFeesAmount = unFees $ calcFeePub (fcPublication feeConfig) header
            , ptHeader = header
            }
    return $ signPubTx sk tx

privateBlockPublishingWorker :: CombinedWorkMode ctx m => Worker m
privateBlockPublishingWorker =
    Worker "privateBlockPublishingWorker" [] [] $ \_ -> bootstrap >> work
  where
    period = educatorConfig ^. sub #educator . sub #publishing . option #period
    slotDuration =
        toUnit . ms $ fromIntegral $ unSlotDuration $ giveL @WitnessConfig

    bootstrap =
        unless (period > slotDuration) $
            logWarning $ "Private block publishing period is not greater than \
                         \witness slot duration ("
                         +|| period ||+ " <= " +|| slotDuration ||+ ")"

    work = periodically "Private block publisher" period $ do
        mblock <- transactW $ runMaybeT (createPrivateBlock Nothing)
        case mblock of
            Nothing -> logInfo "No private chain updates, skipping private \
                                \block creation"
            Just block -> do
                txw <- makePublicationTx block
                logInfo $ fmt $ nameF "Created new private block" (build txw)
                -- TODO [DSCP-299] Be more insistent
                isNew <- writingSDLock "add pub to mempool" $
                          addTxToMempool (GPublicationTxWitnessed txw)
                unless isNew $
                    logWarning "Private block was already present"