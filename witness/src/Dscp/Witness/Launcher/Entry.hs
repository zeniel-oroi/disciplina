-- | Witness entry point.

module Dscp.Witness.Launcher.Entry
    ( witnessEntry
    ) where

import Control.Concurrent (threadDelay)
import Fmt ((+|), (|+))
import Loot.Base.HasLens (lensOf)
import Loot.Log (logInfo, modifyLogName)
import UnliftIO.Async (async)

import Dscp.Network (runListener, runWorker, withServer)
import Dscp.Witness.Config
import Dscp.Witness.Launcher.Mode
import Dscp.Witness.Launcher.Params
import Dscp.Witness.Listeners
import Dscp.Witness.Web
import Dscp.Witness.Workers

witnessEntry :: HasWitnessConfig => WitnessRealMode ()
witnessEntry =
    withServer $
    modifyLogName (<> "node") $ do
        -- todo git revision
        logInfo $ "Genesis header: " +| genesisHeader |+ ""

        logInfo "Forking workers"
        forM_ witnessWorkers $ void . async . runWorker identity

        logInfo "Forking listeners"
        forM_ witnessListeners $ void . async . runListener identity

        witnessParams <- view (lensOf @WitnessParams)
        logInfo "Forking wallet server"
        void . async $
            serveWitnessAPIReal (wpWalletServerParams witnessParams)

        logInfo "All done"
        forever $ liftIO $ threadDelay 10000000