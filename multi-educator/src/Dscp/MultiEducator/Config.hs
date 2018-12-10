{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All educator's configurations.

module Dscp.MultiEducator.Config
    ( MultiEducatorConfig
    , MultiEducatorConfigRec
    , HasMultiEducatorConfig
    , defaultMultiEducatorConfig
    , multiEducatorConfig
    , withMultiEducatorConfig
    , fillMultiEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, upcast)
import Time (Second, Time)

import Dscp.Config
import Dscp.DB.SQLite
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Config
import Dscp.MultiEducator.Launcher.Params (MultiEducatorKeyParams)
import Dscp.Witness.Config

type MultiEducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::< SQLiteParams
        , "keys" ::: MultiEducatorKeyParams
        , "api" ::< EducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        ]
     ]

type MultiEducatorConfigRecP = ConfigRec 'Partial MultiEducatorConfig
type MultiEducatorConfigRec = ConfigRec 'Final MultiEducatorConfig

type HasMultiEducatorConfig = Given MultiEducatorConfigRec

defaultMultiEducatorConfig :: MultiEducatorConfigRecP
defaultMultiEducatorConfig = upcast defaultWitnessConfig
    & sub #educator . sub #db .~ defSqliteParams
    & sub #educator . sub #api . sub #botParams .~ defBotParams
  where
    defSqliteParams :: SQLiteParamsRecP
    defSqliteParams = mempty
        & tree #mode . selection ?~ "real"
        & tree #mode . branch #real . option #path       ?~ "educator-db"
        & tree #mode . branch #real . option #connNum    ?~ Nothing
        & tree #mode . branch #real . option #maxPending ?~ 200
    defBotParams :: EducatorBotParamsRecP
    defBotParams = mempty
        & option #enabled         ?~ False
        & option #seed            ?~ "Memes generator"
        & option #operationsDelay ?~ 0

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

multiEducatorConfig :: HasMultiEducatorConfig => MultiEducatorConfigRec
multiEducatorConfig = given

withMultiEducatorConfig :: MultiEducatorConfigRec -> (HasMultiEducatorConfig => a) -> a
withMultiEducatorConfig = give

fillMultiEducatorConfig :: MultiEducatorConfigRecP -> IO MultiEducatorConfigRecP
fillMultiEducatorConfig = fillExpandedConfig fillWitnessConfig
