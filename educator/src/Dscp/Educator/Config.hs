{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators    #-}

-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , EducatorConfigRec
    , EducatorConfigRecP
    , HasEducatorConfig
    , defaultEducatorConfig
    , educatorConfig
    , withEducatorConfig
    , fillEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Control.Lens ((?~))
import Data.Reflection (Given, give, given)
import Loot.Config ((:::), (::<), ConfigKind (Final, Partial), ConfigRec, upcast)
import Time (Second, Time)

import Dscp.Config
import Dscp.DB.SQLite
import Dscp.Educator.Launcher.Params
import Dscp.Educator.Web.Bot.Params
import Dscp.Educator.Web.Config
import Dscp.Witness.Config

type EducatorConfig = WitnessConfig ++
    '[ "educator" ::<
       '[ "db" ::< SQLiteParams
        , "keys" ::< EducatorKeyParams
        , "api" ::< EducatorWebConfig
        , "publishing" ::<
           '[ "period" ::: Time Second
            ]
        ]
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = (Given EducatorConfigRec, HasWitnessConfig)

defaultEducatorConfig :: EducatorConfigRecP
defaultEducatorConfig = upcast defaultWitnessConfig
    & sub #educator . sub #db .~ defSqliteParams
    & sub #educator . sub #keys . sub #keyParams .~ defBaseKeyParams
    & sub #educator . sub #api . sub #botParams .~ defBotParams
  where
    defSqliteParams :: SQLiteParamsRecP
    defSqliteParams = mempty
        & tree #mode . selection ?~ "real"
        & tree #mode . branch #real . option #path       ?~ "educator-db"
        & tree #mode . branch #real . option #connNum    ?~ Nothing
        & tree #mode . branch #real . option #maxPending ?~ 200
    defBaseKeyParams = mempty
        & option #path       ?~ Nothing
        & option #genNew     ?~ False
        & option #passphrase ?~ Nothing
    defBotParams :: EducatorBotParamsRecP
    defBotParams = mempty
        & option #enabled         ?~ False
        & option #seed            ?~ "Memes generator"
        & option #operationsDelay ?~ 0

-- instance (HasEducatorConfig, cfg ~ WitnessConfigRec) => Given cfg where
--     given = rcast (given @EducatorConfigRec)

educatorConfig :: HasEducatorConfig => EducatorConfigRec
educatorConfig = given

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig conf a = give (rcast @_ @WitnessConfig conf) $ give conf a

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = fillExpandedConfig fillWitnessConfig
