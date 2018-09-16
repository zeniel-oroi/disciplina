{-# LANGUAGE TypeOperators #-}

-- | All educator's configurations.

module Dscp.Educator.Config
    ( EducatorConfig
    , EducatorConfigRec
    , HasEducatorConfig
    , withEducatorConfig
    , fillEducatorConfig

    , module Dscp.Witness.Config
    ) where

import Data.Reflection (Given, give)
import Loot.Config (ConfigKind (Final, Partial), ConfigRec)

import Dscp.Config
import Dscp.Witness.Config

type EducatorConfig = WitnessConfig +++
    '[
     ]

type EducatorConfigRecP = ConfigRec 'Partial EducatorConfig
type EducatorConfigRec = ConfigRec 'Final EducatorConfig

type HasEducatorConfig = Given EducatorConfigRec

withEducatorConfig :: EducatorConfigRec -> (HasEducatorConfig => a) -> a
withEducatorConfig = give

fillEducatorConfig :: EducatorConfigRecP -> IO EducatorConfigRecP
fillEducatorConfig = fillWitnessConfig
