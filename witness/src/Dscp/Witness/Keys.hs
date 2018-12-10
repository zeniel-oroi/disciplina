{-# LANGUAGE StrictData #-}

module Dscp.Witness.Keys
       ( WitnessKeyParams
       , WitnessKeyParamsRec
       , WitnessKeyParamsRecP

       , WitnessKeysParams
       , WitnessKeysParamsRec
       , WitnessKeysParamsRecP
       ) where

import Loot.Config ((::+), (::-), Config, PartialConfig)

import Dscp.Resource.Keys

-- | Witness key parameters.
type WitnessKeyParams =
   '[ "basic" ::- BaseKeyParams
      -- ^ Basic key management with a keyfile
    , "committee" ::- CommitteeKeyParams
      -- ^ Generate a key from committee params
    ]

type WitnessKeyParamsRec = Config WitnessKeyParams
type WitnessKeyParamsRecP = PartialConfig WitnessKeyParams

-- | Wrapper for 'WitnessKeyParams', this helps passing the vinyl record around
type WitnessKeysParams =
   '[ "params" ::+ WitnessKeyParams
    ]

type WitnessKeysParamsRec = Config WitnessKeysParams
type WitnessKeysParamsRecP = PartialConfig WitnessKeysParams
