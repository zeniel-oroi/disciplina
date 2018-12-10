module Dscp.Educator.Web.Bot.Params
    ( EducatorBotParams
    , EducatorBotParamsRec
    , EducatorBotParamsRecP
    ) where

import Loot.Config ((:::), Config, PartialConfig)
import Time.Units (Microsecond, Time)

-- | Which params to use when launching bot.
-- Bool flag is used instead of `Maybe` value or custom sum type
-- to allow for specifying default values for bot params in
-- default config even though bot should be disabled by default.
type EducatorBotParams =
   '[ "enabled" ::: Bool
      -- ^ Whether or not the bot is enabled
    , "seed" ::: Text
      -- ^ Seed to generate initial data (assignments, ...).
    , "operationsDelay" ::: Time Microsecond
      -- ^ Artificial delay in bot operations.
    ]

type EducatorBotParamsRec = Config EducatorBotParams
type EducatorBotParamsRecP = PartialConfig EducatorBotParams
