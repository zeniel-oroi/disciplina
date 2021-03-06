module Dscp.Witness.Web.Client.Error
    ( WitnessClientError (..)
    , servantToWitnessError
    ) where

import qualified Data.Text.Buildable as B
import Servant.Client (ServantError (..))
import qualified Text.Show

import Dscp.Web.Class
import Dscp.Witness.Web.Error

data WitnessClientError
    = WitnessClientError !WitnessAPIError
    | SomeClientError !Text

instance Show WitnessClientError where
    show = toString . pretty

instance Buildable WitnessClientError where
    build = \case
        WitnessClientError err -> B.build err
        SomeClientError msg -> B.build msg

instance Exception WitnessClientError

servantToWitnessError :: ServantError -> WitnessClientError
servantToWitnessError servantError =
    maybe (SomeClientError $ show servantError)
          WitnessClientError
          (fromServantError servantError)
