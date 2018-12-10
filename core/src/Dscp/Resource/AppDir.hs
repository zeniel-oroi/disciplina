{-# LANGUAGE OverloadedLabels #-}

-- | Application folder which we carry in config.
-- For now we create it strictly on application startup,
-- which makes sense because logs go there anyway.

module Dscp.Resource.AppDir
       ( AppDirParam
       , AppDirParamRec
       , AppDirParamRecP

       , AppDirConfig
       , AppDirConfigRec
       , AppDirConfigRecP

       , AppDir
       , getOSAppDir
       ) where

import Fmt ((+|), (|+))
import Loot.Config ((:::), (::-), (::+), Config, PartialConfig)
import System.Directory (XdgDirectory (XdgData), createDirectoryIfMissing, getXdgDirectory)
import System.Environment (lookupEnv)
import System.IO.Error (catchIOError, isDoesNotExistError, ioError)

import Dscp.Config
import Dscp.Resource.Class (AllocResource (..), buildComponentR)
import Dscp.System (appName)

-- | Which application directory to use.
type AppDirParam =
   '[ "os" ::- '[]
      -- ^ Dedicated folder inside OS directory for applications
    , "specific" ::- '[ "path" ::: FilePath ]
      -- ^ Given path
    ]

type AppDirParamRec = Config AppDirParam
type AppDirParamRecP = PartialConfig AppDirParam

-- | Wrapper for 'AppDirParam', this helps passing the vinyl record around
type AppDirConfig =
   '[ "param" ::+ AppDirParam
    ]

type AppDirConfigRec = Config AppDirConfig
type AppDirConfigRecP = PartialConfig AppDirConfig


type AppDir = FilePath

-- | Return folder for this application, which will be within directory next to
-- other applications in the system, e.g. "~/.local/share/disciplina".
getOSAppDir :: MonadIO m => m FilePath
getOSAppDir = liftIO $
    getXdgDirectory XdgData appName `catchIOError` \e ->
        -- only for `DoesNotExistError`s, this is to provide a fallback in case
        -- `$HOME` is not set
        if isDoesNotExistError e
        then fromMaybe "." <$> lookupEnv "PWD"
        else ioError e

-- | Create application directory if absent.
prepareAppDir
    :: MonadIO m
    => AppDirConfigRec -> m AppDir
prepareAppDir dirConfig = do
    appDir <- case dirConfig ^. tree #param . selection of
        "os"       -> getOSAppDir
        "specific" -> pure $ 
            dirConfig ^. tree #param . peekBranch #specific . option #path
        sel -> error $ "unknown AppDir type: " <> fromString sel
    -- we would unlikely have logging context here
    putTextLn $ "Application home directory will be at "+|appDir|+""
    liftIO $ createDirectoryIfMissing True appDir
    return appDir

instance AllocResource AppDir where
    type Deps AppDir = AppDirConfigRec
    allocResource p = buildComponentR "AppDir" (prepareAppDir p) (\_ -> pass)
