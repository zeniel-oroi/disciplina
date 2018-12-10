module Dscp.DB.SQLite.Types
       ( -- * SQLite bindings
         SQLiteRealParams
       , SQLiteRealParamsRec
       , SQLiteRealParamsRecP

       , SQLiteDBMode
       , SQLiteDBModeRec
       , SQLiteDBModeRecP

       , SQLiteParams
       , SQLiteParamsRec
       , SQLiteParamsRecP

       , SQLiteDB (..)
       ) where

import Control.Concurrent.Chan (Chan)
import Database.SQLite.Simple (Connection)
import Loot.Config ((::+), (:::), (::-), Config, PartialConfig)

----------------------------------------------------------
-- SQLite bindings
----------------------------------------------------------

type SQLiteRealParams =
   '[ "path"       ::: FilePath
      -- ^ Path to the file with database.
    , "connNum"    ::: Maybe Int
      -- ^ Connections pool size.
    , "maxPending" ::: Int
      -- ^ Maximal number of requests waiting for a free connection.
    ]

type SQLiteRealParamsRec = Config SQLiteRealParams
type SQLiteRealParamsRecP = PartialConfig SQLiteRealParams


-- | Database mode.
type SQLiteDBMode =
   '[ "real"     ::- SQLiteRealParams
      -- ^ In given file using given number of connections
    , "inMemory" ::- '[]
      -- ^ In memory
    ]

type SQLiteDBModeRec = Config SQLiteDBMode
type SQLiteDBModeRecP = PartialConfig SQLiteDBMode


-- | Wrapper for 'SQLiteDBMode', this helps passing the vinyl record around
type SQLiteParams =
   '[ "mode" ::+ SQLiteDBMode
    ]

type SQLiteParamsRec = Config SQLiteParams
type SQLiteParamsRecP = PartialConfig SQLiteParams


data SQLiteDB = SQLiteDB
    { sdConnPool   :: Chan Connection
      -- ^ Connections to given database. Each connection is used no more than
      -- one thread at once - requirement of SQLite.
    , sdConnNum    :: Int
      -- ^ Number of connections in pool
    , sdPendingNum :: TVar Int
      -- ^ Number of threads waiting for free connection.
    , sdMaxPending :: Int
      -- ^ Allowed number of pending threads.
    }
