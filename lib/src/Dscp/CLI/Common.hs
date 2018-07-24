{-# LANGUAGE ApplicativeDo #-}

-- | Common CLI params

module Dscp.CLI.Common
       ( logParamsParser
       , rocksParamsParser
       , sqliteParamsParser
       , versionOption
       , keyParamsParser

       , peersParser
       , ourZTNodeIdParser
       , netCliParamsParser
       , netServParamsParser

       , networkAddressParser
       ) where

import qualified Data.Set as Set
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Loot.Network.ZMQ.Common (ZTNodeId (..), parseZTNodeId)
import Options.Applicative (Parser, eitherReader, help, infoOption, long, metavar, option, optional,
                            strOption, switch, value)
import qualified Text.Parsec.String as Parsec
import Text.Parsec (parse, many1, sepBy, eof)
import Text.Parsec.Char (digit, char)

import Dscp.DB.Rocks.Real.Types (RocksDBParams (..))
import Dscp.DB.SQLite.Types (SQLiteDBLocation (..), SQLiteParams (..))
import Dscp.Resource.Keys (KeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Resource.Network (NetCliParams (..), NetServParams (..))
import Dscp.Web (NetworkAddress (..))
import Paths_disciplina (version)

logParamsParser :: Log.Name -> Parser LoggingParams
logParamsParser lpDefaultName = do
    lpDebug <- logDebugParser
    lpConfigPath <- logConfigParser
    lpDirectory <- logDirParser
    return LoggingParams {..}
  where
    logDebugParser = switch $
        long "debug" <>
        help "Switch default logging level from Info to Debug"
    logConfigParser = optional $ strOption $
        long "log-config" <>
        metavar "FILEPATH" <>
        help "Path to logger configuration."
    logDirParser = optional $ strOption $
        long "log-dir" <>
        metavar "FILEPATH" <>
        help "Path to logs directory."

rocksParamsParser :: Parser RocksDBParams
rocksParamsParser = fmap RocksDBParams $ strOption $
    long "db-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for witness node." <>
    value "witness-db"

sqliteParamsParser :: Parser SQLiteParams
sqliteParamsParser = fmap (SQLiteParams . SQLiteReal) $ strOption $
    long "sql-path" <>
    metavar "FILEPATH" <>
    help "Path to database directory for educator's private data." <>
    value "educator-db"

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

keyParamsParser :: Parser KeyParams
keyParamsParser = do
    kpKeyPath <- kpKeyPathParser
    kpGenKey <- kpGenKeyParser
    pure KeyParams{..}
  where
    kpKeyPathParser =
        strOption $ long "key" <> help "Path to the secret key" <> metavar "FILEPATH"
    kpGenKeyParser =
        switch $ long "key-gen" <> help "Generate the key and write it to 'key' path"

----------------------------------------------------------------------------
-- ZMQ TCP
----------------------------------------------------------------------------

-- | Parse peers to connect to.
peersParser :: Parser (Set ZTNodeId)
peersParser =
    fmap Set.fromList $
    many $
    option (eitherReader parseZTNodeId)
           (long "peer" <> metavar "HOST:PORT1:PORT2" <> help "Peer(s) we should connect to")

-- | Parser for ZTNodeId we will bind on.
ourZTNodeIdParser :: Parser ZTNodeId
ourZTNodeIdParser = do
    option (eitherReader parseZTNodeId)
           (long "bind" <> metavar "HOST:PORT1:PORT2" <> help "Host/ports to bind on")

netCliParamsParser :: Parser NetCliParams
netCliParamsParser = NetCliParams <$> peersParser

netServParamsParser :: Parser NetServParams
netServParamsParser = NetServParams <$> peersParser <*> ourZTNodeIdParser

---------------------------------------------------------------------------
-- Utils
---------------------------------------------------------------------------

parseNetAddr :: String -> Either String NetworkAddress
parseNetAddr str =
    first niceError $ parse parseNA "" str
  where
    niceError = const "Invalid Network Address"
    parseNA :: Parsec.Parser NetworkAddress
    parseNA = NetworkAddress <$> parseHost <* char ':'
                             <*> parsePort <* eof
    parseHost = do host <- parseByte `sepBy` (char '.')
                   unless (length host == 4) $ fail "invalid"
                   return $ toText $ intercalate "." $ (map show host)
    parsePort = parseWord 16
    parseByte = parseWord 8 :: Parsec.Parser Integer
    parseWord n = do x <- fromMaybe (error "unexpected") . readMaybe <$> many1 digit
                     when ((x :: Integer) > 2 ^ (n :: Integer) - 1) $ fail "invalid"
                     return $ fromIntegral x

networkAddressParser :: String -> String -> Parser NetworkAddress
networkAddressParser pName helpTxt =
    option (eitherReader parseNetAddr) $
    long pName <>
    metavar "HOST:PORT" <>
    help helpTxt
