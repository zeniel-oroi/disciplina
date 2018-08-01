{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Common (among witness and educator) CLI params.

module Dscp.CommonCLI
       ( logParamsParser
       , versionOption
       , keyParamsParser
       , serverParamsParser
       , networkAddressParser
       ) where

import Data.Char (toLower)
import Data.Version (showVersion)
import qualified Loot.Log as Log
import Options.Applicative (Parser, eitherReader, help, infoOption, long, metavar, option, str,
                            strOption, switch)
import Text.InterpolatedString.Perl6 (qc)
import Text.Parsec (eof, many1, parse, sepBy)
import Text.Parsec.Char (char, digit)
import qualified Text.Parsec.String as Parsec

import Dscp.Crypto (mkPassPhrase)
import Dscp.Resource.Keys (KeyParams (..))
import Dscp.Resource.Logging (LoggingParams (..))
import Dscp.Util (leftToFail)
import Dscp.Web (NetworkAddress (..), ServerParams (..))
import Paths_disciplina_witness (version)

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

versionOption :: Parser (a -> a)
versionOption = infoOption ("disciplina-" <> (showVersion version)) $
    long "version" <>
    help "Show version."

keyParamsParser :: Text -> Parser KeyParams
keyParamsParser who = do
    kpPath <- kpKeyPathParser
    kpGenNew <- kpGenKeyParser
    kpPassphrase <- kpPassphraseParser
    pure KeyParams{..}
  where
    kpKeyPathParser = optional . strOption $
         long [qc|{who}-keyfile-path|] <>
         metavar "FILEPATH" <>
         help [qc|Path to the secret key of {who}.|]
    kpGenKeyParser = switch $
         long [qc|{who}-generate-new-key|] <>
         help [qc|Generate the key and write it to '{who}-keyfile-path' path.|]
    kpPassphraseParser = optional . option passphraseReadM $
         long [qc|{who}-keyfile-password|] <>
         metavar "PASSWORD" <>
         help "Password of secret key."
    passphraseReadM = leftToFail . first pretty . mkPassPhrase =<< str

parseNetAddr :: String -> Either String NetworkAddress
parseNetAddr st =
    first niceError $ parse parseNA "" st
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

serverParamsParser :: String -> Parser ServerParams
serverParamsParser desc = do
    spAddr <- networkAddressParser (map toLower desc <> "-listen")
        ("Host/port for serving " <> desc <> " API")
    return ServerParams{..}