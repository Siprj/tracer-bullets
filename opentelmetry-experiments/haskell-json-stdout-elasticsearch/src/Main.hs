module Main
  ( main
  )
where

import Effectful
import Effectful.Log
import Effectful.Time
import System.Posix (usleep)
import Data.Aeson (Value(..), (.=), pairs, object)
import qualified Data.ByteString.Lazy.Char8 as BLS
import Effectful.Log.Logger
import Log (LogMessage(..), LogLevel (LogInfo), logInfo)
import Data.Aeson.Encoding (encodingToLazyByteString, Series)
import Data.Aeson.KeyMap (foldMapWithKey)
import Data.Text

withJsonOtelLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withJsonOtelLogger act = do
  logger <- Effectful.Log.Logger.mkLogger "json-otel" $ \LogMessage{..} -> liftIO $ do
    BLS.putStrLn . encodingToLazyByteString . pairs $
      "timestamp" .= lmTime
      <> "body" .= lmMessage
      <> "log.component" .= lmComponent
      <> "log.domain" .= lmDomain
      <> toSeries lmData
      <> "severity_text" .= lmLevel
  withLogger logger act
  where
    toSeries :: Value -> Series
    toSeries (Object object') = foldMapWithKey (\key val -> key .= val) object'
    toSeries (Array array) = "data.array" .= array
    toSeries (String string) = "data.string" .= string
    toSeries (Number number) = "data.number" .= number
    toSeries (Bool bool) = "data.bool" .= bool
    toSeries Null = mempty

runJsonOtelLogger
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runJsonOtelLogger component maxLogLevel k =
  withJsonOtelLogger $ \logger -> do
    runLogging component logger maxLogLevel k

main :: IO ()
main = runEff $ do
  runCurrentTimeIO . runJsonOtelLogger "main" LogInfo $ do
    app 0

app :: (Logging :> es, Time :> es, IOE :> es) => Int -> Eff es ()
app n = do
  time <- getCurrentTime
  logInfo "Hello !" $ object ["some_random_field" .= ("Some random text" :: Text), "number" .= n, "some_time" .= time]
  liftIO $ usleep 1000
  app (n + 1)
