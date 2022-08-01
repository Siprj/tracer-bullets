{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Log.FileRotate
  ( runJsonFileRotateLogger
  , withJsonFileRotateLogger
  )
where

import Effectful.Log.Logger
import Effectful
import Data.Text
import Effectful.Log
import Log (LogLevel, LogMessage (..))
import System.RotatingLog
import System.IO
import Data.ByteString
import Data.Aeson (Value(..), (.=), Series)
import Data.Aeson.Encoding (pairs, encodingToLazyByteString)
import Data.Aeson.KeyMap (foldMapWithKey)
--import Data.Time.Clock.POSIX
--import Data.Time


-- | Lifted 'LogBase.withJsonStdOutLogger'.
withJsonFileRotateLogger :: IOE :> es => (Logger -> Eff es a) -> Eff es a
withJsonFileRotateLogger act = do
  rotatingFile <- liftIO $ mkRotatingLog "logs/test-log-file" 1000000 LineBuffering (archiveFile "logs/archive")
  logger <- mkLogger "file-json" $ \LogMessage{..} -> liftIO $ do
    rotatedWrite rotatingFile . toStrict . encodingToLazyByteString . pairs $
      "timestamp" .= lmTime
      <> "body" .= lmMessage
      <> "log.component" .= lmComponent
      <> "log.domain" .= lmDomain
      <> toSeries lmData
      <> "severity_text" .= lmLevel
    rotatedWrite rotatingFile "\n"
  withLogger logger act
  where
    toSeries :: Value -> Series
    toSeries (Object object) = foldMapWithKey (\key val -> key .= val) object
    toSeries (Array array) = "data.array" .= array
    toSeries (String string) = "data.string" .= string
    toSeries (Number number) = "data.number" .= number
    toSeries (Bool bool) = "data.bool" .= bool
    toSeries Null = mempty

runJsonFileRotateLogger
  :: IOE :> es
  => Text
  -- ^ Application component name to use.
  -> LogLevel
  -- ^ The maximum log level allowed to be logged.
  -> Eff (Logging : es) a
  -- ^ The computation to run.
  -> Eff es a
runJsonFileRotateLogger component maxLogLevel k =
  withJsonFileRotateLogger $ \logger -> do
    runLogging component logger maxLogLevel k
