{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedRecordDot #-}


module Main (
  main,
) where

import Control.Concurrent (threadDelay)
import Control.Monad
import Control.Monad.Catch (Exception, MonadThrow (throwM), SomeException, try)
import Data.Text qualified as T
import Effectful
import Otel.Client
import Otel.Effect
    ( logInfo_,
      logWarn_,
      runOtel,
      traceInternal_,
      withInstrumentationScope,
      spanLink, Otel, traceClient )
import Otel.Type (ResourceAttributes, SpanLink(spanId, traceId))
import Prelude hiding (log)
import GHC.Stack (HasCallStack)
import Network.HTTP.Client
import Network.HTTP.Types (Status(statusCode), RequestHeaders)
import Otel.Internal.Type (Scope(Scope), fromTraceId, fromSpanId, TraceData (..), SpanKind (Internal), KeyValue (KeyValue), Value (..))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as B
import Data.ByteArray.Encoding (convertToBase, Base (Base16))

myAppScope :: Scope "haskell-otel-example" "0.0.0.1"
myAppScope = Scope

-------------------------------------------------------------------------------
-- W3C TraceContext
-------------------------------------------------------------------------------
encodeTraceContext :: (HasCallStack, Otel :> es) => Eff es RequestHeaders
encodeTraceContext = do
  link <- spanLink
  case link of
    Nothing -> pure []
    Just link' -> pure [("traceparent", L.toStrict . B.toLazyByteString $ traceparentHeader link'), ("ce87d8ba50db5b19", "")]
  where
    traceparentHeader :: SpanLink -> B.Builder
    traceparentHeader link =
      -- version
      B.word8HexFixed 0
        <> B.char7 '-'
        <> B.byteString (convertToBase Base16 (fromTraceId link.traceId))
        <> B.char7 '-'
        <> B.byteString (convertToBase Base16 (fromSpanId link.spanId))
        <> B.char7 '-'
        <> B.word8HexFixed traceFlags
    traceFlags = 1;

data TestException = TestException
  deriving (Show)

instance Exception TestException

requestHealth :: (HasCallStack, Otel :> es, IOE :> es) => Manager -> Eff es ()
requestHealth manager = do
  void . try @_ @SomeException $ do
    traceClient "httpRequest"
      [ KeyValue "http.host" $ StringV "rust-example"
      , KeyValue "http.flavor" $ StringV "1.1"
      , KeyValue "http.schema" $ StringV "http"
      , KeyValue "http.method" $ StringV "GET"
      , KeyValue "http.status_code" $ IntV 200
      , KeyValue "http.target" $ StringV "/health"
      , KeyValue "http.url" $ StringV "http://rust-example:3000/health"
      ] mempty $ do
        initialHealthRequest <- parseRequest "http://rust-example:3000/health"

        straceHeaders <- encodeTraceContext
        let healthRequest = initialHealthRequest {
            requestHeaders = requestHeaders initialHealthRequest <> straceHeaders
        }
        healthResponse <- liftIO $ httpLbs healthRequest manager
        liftIO . putStrLn $ "The status code for: " <> show healthRequest <> " was: " <> show (statusCode $ responseStatus healthResponse)


main :: IO ()
main = do
  let clientConfiguration = defautOtelClientParameters
        { logEndpoint = "http://opentelemetry-collector:4318/v1/logs"
        , traceEndpoint = "http://opentelemetry-collector:4318/v1/traces"
        }
  otelClient <- startOtelClient resourceAttributes clientConfiguration
  app otelClient
  where
    app :: OtelClient -> IO ()
    app otelClient = forever $ do
      runEff $ runOtel otelClient (Just $ TraceData "Root span" Internal mempty mempty) $ traceInternal_ "Outer span" $ do
        manager <- liftIO $ newManager defaultManagerSettings
        mapM_ (\v -> logInfo_ $ "Super cool message in default scope " <> T.pack (show @Int v)) [1 .. 3]
        withInstrumentationScope myAppScope $ do
          traceInternal_ "Inside span" $ do
            requestHealth manager
            mapM_ (\v -> logWarn_ $ "Super cool message in my app scope " <> T.pack (show @Int v)) [1 .. 3]
          traceInternal_ "Some internal span" $ do
            void . try @_ @SomeException $ do
              traceInternal_ "Second internal span" $ do
                logInfo_ "Nice message before throw"
                void $ throwM TestException
                logInfo_ "Nice message after throw"

      liftIO $ threadDelay 100000

resourceAttributes :: ResourceAttributes
resourceAttributes =
  [ KeyValue "service.name" $ StringV "haskell-otel-example"
  , KeyValue "container_name" $ StringV "haskell-otel-example"
  ]
