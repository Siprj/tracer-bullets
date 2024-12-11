{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (void, forever)
import Data.Text (Text)
import qualified OpenTelemetry.Context as Context
import OpenTelemetry.Context.ThreadLocal (attachContext)
import OpenTelemetry.Trace hiding (inSpan)
import qualified OpenTelemetry.Trace as Trace
import OpenTelemetry.Trace.Sampler
import GHC.Stack
import OpenTelemetry.Processor.Batch
import UnliftIO
import OpenTelemetry.Exporter.OTLP (otlpExporter, loadExporterEnvironmentVariables)
import Data.Aeson
import OpenTelemetry.Instrumentation.HttpClient
import Network.HTTP.Types
import Control.Concurrent (threadDelay)


{- | Initialize the global tracing provider for the application and run an action
   (that action is generally the entry point of the application), cleaning
   up the provider afterwards.

   This also sets up an empty context (creating a new trace ID).
-}
withGlobalTracing :: Sampler -> IO a -> IO a
withGlobalTracing sampler act = do
  void $ attachContext Context.empty
  bracket
    (initializeTracing sampler)
    shutdownTracerProvider
    $ const act


initializeTracing :: Sampler -> IO TracerProvider
initializeTracing sampler = do
  (processors, tracerOptions') <- getTracerProviderInitializationOptions

  -- forcibly adds a stderr exporter; this is just for demo purposes
  otelExporter <- loadExporterEnvironmentVariables >>= otlpExporter
  stderrProc <- batchProcessor batchTimeoutConfig otelExporter
  let processors' = stderrProc : processors

  provider <- createTracerProvider processors' (tracerOptions' {tracerProviderOptionsSampler = sampler})
  setGlobalTracerProvider provider

  pure provider


inSpan :: (MonadUnliftIO m, HasCallStack) => Text -> SpanArguments -> m a -> m a
inSpan name args act = do
  tp <- getGlobalTracerProvider
  let tracer = makeTracer tp "haskell-example" tracerOptions
  Trace.inSpan tracer name args act


main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  loop

loop :: IO ()
loop = forever $ do
  withGlobalTracing alwaysOn $ do
    void $ inSpan "Run haskell example" defaultSpanArguments $ try @_ @SomeException $ do
      healthRequest <- parseRequest "http://rust-example:3000/health"
      let healthRequestCfg = HttpClientInstrumentationConfig
            { requestName = Just "is it good???"
            , requestHeadersToRecord = []
            , responseHeadersToRecord = []
            }
      manager <- newManager defaultManagerSettings
      healthResponse <- httpLbs healthRequestCfg healthRequest manager
      putStrLn $ "The status code for: " <> show healthRequest <> " was: " <> show (statusCode $ responseStatus healthResponse)


      let requestObject = object
           [ "name" .= ("Michael" :: Text)
           , "age"  .= (30 :: Int)
           ]

      initialRequest <- parseRequest "http://rust-example:3000/echo"
      let echoRequest = initialRequest { method = "GET", requestBody = RequestBodyLBS $ encode requestObject }

      let echoRequestCfg = HttpClientInstrumentationConfig
            { requestName = Just "do we get an echo???"
            , requestHeadersToRecord = []
            , responseHeadersToRecord = []
            }
      echoResponse <- httpLbs echoRequestCfg echoRequest manager
      putStrLn $ "The status code for: " <> show echoRequest <> " was: " <> show (statusCode $ responseStatus echoResponse)
      print $ responseBody echoResponse
  threadDelay 100000
