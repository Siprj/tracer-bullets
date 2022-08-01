module Main
  ( main
  )
where

import Effectful
import Effectful.Log
import Effectful.Time
import Log
import Data.Text
import Log.FileRotate
import System.Posix

main :: IO ()
main = runEff $ do
  runCurrentTimeIO . runJsonFileRotateLogger "main" LogInfo $ do
    app 0

app :: (Logging :> es, Time :> es, IOE :> es) => Int -> Eff es ()
app n = do
  time <- getCurrentTime
  logInfo "Hello !" $ object ["some_random_field" .= ("Some random text" :: Text), "number" .= n, "some_time" .= time]
  liftIO $ usleep 1000
  app (n + 1)
