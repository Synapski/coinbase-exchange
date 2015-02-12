module Main where

import Control.Monad.Reader

import Data.Configuration
import Network.HTTP.Api
import Network.WebSocket.Connection

main :: IO ()
main = do
    config <- loadConfiguration
    case config of
        Nothing -> print "Malformed configuration file"
        Just c  -> do
            runReaderT (run "BTC-USD") c
--            trades <- runReaderT (getTrades "BTC-USD") c
--            print trades

