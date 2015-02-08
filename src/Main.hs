module Main where

import Control.Monad.Reader

import Data.Configuration
import Network.Api.Coinbase

main :: IO ()
main = do
    config <- loadConfiguration
    case config of
        Nothing -> print "Malformed configuration file"
        Just c  -> do
            trades <- runReaderT (getTrades "BTC-USD") c
            print trades

