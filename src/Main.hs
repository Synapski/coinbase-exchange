module Main where

import Control.Monad.Reader (runReaderT)

import Data.Configuration
import Data.Coinbase
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

--            orderBook <- runReaderT (getOrderBook "BTC-USD" 3) c
--            print "BIDS"
--            mapM_ print (bids orderBook)
----            print $ fmap (\x -> (show x) ++ "\n") (bids orderBook)
--            print "ASKS"
--            mapM_ print (asks orderBook)
----            print $ fmap (\x -> (show x) ++ "\n") (asks orderBook)
----            print orderBook
--            print "done"

