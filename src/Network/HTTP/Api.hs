module Network.HTTP.Api where

import Network.Wreq
import Control.Lens

import Data.Types
import Data.Coinbase
import Data.Json
import Data.Configuration
import Network.HTTP.Auth
import Network.HTTP.Action

-- Account
getAccounts :: ReaderConfigIO [Account]
getAccounts = do
    r <- asJSON =<< getAuthRequest accountsAction :: ReaderConfigIOResponse [Account]
    return (r ^. responseBody)

getAccount :: AccountId -> ReaderConfigIO Account
getAccount aId = do
    r <- asJSON =<< getAuthRequest (accountAction aId) :: ReaderConfigIOResponse Account
    return (r ^. responseBody)

-- Product
getProducts :: ReaderConfigIO [Product]
getProducts = do
    r <- asJSON =<< getAuthRequest productsAction :: ReaderConfigIOResponse [Product]
    return (r ^. responseBody)

-- Order Book
getOrderBook :: ProductId -> Int -> ReaderConfigIO OrderBook
getOrderBook pId level = do
    r <- asJSON =<< getAuthRequest (orderBookAction pId level) :: ReaderConfigIOResponse OrderBook
    return (r ^. responseBody)

-- Trades
getTrades :: ProductId -> ReaderConfigIO [Trade]
getTrades pId = do
    r <- asJSON =<< getAuthRequest (tradesAction pId) :: ReaderConfigIOResponse [Trade]
    return (r ^. responseBody)
