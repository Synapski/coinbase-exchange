{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Coinbase where

import Data.Time.Format
import Data.Time.Clock
import Currency
import GHC.Generics (Generic)

import Data.Types

-- Account
data Account = Account
    { accountId :: AccountId
    , balance   :: Amount
    , hold      :: Amount
    , available :: Amount
    , currency  :: Currency
    } deriving (Show,Generic)

-- Product
data Product = Product
    { productId      :: ProductId
    , baseCurrency   :: Currency
    , quoteCurrency  :: Currency
    , baseMinSize    :: Amount
    , baseMaxSize    :: Amount
    , quoteIncrement :: Quantity
    } deriving (Show,Generic)

-- Order Book
data Bid = Bid
    { bidPrice     :: Amount
    , bidSize      :: Quantity
    , bidNumOrders :: Int
    } deriving (Show,Generic)

data Ask = Ask
    { askPrice     :: Amount
    , askSize      :: Quantity
    , askNumOrders :: Int
    } deriving (Show,Generic)

data OrderBook = OrderBook
    { sequence :: Int
    , bids     :: [Bid]
    , asks     :: [Ask]
    } deriving (Show,Generic)

data TradeSide = BUY | SELL deriving (Show)

data Trade = Trade
    { tradeId    :: Int
    , tradePrice :: Amount
    , tradeSize  :: Quantity
    , tradeTime  :: UTCTime
    , tradeSide  :: TradeSide
    } deriving (Show,Generic)
