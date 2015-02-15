{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Data.Coinbase where

import Data.Time.Format
import Data.Time.Clock
import Currency
import GHC.Generics (Generic)

import Data.Types

type AccountId = String
type ProductId = String
type TradeId   = Int
type OrderId   = String

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
    { orderBookSequence :: Int
    , bids              :: [Bid]
    , asks              :: [Ask]
    } deriving (Show,Generic)

data Side = BUY | SELL deriving (Show)

data Trade = Trade
    { tradeId    :: TradeId
    , tradePrice :: Amount
    , tradeSize  :: Quantity
    , tradeTime  :: UTCTime
    , tradeSide  :: Side
    } deriving (Show,Generic)

-- Market Data

data DoneReason = Filled | Canceled deriving (Show)

data MarketData =
    Received
    { sequence          :: Int
    , receivedOrderId   :: OrderId
    , receivedSize      :: Quantity
    , receivedPrice     :: Amount
    , receivedSide      :: Side
    }
  | Open
    { sequence          :: Int
    , openOrderId       :: OrderId
    , openPrice         :: Amount
    , openRemainingSize :: Quantity
    , openSide          :: Side
    }
  | Done
    { sequence          :: Int
    , donePrice         :: Amount
    , doneOrderId       :: OrderId
    , doneReason        :: DoneReason
    , doneSide          :: Side
    , doneRemainingSize :: Quantity
    }
  | Match
    { matchTradeId      :: TradeId
    , sequence          :: Int
    , makerOrderId      :: OrderId
    , takerOrderId      :: OrderId
    , matchTime         :: UTCTime
    , matchSize         :: Quantity
    , matchPrice        :: Amount
    , matchSide         :: Side
    }
  | Change
    { sequence          :: Int
    , changeOrderId     :: OrderId
    , changeTime        :: UTCTime
    , changeNewSize     :: Quantity
    , changeOldSize     :: Quantity
    , changePrice       :: Amount
    , changeSide        :: Side
    } deriving (Show,Generic)

