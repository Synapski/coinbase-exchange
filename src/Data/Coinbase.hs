{-# LANGUAGE DeriveGeneric DeriveData 
DeriveFunctor DeriveFoldable DeriveTraversable  DeriveTypeable #-}

module Data.Coinbase where

import Data.Time.Format
import Data.Time.Clock
import Currency
import GHC.Generics (Generic)
import qualified Data.Text as T
import Data.Text (Text)

import Data.Types
import Data.Int

newtype AccountId = AccountId {_accountId ::Text}
newtype ProductId = ProductId Text
newtype TradeId   = TradeId Int64
newtype OrderId   = Order Text

-- Account
data Account = Account
    { accountId :: AccountId
    , balance   :: Amount
    , hold      :: Amount
    , available :: Amount
    , currency  :: Currency
    } deriving (Show,Eq,Data,Generic)

-- Product
data Product = Product
    { productId      :: ProductId
    , baseCurrency   :: Currency
    , quoteCurrency  :: Currency
    , baseRangeSize    :: (Amount,Amount)
    --, baseMaxSize    :: Amount
    , quoteIncrement :: Quantity
    } deriving (Show,Eq,Generic)

-- Order Book
data Bid = Bid
    { bidPrice     :: Amount
    , bidSize      :: Quantity
    , bidOrderId   :: OrderId
    } deriving (Show,Generic)

data Ask = Ask
    { askPrice     :: Amount
    , askSize      :: Quantity
    , askOrderId   :: OrderId
    } deriving (Show,Generic)

data OrderBook = OrderBook
    { orderBookSequence :: Int
    , bids              :: [Bid]
    , asks              :: [Ask]
    } deriving (Show,Generic)

data Side = BUY | SELL deriving (Show,Eq)

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
    , orderId           :: OrderId
    , receivedSize      :: Quantity
    , receivedPrice     :: Amount
    , receivedSide      :: Side
    }
  | Open
    { sequence          :: Int
    , orderId           :: OrderId
    , openPrice         :: Amount
    , openRemainingSize :: Quantity
    , openSide          :: Side
    }
  | Done
    { sequence          :: Int
    , donePrice         :: Amount
    , orderId           :: OrderId
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
    , orderId           :: OrderId
    , changeTime        :: UTCTime
    , changeNewSize     :: Quantity
    , changeOldSize     :: Quantity
    , changePrice       :: Amount
    , changeSide        :: Side
    } deriving (Show,Generic)
