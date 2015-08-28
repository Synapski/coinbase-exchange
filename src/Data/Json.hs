{-# LANGUAGE OverloadedStrings #-}

module Data.Json where

import Data.Aeson
import Data.Time.Format
import Data.Time.Clock
import Currency
import Control.Applicative
import Control.Monad
import qualified Data.HashMap.Strict as Hm

import Data.Coinbase
import Data.Types

-- Private

instance FromJSON Account where
    parseJSON (Object v) =
        Account <$> v .: "id"
                <*> liftM read (v .: "balance")
                <*> liftM read (v .: "hold")
                <*> liftM read (v .: "available")
                <*> liftM NonStandardCurrency (v .: "currency")
    parseJSON _          = mzero

-- Market Data

instance FromJSON Product where
    parseJSON (Object v) =
        Product <$> v .: "id"
                <*> liftM NonStandardCurrency (v .: "base_currency")
                <*> liftM NonStandardCurrency (v .: "quote_currency")
                <*> v .: "base_min_size"
                <*> v .: "base_max_size"
                <*> v .: "quote_increment"
    parseJSON _          = mzero

instance FromJSON Bid where
    parseJSON v = do
        (price, size, numOrders) <- parseJSON v
        return $ Bid (read price) (read size) numOrders

instance FromJSON Ask where
    parseJSON v = do
        (price, size, numOrders) <- parseJSON v
        return $ Ask (read price) (read size) numOrders

instance FromJSON OrderBook where
    parseJSON (Object v) = do
        OrderBook <$> v .: "sequence"
                  <*> v .: "bids"
                  <*> v .: "asks"
    parseJSON _          = mzero

getSide :: String -> Side
getSide "buy"  = BUY
getSide "sell" = SELL

getTime :: String -> UTCTime
getTime time =
    readTime defaultTimeLocale "%FT%T%QZ" time

instance FromJSON Trade where
    parseJSON (Object v) = do
        Trade <$> v .: "trade_id"
              <*> liftM read (v .: "price")
              <*> liftM read (v .: "size")
              <*> liftM getTime (v .: "time")
              <*> liftM getSide (v .: "side")
    parseJSON _          = mzero


-- Real Time Market Data

instance FromJSON MarketData where
    parseJSON (Object v) =
        case Hm.lookup "type" v of
            Just "received" -> parseJSONReceived v
            Just "open"     -> parseJSONOpen v
            Just "done"     -> parseJSONDone v
            Just "match"    -> parseJSONMatch v
            Just "change"   -> parseJSONChange v
            Just t          -> fail $ "Non recognized type " ++ (show t)
            Nothing         -> fail "Missing type"
    parseJSON _          = mzero


parseJSONReceived v = do
    Received <$> v .: "sequence"
             <*> v .: "order_id"
             <*> liftM read (v .: "size")
             <*> liftM read (v .: "price")
             <*> liftM getSide (v .: "side")

parseJSONOpen v = do
    Open <$> v .: "sequence"
         <*> v .: "order_id"
         <*> liftM read (v .: "price")
         <*> liftM read (v .: "remaining_size")
         <*> liftM getSide (v .: "side")

getDoneReason :: String -> DoneReason
getDoneReason "filled"   = Filled
getDoneReason "canceled" = Canceled

parseJSONDone v = do
    Done <$> v .: "sequence"
         <*> liftM read (v .: "price")
         <*> v .: "order_id"
         <*> liftM getDoneReason (v .: "reason")
         <*> liftM getSide (v .: "side")
         <*> liftM read (v .: "remaining_size")

parseJSONMatch v = do
    Match <$> v .: "trade_id"
          <*> v .: "sequence"
          <*> v .: "maker_order_id"
          <*> v .: "taker_order_id"
          <*> liftM getTime (v .: "time")
          <*> liftM read (v .: "size")
          <*> liftM read (v .: "price")
          <*> liftM getSide (v .: "side")

parseJSONChange v = do
    Change <$> v .: "sequence"
           <*> v .: "order_id"
           <*> liftM getTime (v .: "time")
           <*> liftM read (v .: "new_size")
           <*> liftM read (v .: "old_size")
           <*> liftM read (v .: "price")
           <*> liftM getSide (v .: "side")
