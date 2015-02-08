{-# LANGUAGE OverloadedStrings #-}

module Data.Json where

import Data.Aeson
import Data.Time.Format
import Data.Time.Clock
import Currency
import System.Locale
import Control.Applicative
import Control.Monad

import Data.Coinbase

instance FromJSON Account where
    parseJSON (Object v) =
        Account <$> v .: "id"
                <*> liftM read (v .: "balance")
                <*> liftM read (v .: "hold")
                <*> liftM read (v .: "available")
                <*> liftM NonStandardCurrency (v .: "currency")
    parseJSON _          = mzero

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

getTradeSide :: String -> TradeSide
getTradeSide "buy"  = BUY
getTradeSide "sell" = SELL

getTradeTime :: String -> UTCTime
getTradeTime time =
    readTime defaultTimeLocale "%F %T%Q+00" time

instance FromJSON Trade where
    parseJSON (Object v) = do
        Trade <$> v .: "trade_id"
              <*> liftM read (v .: "price")
              <*> liftM read (v .: "size")
              <*> liftM getTradeTime (v .: "time")
              <*> liftM getTradeSide (v .: "side")
    parseJSON _          = mzero
