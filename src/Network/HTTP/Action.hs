{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Action where

import Network.HTTP.Types

import Data.Types

-- Account
accountsAction :: Action
accountsAction =
    Action methodGet "/accounts" [] ""

accountAction :: AccountId -> Action
accountAction aId =
    Action methodGet ("/accounts/" ++ aId) [] ""

-- Product
productsAction :: Action
productsAction =
    Action methodGet "/products" [] ""

-- Order Book
orderBookAction :: ProductId -> Int -> Action
orderBookAction pId level =
    Action methodGet ("/products/" ++ pId ++ "/book") [("level", [show level])] ""

-- Trades
tradesAction :: ProductId -> Action
tradesAction pId =
    Action methodGet ("/products/" ++ pId ++ "/trades") [] ""