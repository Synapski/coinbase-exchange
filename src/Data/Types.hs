{-# LANGUAGE DeriveGeneric #-}

module Data.Types where

import Data.Fixed
import Network.HTTP.Types
import GHC.Generics (Generic)

type URL = String
type Body = String
type Params = [(String, [String])]
type Timestamp = Int

data Action = Action
    { actionMethod :: Method
    , actionUrl    :: URL
    , actionParams :: Params
    , actionBody   :: Body
    }

data Subscribe = Subscribe
    { productId :: ProductId
    } deriving (Show,Generic)

type Amount = Micro
type Quantity = Micro

type AccountId = String
type ProductId = String