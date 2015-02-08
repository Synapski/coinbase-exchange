module Data.Types where

import Data.Fixed
import Network.HTTP.Types

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

type Amount = Micro
type Quantity = Micro

type AccountId = String
type ProductId = String