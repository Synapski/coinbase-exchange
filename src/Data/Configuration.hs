{-# LANGUAGE OverloadedStrings #-}

module Data.Configuration where

import Data.String.Conversions (SBS)
import qualified Data.Configurator as C
import Network.Wreq            (Response)
import Control.Applicative
import Control.Monad.Reader

import Data.Types

data Configuration = Configuration
    { baseUrl :: URL
    , key     :: String
    , secret  :: SBS
    , pass    :: String
    }

type ReaderConfig a = Reader Configuration a
type ReaderConfigM m a = ReaderT Configuration m a
type ReaderConfigIO a = ReaderConfigM IO a
type ReaderConfigIOResponse a = ReaderConfigIO (Response a)

loadConfiguration :: IO (Maybe Configuration)
loadConfiguration = do
    config <- C.load [ C.Required "app.cfg" ]
    u <- C.lookup config "api-coinbase.url"
    k <- C.lookup config "api-coinbase.key"
    s <- C.lookup config "api-coinbase.secret"
    p <- C.lookup config "api-coinbase.pass"

    return $ Configuration <$> u <*> k <*> s <*> p
