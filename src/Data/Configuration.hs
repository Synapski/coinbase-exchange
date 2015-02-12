{-# LANGUAGE OverloadedStrings #-}

module Data.Configuration where

import Data.String.Conversions (SBS)
import qualified Data.Configurator as C
import Network.Wreq            (Response)
import Control.Applicative
import Control.Monad.Reader

import Data.Types

data Configuration = Configuration
    { httpBaseUrl :: URL
    , httpKey     :: String
    , httpSecret  :: SBS
    , httpPass    :: String
    , wsUrl       :: URL
    , wsPort      :: String
    , wsPath      :: String
    }

type ReaderConfig a = Reader Configuration a
type ReaderConfigM m a = ReaderT Configuration m a
type ReaderConfigIO a = ReaderConfigM IO a
type ReaderConfigIOResponse a = ReaderConfigIO (Response a)

loadConfiguration :: IO (Maybe Configuration)
loadConfiguration = do
    config <- C.load [ C.Required "app.cfg" ]
    hu  <- C.lookup config "api-http-coinbase.url"
    hk  <- C.lookup config "api-http-coinbase.key"
    hs  <- C.lookup config "api-http-coinbase.secret"
    hp  <- C.lookup config "api-http-coinbase.pass"
    wu  <- C.lookup config "api-ws-coinbase.url"
    wp  <- C.lookup config "api-ws-coinbase.port"
    wpp <- C.lookup config "api-ws-coinbase.path"
    return $ Configuration <$> hu <*> hk <*> hs <*> hp <*> wu <*> wp <*> wpp
