{-# LANGUAGE OverloadedStrings #-}

module Network.HTTP.Auth where

import Data.String.Conversions (SBS,LBS,cs)
import Data.Time.Clock.POSIX
import Data.Digest.Pure.SHA
import Data.ByteString.Base64
import Network.Wreq            (Options,getWith,header,param,defaults)
import Network.HTTP.Types
import Control.Lens
import Control.Monad.Reader

import Data.Types
import Data.Configuration

getAccessSign :: Monad m => Timestamp -> Method -> URL -> Body -> ReaderConfigM m SBS
getAccessSign timestamp method url body = do
    config <- ask
    let
        s = cs $ decodeLenient $ httpSecret config
        message = cs $ (show timestamp) ++ (cs method) ++ url ++ body
    return $ encode $ cs $ bytestringDigest $ hmacSha256 s message

getHeaders :: Monad m => Timestamp -> Method -> URL -> Body -> ReaderConfigM m [(Options -> Options)]
getHeaders timestamp method uri body = do
    config <- ask
    accessSign <- getAccessSign timestamp method uri body
    return [ header "CB-ACCESS-KEY" .~ [cs $ httpKey config]
           , header "CB-ACCESS-SIGN" .~ [accessSign]
           , header "CB-ACCESS-TIMESTAMP" .~ [timestampToByteString timestamp]
           , header "CB-ACCESS-PASSPHRASE" .~ [cs $ httpPass config]
           ]

getParams :: Params -> [(Options -> Options)]
getParams params =
    fmap (\(pKey,pValues) -> param (cs pKey) .~ fmap (cs) pValues) params

getCurrentTimestamp :: IO Timestamp
getCurrentTimestamp =
    fmap round getPOSIXTime

timestampToByteString :: Timestamp -> SBS
timestampToByteString timestamp =
    cs $ show timestamp

getRequestOpts :: Method -> URL -> Params -> Body -> ReaderConfigIO Options
getRequestOpts method url params body = do
    timestamp <- liftIO $ getCurrentTimestamp
    h <- getHeaders timestamp method url body
    let
        p = getParams params
    return $ foldl (&) defaults (h ++ p)

getAuthRequest :: Action -> ReaderConfigIOResponse LBS
getAuthRequest (Action method url params body) = do
    config <- ask
    opts <- getRequestOpts method url params body
    let
        fullUrl = httpBaseUrl config ++ url
    liftIO $ getWith opts fullUrl
