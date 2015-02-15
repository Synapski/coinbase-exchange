{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Network.WebSocket.Connection where

import           Control.Concurrent    (forkIO)
import           Control.Applicative   ((<$>))
import           Control.Monad         (forever, unless)
import           Control.Monad.Trans   (liftIO)
import           Control.Monad.Reader
import           Data.Aeson
import qualified Data.ByteString.Lazy  as B (toStrict)
import           Data.String.Conversions (cs)
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import qualified Network.WebSockets    as WS
import qualified Network.WebSockets.Stream as WS
import qualified OpenSSL               as SSL
import qualified OpenSSL.Session       as SSL
import qualified System.IO.Streams     as Streams
import qualified System.IO.Streams.SSL as Streams
import qualified Network.Socket        as S
import           GHC.Generics (Generic)

import Data.Configuration
import Data.Coinbase
import Data.Types
import Data.Json

data Subscribe = Subscribe
    { productId :: ProductId
    } deriving (Show,Generic)

instance ToJSON Subscribe where
    toJSON (Subscribe productId) =
        object [ "type" .= ("subscribe" :: String)
               , "product_id" .= productId
               ]

feed :: ProductId -> WS.ClientApp ()
feed productId conn = do
    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        msg <- WS.receiveData conn
--        liftIO $ T.putStrLn msg
--        let m = (decode msg) :: Maybe Received
--        let m = (decode msg) :: Maybe Object
        let m = (eitherDecode msg) :: Either String MarketData
        case m of
            Right o  -> liftIO $ print o
            Left err -> liftIO $ print $ err ++ " in " ++ cs msg

    WS.sendTextData conn $ encode $ toJSON (Subscribe productId)


    -- Read from stdin and write to WS
    let loop = do
            line <- T.getLine
            unless (T.null line) $ WS.sendTextData conn line >> loop

    loop
    WS.sendClose conn ("Bye!" :: Text)


run :: ProductId -> ReaderConfigIO ()
run productId = do
    config <- ask
    let
        host = wsUrl config
        port = wsPort config
        path = wsPath config

    liftIO $ SSL.withOpenSSL $ do
        ctx <- SSL.context
        is  <- S.getAddrInfo Nothing (Just host) (Just port)
        let
            a = S.addrAddress $ head is
            f = S.addrFamily $ head is
        s <- S.socket f S.Stream S.defaultProtocol
        S.connect s a
        ssl <- SSL.connection ctx s
        SSL.connect ssl
        (i,o) <- Streams.sslToStreams ssl
        stream <- WS.makeStream (Streams.read i)
                  (\b -> Streams.write (B.toStrict <$> b) o)
        WS.runClientWithStream stream host path WS.defaultConnectionOptions [] $ feed productId
