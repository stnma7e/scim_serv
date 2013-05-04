{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Handler.Scim.Auth (
    scimAuthHandler
) where

import Handler (HandlerFunc)
import ByteStringTools

import Network.Socket
import Network.Socket.ByteString (sendAllTo, recvFrom)
import qualified Data.ByteString.Lazy as L (ByteString, take, pack, unpack, length)
import qualified Data.ByteString as B (ByteString, pack)
import Data.Word (Word8)
import Database.HDBC
import Database.HDBC.PostgreSQL

scimAuthHandler :: HandlerFunc
scimAuthHandler sock addr pkt = 
    scimHandler sock addr pkt (take 2 (L.unpack pkt))

scimHandler :: Socket -> SockAddr -> L.ByteString -> [Word8] -> IO ()
scimHandler sock addr pkt [0x00, 0x00] = do
    conn <- connectPostgreSQL "dbname=scim"
    let pktL    = L.unpack pkt
    let name    = take (fromIntegral (pktL!!2 :: Word8)) (drop 4 pktL)
    let passwd  = take (fromIntegral (pktL!!3 :: Word8)) (drop 36 pktL)
    r <- quickQuery' conn
        "SELECT id FROM account WHERE email=? AND password=?" [toSql (B.pack name), toSql (B.pack passwd)]
    putStrLn $ show (B.pack name) ++ "\n"
               ++ show (B.pack passwd)
    mapM_ putStrLn (map convRow r)
    let res = map convRow r
    if (res /= [])
        then sendAllTo sock (B.pack [1]) addr
        else sendAllTo sock (B.pack ([0x00, 0x00, 0x01] :: [Word8])) addr
    disconnect conn
  where convRow [id] =
            show (desc id)
        convRow [id, passwd] =
            show (desc id) ++ ": " ++ show (desc passwd)
        desc :: SqlValue -> String
        desc sqlDesc = case fromSql sqlDesc of
            Just x -> x
            Nothing -> "NULL"

scimHandler sock addr pkt _ =
    sendAllTo sock "invalid packet type" addr