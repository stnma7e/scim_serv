{-# LANGUAGE OverloadedStrings #-}

module Handler.Scim.Auth (
    scimAuthHandler
) where

import Handler (HandlerFunc)
import ByteStringTools

import Network.Socket
import Network.Socket.ByteString (sendAllTo, recvFrom)
import qualified Data.ByteString.Lazy as L (ByteString, take, pack, unpack, length, intercalate)
import qualified Data.ByteString as B (ByteString, pack, intercalate, length)
import qualified Data.ByteString.Char8 as C (pack)
import Data.Word (Word8, Word32)
import Data.Binary (encode)
import Data.Bits (shiftL, (.|.))
import Database.HDBC
import Database.HDBC.PostgreSQL
import Data.ByteString.Internal (w2c, c2w)

scimAuthHandler :: HandlerFunc
scimAuthHandler sock addr pkt = 
    scimHandler sock addr pkt (take 2 (L.unpack pkt))

scimHandler :: Socket -> SockAddr -> L.ByteString -> [Word8] -> IO ()
scimHandler sock addr pkt [0x00, 0x00] = do
    conn <- connectPostgreSQL "dbname=scim_auth"
    let pktL    = L.unpack pkt
    let name    = take (fromIntegral (pktL!!2 :: Word8)) (drop 4 pktL)
    let passwd  = take (fromIntegral (pktL!!3 :: Word8)) (drop 36 pktL)
    r <- quickQuery' conn
        "SELECT id FROM account WHERE email=? AND password=?" [toSql (B.pack name), toSql (B.pack passwd)]
    putStrLn $ show (B.pack name) ++ "\n"
               ++ show (B.pack passwd)
    let res = map convRow r -- returns [[SqlValue]]; in this case will return [[SqlValue id], [SqlValue id], ...]
    mapM_ putStrLn res
    let id = read (res!!0) :: Word8 -- reads id; should only be one element in list e.g. [SqlValue id]
    if (res /= [])
        then do r <- quickQuery' conn "UPDATE account SET online=true WHERE id=?" [SqlInteger (fromIntegral id)]
                sendAllTo sock (B.pack [id]) addr
        else sendAllTo sock (B.pack ([0x00] :: [Word8])) addr
    commit conn
    disconnect conn

scimHandler sock addr pkt [0x00, 0x03] = do
    conn <- connectPostgreSQL "dbname=scim_auth"
    let pktL = L.unpack pkt
    putStrLn $ show pktL
    let id   = take 8 (drop 2 pktL)
    putStrLn $ show (fromOctets id) 
    r <- quickQuery' conn
        "SELECT online FROM account WHERE id=?" [toSql (fromOctets id)]
    let res = map convRow r
    let id  = read (res!!0) :: Integer
    if (res /= [])
        then do r1 <- quickQuery' conn "SELECT address,port FROM portal ORDER BY address,port" []
                let res1 = map convRow r1
                mapM_ (\a -> sendAllTo sock (C.pack a) addr) res1
        else sendAllTo sock (B.pack [0x00]) addr
    disconnect conn

scimHandler sock addr pkt _ =
    sendAllTo sock "invalid packet type" addr

fromOctets :: [Word8] -> Word32 -- convert Word8 to Word32
fromOctets = foldr accum 0
  where
    accum :: Word8 -> Word32 -> Word32
    accum a o = (o `shiftL` 8) .|. (fromIntegral a)

convRow [id] =
    desc id
convRow [id, passwd] =
    (desc id) ++ " " ++ (desc passwd)

desc :: SqlValue -> String
desc sqlDesc = case fromSql sqlDesc of
    Just x -> x
    Nothing -> "NULL"