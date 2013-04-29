-- udp server

module Main (
    main
) where

import Data.Bits
import Network.Socket hiding (recvFrom)
import Network.BSD
import Data.List
import Control.Concurrent
import Network.Socket.ByteString (sendAllTo, recvFrom)
import qualified Data.ByteString.Lazy as L (ByteString, unpack, take)
import qualified Data.ByteString as B (ByteString)
import Data.Word

import Handler (HandlerFunc)
import Handler.ScimHandler (scimHandler)
import ByteStringTools

serve :: String -> HandlerFunc -> IO ()
serve port handlerFunc = withSocketsDo $ do 
    addrInfoList <- getAddrInfo
                    (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
                     Nothing
                    (Just port)
    let serveraddr = head addrInfoList
    sock <- socket (addrFamily serveraddr) Datagram defaultProtocol
    bindSocket sock (addrAddress serveraddr)
    handleConnections sock

    where handleConnections sock = do
            (pkt, addr) <- recvFrom sock 1024
            forkIO $ handlerFunc sock addr (strictToLazyBS pkt) (stripScimHeader (strictToLazyBS pkt))
            handleConnections sock
          stripScimHeader :: L.ByteString -> [Word8]
          stripScimHeader pkt = L.unpack (L.take 2 pkt)

repeatHandler :: HandlerFunc
repeatHandler sock addr pkt header = do
    putStrLn ("From " ++ show addr ++ ": " ++ show (lazyToStrictBS pkt))
    sendAllTo sock (lazyToStrictBS pkt) addr

main = do
    serve "13572" scimHandler
