-- scim server

module Serve (
    serve
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
import Handler.Scim.Auth
import Handler.Repeat
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
            (pkt, addr) <- recvFrom sock 4096
            forkIO $ handlerFunc sock addr (strictToLazyBS pkt)
            handleConnections sock