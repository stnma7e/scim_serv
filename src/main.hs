-- udp server

import Data.Bits
import Network.Socket hiding (recvFrom)
import Network.BSD
import Data.List
import Control.Concurrent
import Network.Socket.ByteString (sendAllTo, recvFrom)
import Data.ByteString (ByteString, pack)

type HandlerFunc = Socket -> SockAddr -> ByteString -> IO ()

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
            (msg, addr) <- recvFrom sock 1024
            forkIO $ handlerFunc sock addr msg
            handleConnections sock

repeatHandler :: HandlerFunc
repeatHandler sock addr msg = do 
    putStrLn ("From " ++ show addr ++ ": " ++ show msg)
    sendAllTo sock msg addr

main = do
    serve "5002" repeatHandler