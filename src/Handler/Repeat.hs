module Handler.Repeat (
	repeatHandler
) where

import Handler
import ByteStringTools
import Network.Socket.ByteString (sendAllTo)

repeatHandler :: HandlerFunc
repeatHandler sock addr pkt = do
    putStrLn ("From " ++ show addr ++ ": " ++ show (lazyToStrictBS pkt))
    sendAllTo sock (lazyToStrictBS pkt) addr