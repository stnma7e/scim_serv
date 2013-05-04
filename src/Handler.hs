module Handler (
	HandlerFunc
) where

import Network.Socket
import qualified Data.ByteString.Lazy as L (ByteString, pack, unpack, take, toChunks, fromChunks)
import Data.Word

type HandlerFunc = Socket -> SockAddr -> L.ByteString -> IO ()
