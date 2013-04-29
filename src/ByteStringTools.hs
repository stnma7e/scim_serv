module ByteStringTools (
	lazyToStrictBS
	, strictToLazyBS
) where

import qualified Data.ByteString.Lazy as L (ByteString, toChunks, fromChunks)
import qualified Data.ByteString as B (ByteString, concat)

lazyToStrictBS :: L.ByteString -> B.ByteString
strictToLazyBS :: B.ByteString -> L.ByteString
lazyToStrictBS x = B.concat $ L.toChunks x
strictToLazyBS x = L.fromChunks [x]
