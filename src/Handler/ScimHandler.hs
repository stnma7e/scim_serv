{-# LANGUAGE OverloadedStrings #-}

module Handler.ScimHandler (
	scimHandler
) where

import Handler (HandlerFunc)
import ByteStringTools

import Network.Socket.ByteString (sendAllTo, recvFrom)
import qualified Data.ByteString.Lazy as L (ByteString, take, unpack)

scimHandler :: HandlerFunc
scimHandler sock addr pkt [0x00, 0x00] =
	do let m_pkt = L.unpack (L.take 4 pkt)
	   sendAllTo sock m_msg addr
	where m_msg = "connect"

scimHandler sock addr pkt _ =
		sendAllTo sock m_msg addr
	where m_msg = "invalid packet header"
