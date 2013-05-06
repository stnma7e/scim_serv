module Handler.Scim.Portal (
	scimPortalHandler
) where

import Handler
import Handler.Repeat

scimPortalHandler :: HandlerFunc
scimPortalHandler = repeatHandler
