module Main where

import Serve
import Handler.Scim.Portal

main = serve "13572" scimPortalHandler