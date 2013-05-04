module Main where

import Serve
import Handler.Scim.Auth

main = serve "13572" scimAuthHandler