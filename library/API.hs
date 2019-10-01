{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module API
    ( serve
    ) where

import qualified Network.Wai as Wai
import qualified Servant
import Servant ((:>))

type API = "hello" :> Servant.Get '[Servant.JSON] String

helloWorld :: String
helloWorld = "Hello World"

server :: Servant.Server API
server = pure helloWorld

apiProxy :: Servant.Proxy API
apiProxy = Servant.Proxy

serve :: Wai.Application
serve = Servant.serve apiProxy server

