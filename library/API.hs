module API
    ( api
    ) where

import qualified Network.Wai as Wai
import qualified Network.HTTP.Types as Types

api :: Wai.Application
api _ respond = do
    respond $ Wai.responseLBS
        Types.status200
        [("Content-Type", "text/plain")]
        "Hello, Web!"

