module Main (main) where

import qualified API

import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  putStrLn "Running api on localhost:8080"
  Warp.run 8080 API.serve
