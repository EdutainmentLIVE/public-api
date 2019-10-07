module Main (main) where

import qualified API

import qualified Database.SQLite.Simple as SQLite
import qualified Network.Wai.Handler.Warp as Warp

main :: IO ()
main = do
  conn <- SQLite.open "test.db"
  SQLite.execute_ conn "CREATE TABLE IF NOT EXISTS course (id INTEGER PRIMARY KEY, name TEXT)"
  putStrLn "Running api on localhost:8080"
  Warp.run 8080 (API.serve conn)
