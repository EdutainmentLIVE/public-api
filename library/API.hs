{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
module API
    ( serve
    ) where

import qualified Control.Monad.IO.Class as IO
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Network.Wai as Wai
import qualified Servant
import Servant ((:>))

type API = "hello" :> Servant.Get '[Servant.JSON] String
        Servant.:<|> "course" :> Servant.ReqBody '[Servant.JSON] CourseName :> Servant.Post '[Servant.JSON] Int
        Servant.:<|> "course" :> Servant.Capture "id" Int :> Servant.Get '[Servant.JSON] [Course]

server :: SQLite.Connection -> Servant.Server API
server conn = pure helloWorld 
              Servant.:<|> insertCourse conn
              Servant.:<|> findById conn

apiProxy :: Servant.Proxy API
apiProxy = Servant.Proxy

serve :: SQLite.Connection -> Wai.Application
serve conn = Servant.serve apiProxy (server conn)

helloWorld :: String
helloWorld = "Hello World"

insertCourse :: SQLite.Connection -> CourseName -> Servant.Handler Int
insertCourse conn unsavedName = do
  IO.liftIO $ SQLite.execute conn "INSERT INTO course (name) VALUES (?)" (SQLite.Only unsavedName)
  rowId <- IO.liftIO $ SQLite.lastInsertRowId conn
  pure $ fromIntegral rowId

findById :: SQLite.Connection -> Int -> Servant.Handler [Course]
findById conn rId =
  IO.liftIO $ SQLite.query conn "SELECT * FROM course WHERE id = ?" (SQLite.Only rId)

data Course = Course
  { courseId :: Int.Int64
  , courseName :: CourseName
  }
  
instance Aeson.ToJSON Course where
  toJSON course = Aeson.object ["id" Aeson..= courseId course, "name" Aeson..= courseName course]

instance SQLite.FromRow Course where
  fromRow = Course <$> SQLite.field <*> SQLite.field

newtype CourseName = CourseName Text.Text
  deriving (Eq, Show, Aeson.ToJSON, SQLite.FromField, SQLite.ToField)

instance Aeson.FromJSON CourseName where
  parseJSON = Aeson.withObject "CourseName" $ \obj -> CourseName <$> obj Aeson..: "name"
