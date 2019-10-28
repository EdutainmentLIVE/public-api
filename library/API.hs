{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}  
{-# LANGUAGE OverloadedLists #-}    
{-# LANGUAGE TypeOperators #-}
module API
    ( serve
    ) where

import Control.Lens ((.~), (?~), (&))
import qualified Control.Monad.IO.Class as IO
import qualified Control.Monad.Trans.Reader as Reader
import qualified Data.Aeson as Aeson
import qualified Data.Int as Int
import qualified Data.Text as Text
import qualified Database.SQLite.Simple as SQLite
import qualified Database.SQLite.Simple.ToField as SQLite
import qualified Database.SQLite.Simple.FromField as SQLite
import qualified Network.Wai as Wai
import qualified Servant
import qualified Servant.Swagger as Swagger
import qualified Servant.Swagger.UI as Swagger
import qualified Data.Swagger as Swagger
import Servant ((:>))

type Walle = Reader.ReaderT SQLite.Connection Servant.Handler

type BasicAPI = "hello" :> Servant.Get '[Servant.JSON] String
        Servant.:<|> "course" :> Servant.ReqBody '[Servant.JSON] CourseName :> Servant.Post '[Servant.JSON] Int
        Servant.:<|> "course" :> Servant.Capture "id" Int :> Servant.Get '[Servant.JSON] [Course]

type API = Swagger.SwaggerSchemaUI "swagger-ui" "swagger.json"
              Servant.:<|> BasicAPI

server :: SQLite.Connection -> Servant.Server API
server conn = Swagger.swaggerSchemaUIServer (Swagger.toSwagger basicApiProxy)
                Servant.:<|> (runWalle conn helloWorld) 
                                Servant.:<|> (runWalle conn . insertCourse)
                                Servant.:<|> (runWalle conn . findById)

basicApiProxy :: Servant.Proxy BasicAPI
basicApiProxy = Servant.Proxy

apiProxy :: Servant.Proxy API
apiProxy = Servant.Proxy

runWalle :: SQLite.Connection -> Walle a -> Servant.Handler a
runWalle conn action = Reader.runReaderT action conn

serve :: SQLite.Connection -> Wai.Application
serve = Servant.serve apiProxy . server

helloWorld :: Walle String
helloWorld = do
  pure "Hello World"

insertCourse :: CourseName -> Walle Int
insertCourse unsavedName = do
  conn <- Reader.ask
  IO.liftIO $ SQLite.execute conn "INSERT INTO course (name) VALUES (?)" (SQLite.Only unsavedName)
  rowId <- IO.liftIO $ SQLite.lastInsertRowId conn
  pure $ fromIntegral rowId

findById :: Int -> Walle [Course]
findById rId = do
  conn <- Reader.ask
  IO.liftIO $ SQLite.query conn "SELECT * FROM course WHERE id = ?" (SQLite.Only rId)

data Course = Course
  { courseId :: Int.Int64
  , courseName :: CourseName
  }

instance Swagger.ToSchema Course where
  declareNamedSchema _ = do
    int64Schema <- Swagger.declareSchemaRef (Servant.Proxy :: Servant.Proxy Int.Int64)
    courseNameSchema <- Swagger.declareSchemaRef (Servant.Proxy :: Servant.Proxy CourseName)
    pure $ Swagger.NamedSchema (Just "Course") $ mempty
      & Swagger.type_ ?~ Swagger.SwaggerObject
      & Swagger.properties .~
          [ ("id", int64Schema)
          , ("name", courseNameSchema)
          ]
      & Swagger.required .~ [ "id", "name" ]

instance Aeson.ToJSON Course where
  toJSON course = Aeson.object ["id" Aeson..= courseId course, "name" Aeson..= courseName course]

instance SQLite.FromRow Course where
  fromRow = Course <$> SQLite.field <*> SQLite.field

newtype CourseName = CourseName Text.Text
  deriving (Eq, Show, Aeson.ToJSON, SQLite.FromField, SQLite.ToField, Swagger.ToSchema)

instance Aeson.FromJSON CourseName where
  parseJSON = Aeson.withObject "CourseName" $ \obj -> CourseName <$> obj Aeson..: "name"
