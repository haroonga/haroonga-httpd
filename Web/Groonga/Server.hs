module Web.Groonga.Server where

import Web.Scotty
import Data.Monoid (mconcat)
import Bindings.Groonga.Raw (C'_grn_ctx)
import qualified Bindings.Groonga.CommandAPI as Groonga
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory

type GrnCtx = Ptr C'_grn_ctx

db :: String -> IO ()
db dbpath = do
  ctx <- Groonga.grn_ctx_init
  create_db_if_needed ctx dbpath

create_db_if_needed :: GrnCtx -> String -> IO ()
create_db_if_needed ctx dbpath = do
  result <- doesFileExist dbpath
  if result
    then putStrLn $ "Skip create database. Already exists " ++ dbpath ++ "."
    else do
      _ <- Groonga.grn_database_create ctx dbpath
      return ()
  _ <- Groonga.grn_ctx_fin ctx
  return ()

app :: String -> ScottyM ()
app dbpath = do
    middleware logStdoutDev

    get "/version" $ do
      ver <- get_groonga_version
      text $ mconcat ["{\"Groonga Version\": \"", ver, "\"}"]
      set_json_header

    get "/d/:command" $ do
      command <- param "command"
      response <- send_groonga_command (L.unpack command)
      text (L.pack response) -- just to send response. Don't decode with Aeson!
      set_json_header

    where
      get_groonga_version :: ActionM L.Text
      get_groonga_version = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      send_groonga_command :: String -> ActionM String
      send_groonga_command command = liftIO $ do
        ctx <- Groonga.grn_ctx_init
        _ <- Groonga.grn_database_open ctx dbpath
        response <- Groonga.grn_execute_command ctx command
        _ <- Groonga.grn_ctx_fin ctx
        return response

      set_json_header :: ActionM ()
      set_json_header = setHeader "Content-Type" "application/json; charset=utf-8"
