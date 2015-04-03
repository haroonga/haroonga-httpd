module Web.Groonga.Server where

import Web.Scotty
import Network.HTTP.Types
import Data.Monoid (mconcat)
import Bindings.Groonga.Raw (C'_grn_ctx)
import qualified Bindings.Groonga.CommandAPI as Groonga
import qualified Data.Text.Lazy as L
import Control.Monad.IO.Class (liftIO)
import Foreign.Ptr (Ptr)
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import System.Directory
#if !MIN_VERSION_time(1,5,0)
import Data.Time
import System.Locale
#else
import Data.Time hiding (TimeLocale)
import System.Locale hiding (defaultTimeLocale)
#endif
import Control.Applicative ((<$>))

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
      start_at <- liftIO $ get_current_time_as_double
      ver <- get_groonga_version
      done_at <- liftIO $ get_current_time_as_double
      let buf = concat ["{\"Groonga Version\": \"", (L.unpack ver), "\"}"]
      let response = format_response 0 start_at done_at buf
      text $ L.pack response
      set_json_header

    get "/d/" $ do
      start_at <- liftIO $ get_current_time_as_double
      done_at <- liftIO $ get_current_time_as_double
      let errbuf = "empty param."
      let response = format_err_response (-1) start_at done_at errbuf
      text $ L.pack response
      status internalServerError500
      set_json_header

    get (regex "^/d/(.*)$") $ do
      command <- param "1"
      response <- send_groonga_command $ L.unpack command
      case response of
        Left res -> do
          text $ L.pack res
          status internalServerError500
          set_json_header
        Right res -> do
          text $ L.pack res
          set_json_header

    notFound $ do
      status notFound404
      set_json_header

    where
      get_groonga_version :: ActionM L.Text
      get_groonga_version = liftIO $ do
        version <- Groonga.grn_get_version
        return (L.pack version)

      send_groonga_command :: String -> ActionM (Either String String)
      send_groonga_command command = liftIO $ do
        ctx <- Groonga.grn_ctx_init
        _ <- Groonga.grn_database_open ctx dbpath
        start_at <- get_current_time_as_double
        response <- Groonga.grn_execute_command ctx command
        done_at <- get_current_time_as_double
        errbuf <- Groonga.grn_get_errbuf ctx
        _ <- Groonga.grn_ctx_fin ctx
        if length errbuf > 0
          then return $ Left $ format_err_response (-1) start_at done_at errbuf
          else return $ Right $ format_response 0 start_at done_at response

      set_json_header :: ActionM ()
      set_json_header = setHeader "Content-Type" "application/json; charset=utf-8"

      treat_as_string :: String -> String
      treat_as_string str = concat ["\"", str, "\""]

      get_current_time_as_double :: IO Double
      get_current_time_as_double = do
        epoch_double <- (read <$> formatTime defaultTimeLocale "%s.%q"
                              <$> getCurrentTime) :: IO Double
        return epoch_double

      format_response :: (Show a, Num a) => Int -> a -> a -> String -> String
      format_response status start_at done_at response =
        concat ["[", "[", (show status), ",",
                          (show start_at), ",",
                          (show $ (done_at - start_at)), "],", response, "]"]

      format_err_response :: (Show a, Num a) => Int -> a -> a -> String -> String
      format_err_response status start_at done_at errbuf =
        concat ["[", "[", (show status), ",",
                          (show start_at), ",",
                          (show $ (done_at - start_at)), ",",
                          (treat_as_string errbuf), ",[]", "]]"]
