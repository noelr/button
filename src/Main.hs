{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Network.Wai.Middleware.Static
import System.FilePath ((</>))
import Control.Monad.IO.Class
import Data.Text.IO as TIO
import System.Environment
import Data.Maybe
import Data.List
import Data.Text as T
import Data.Text.Lazy as TL
import Data.Text.Lazy.Encoding as TLE


main :: IO ()
main = do
  ep <- lookupEnv "PORT"
  let p = fromMaybe 4000 (read <$> ep)
  scotty p $ do
    middleware logStdoutDev
    middleware $ staticPolicy (noDots >-> addBase "static")

    let db = "log" </> "log"

    get "/" $
      file "static/index.html"

    get "/log" $ do
      setHeader "cache-control" "no-cache"
      file db

    post "/log" $ do
      l <- body
      liftIO $ TIO.appendFile db (mconcat [(TL.toStrict . TLE.decodeUtf8) l, "\n"])
      text "OK"
