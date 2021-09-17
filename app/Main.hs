{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Main where


import qualified Codec.Binary.UTF8.String      as Codec
import           Control.Monad.IO.Class        (liftIO)
import qualified Data.ByteString.Lazy.Internal as BI
import qualified Network.HTTP.Simple           as Simple
import qualified Network.Wai.Handler.Warp      as Warp
import           Network.Wai.Middleware.Cors   (simpleCors)
import qualified RSS.Parser                    as RSS
import           Servant                       (type (:>))
import qualified Servant                       as S





-- Routing

type API = "api" :> "rss"  :> S.Get '[S.JSON] RSS.RSS



-- Server

main :: IO ()
main = Warp.run 8081 app


app :: S.Application
app = simpleCors $ S.serve api server


api :: S.Proxy API
api = S.Proxy


server :: S.Server API
server = rss
  where
    rss :: S.Handler RSS.RSS
    rss = do
      res <- Simple.httpLBS "https://rss.itmedia.co.jp/rss/2.0/news_bursts.xml"
      liftIO $ RSS.parser $ byteString2String $ Simple.getResponseBody res


byteString2String :: BI.ByteString -> String
byteString2String =  Codec.decodeString . BI.unpackChars
