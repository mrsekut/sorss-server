{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE TypeOperators #-}

module Main where


import           Control.Monad.IO.Class   (liftIO)
import qualified Network.Wai.Handler.Warp as Warp
import qualified RSS.Parser               as RSS
import           Servant                  (type (:>))
import qualified Servant                  as S



-- Routing

type API = "rss"  :> S.Get '[S.JSON] RSS.RSS



-- Server

main :: IO ()
main = Warp.run 8081 app


app :: S.Application
app = S.serve api server

api :: S.Proxy API
api = S.Proxy


server :: S.Server API
server = rss
  where
    rss :: S.Handler RSS.RSS
    rss = liftIO RSS.parser
