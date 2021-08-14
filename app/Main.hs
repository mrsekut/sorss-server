{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           Prelude                  ()
import           Prelude.Compat

import           Data.Aeson
import qualified Data.Aeson.Parser
import           Data.ByteString          (ByteString)
import           Data.List
import           GHC.Generics
import           Network.HTTP.Media       ((//), (/:))
import           Network.Wai
import           Network.Wai.Handler.Warp
-- import           RSS.Parser               (ppp)
import           Control.Monad.IO.Class
-- import           Control.Monad.Trans.Either
import qualified Data.Aeson               as Aeson
import           RSS.Parser               (ppp)
import           Servant


type API = "rss"  :> Get '[JSON] [ Value ]




data RSS = RSS
  { copyright :: String
  , link      :: String
  , pubDate   :: Int
  , language  :: String
  , item      :: [Item]
  } deriving Generic

instance FromJSON RSS
instance ToJSON RSS


data Item = Item {
    itemLink    :: String
  , itemPubDate :: String
  , title       :: String
  , description :: String
} deriving Generic

instance FromJSON Item
instance ToJSON Item


server :: Server API
server = rss
  where
    -- rss :: EitherT ServantErr IO ()
    -- rss = undefined
    rss = liftIO ppp


api :: Proxy API
api = Proxy


app :: Application
app = serve api server


main :: IO ()
main = run 8081 app
