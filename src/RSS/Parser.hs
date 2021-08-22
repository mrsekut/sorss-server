{-# LANGUAGE DeriveGeneric #-}

module RSS.Parser (RSS, parser) where

import qualified Data.Aeson               as Aeson
import           Data.Tree.NTree.TypeDefs (NTree (NTree))
import           GHC.Generics             (Generic)
import           Text.XML.HXT.Core        ((>>>))
import qualified Text.XML.HXT.Core        as HXT



-- Types

data RSS = RSS
  { sVersion  :: Float
  , sChannels :: Channels
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON RSS
instance Aeson.ToJSON RSS


data Channels = Channels
  { title       :: String
  , link        :: String
  , description :: String
  , language    :: String
  , copyright   :: String
  , pubDate     :: String
  , items       :: [Item]
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON Channels
instance Aeson.ToJSON Channels


data Item = Item
  { itemTitle       :: String
  , itemLink        :: String
  , itemDescription :: String
  , itemPubDate     :: String
  } deriving (Show, Eq, Generic)

instance Aeson.FromJSON Item
instance Aeson.ToJSON Item



-- Parser

parser :: String -> IO RSS
parser xml = do
  rootElems <- HXT.runX $
    HXT.readString
      [ HXT.withValidate HXT.no
      , HXT.withTrace 1
      , HXT.withRemoveWS HXT.yes
      , HXT.withPreserveComment HXT.no
      ] xml
    >>> HXT.xunpickleVal xpRss
    >>> proseccRSS
  pure $ head rootElems


proseccRSS :: HXT.IOSArrow RSS RSS
proseccRSS = HXT.arrIO pure




-- XML Pickler

instance HXT.XmlPickler RSS where
  xpickle = xpRss


xpRss :: HXT.PU RSS
xpRss
  = HXT.xpElem "rss"
  $ HXT.xpWrap (uncurry RSS, \s -> (sVersion s, sChannels s))
  $ HXT.xpPair (HXT.xpAttr "version" HXT.xpPrim) HXT.xpickle


instance HXT.XmlPickler Channels where
  xpickle = xpChannels


xpChannels :: HXT.PU Channels
xpChannels
  = HXT.xpElem "channel"
  $ HXT.xpWrap ( \(t,l,d,la,c,p,i) -> Channels t l d la c p i
               , \t -> ( title t
                       , link t
                       , description t
                       , language t
                       , copyright t
                       , pubDate t
                       , items t))
  $ HXT.xp7Tuple (HXT.xpElem "title" HXT.xpText)
                 (HXT.xpElem "link" HXT.xpText)
                 (HXT.xpElem "description" HXT.xpText)
                 (HXT.xpElem "language" HXT.xpText)
                 (HXT.xpElem "copyright" HXT.xpText)
                 (HXT.xpElem "pubDate" HXT.xpText)
                 HXT.xpickle


instance HXT.XmlPickler Item where
  xpickle = xpItem


xpItem :: HXT.PU Item
xpItem
  = HXT.xpElem "item"
  $  HXT.xpWrap ( HXT.uncurry4 Item
               , \t -> ( itemTitle t
                       , itemLink t
                       , itemDescription t
                       , itemPubDate t))
  $ HXT.xp4Tuple (HXT.xpElem "title" HXT.xpText)
                 (HXT.xpElem "link" HXT.xpText)
                 (HXT.xpElem "description" HXT.xpText)
                 (HXT.xpElem "pubDate" HXT.xpText)
