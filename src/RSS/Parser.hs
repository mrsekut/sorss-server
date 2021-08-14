{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module RSS.Parser where

import           Text.XML.HXT.Core

import qualified Data.Aeson                 as Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.HashMap.Strict        as HashMap
import qualified Data.Map                   as M
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Tree.NTree.TypeDefs
import qualified Data.Vector                as Vector
import           System.Console.GetOpt
import           System.Environment
import           System.Exit
import           System.IO


main :: IO ()
main = do
  rootElems <- runX $
    readDocument [] "hoge.xml"
    >>> getChildren >>> isElem
  BS.putStr
    . BS.intercalate (BS.pack "\n")
    . map (Aeson.encode . wrapRoot . xmlTreeToJSON)
    $ rootElems



data Xml
  = Text
  | Tag String
  | Attr String
  deriving (Eq, Ord, Show)


wrapRoot :: Maybe (Xml, Aeson.Value) -> Aeson.Value
wrapRoot Nothing       = Aeson.Null
wrapRoot (Just (a, b)) = Aeson.object [(packJSValueName a, b)]


xmlTreeToJSON :: XmlTree -> Maybe (Xml, Aeson.Value)
xmlTreeToJSON node@(NTree (XTag qName _) children)
  = Just (Tag (localPart qName), tagMapToJSValue objMap)
  where
    objMap =
        arrayValuesToJSONArrays    -- unify into a single map,
      . concatMapValues            -- grouping into arrays by pair name
      . map (uncurry M.singleton)  -- convert pairs to maps
      . (++) attrVals
      . catMaybes                  -- filter out the empty values (unconvertable nodes)
      $ map xmlTreeToJSON children -- convert xml nodes to Maybe (QName, Aeson.Value) pairs

    attrVals =
      map (Attr *** Aeson.String . T.pack) $ getAttrVals node

xmlTreeToJSON (NTree (XText str) _)
  | T.null text = Nothing
  | otherwise = Just (Text, Aeson.String text)
  where
    text = T.strip $ T.pack str


tagMapToJSValue :: M.Map Xml Aeson.Value -> Aeson.Value
tagMapToJSValue m = case M.toList m of
  [(Text, val)] -> val
  _             -> Aeson.Object . HashMap.fromList . (map . first) packJSValueName $ M.toList m


arrayValuesToJSONArrays :: M.Map k [Aeson.Value] -> M.Map k Aeson.Value
arrayValuesToJSONArrays = M.mapMaybe f
  where
    f []  = Nothing -- will be discarded
    f [x] = Just x  -- don't store as array, just a single value
    f xss = Just $ Aeson.Array . Vector.fromList $ xss -- arrays with more than one element are kept


concatMapValues :: (Ord k) => [M.Map k v] -> M.Map k [v]
concatMapValues = M.unionsWith (++) . (fmap . fmap) (: [])


getAttrVals :: XmlTree -> [(String, String)]
getAttrVals = runLA (getAttrl >>> getName &&& (getChildren >>> getText))


packJSValueName :: Xml -> T.Text
packJSValueName Text     = T.pack "value"
packJSValueName (Attr x) = T.pack x
packJSValueName (Tag x)  = T.pack x
