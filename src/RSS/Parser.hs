module RSS.Parser where

import Text.XML.HXT.Core

import System.IO
import System.Environment
import System.Console.GetOpt
import System.Exit

main :: IO ()
main = do
  [rc]  <- runX $ application [withValidate no] "hoge.xml" "hoge-o.xml"
  if rc >= c_err
    then exitWith (ExitFailure (negate 1))
    else exitSuccess


application :: SysConfigList -> String -> String -> IOSArrow b Int
application cfg src dst
  = configSysVars (withTrace 1 : cfg)
    >>> traceMsg 1 "start reading document"
    >>> readDocument [] src
    >>> traceMsg 1 "document read, start processing"
    >>> processChildren (selectAllTextAndRealAltValues `when` isElem)
    >>> traceMsg 1 "document processed"
    >>> writeDocument [] dst
    >>> getErrStatus


selectAllTextAndRealAltValues :: ArrowXml a => a XmlTree XmlTree
selectAllTextAndRealAltValues = deep $ isText <+> imageAlt
  where
    imageAlt :: ArrowXml a => a XmlTree XmlTree
    imageAlt =
      isElem
      >>> hasName "image"
      >>> getAttrValue "href"
      >>> isA significant
      >>> arr addBrackets
      >>> mkText

    significant :: String -> Bool
    significant = not . all (`elem` " \n\r\t")

    addBrackets :: String -> String
    addBrackets s = " [[ " ++ s ++ " ]] "
