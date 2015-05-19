
module Preface.Xml (
    XmlElement (..)
  , parseXml
  , findNodesWith
  , findNodes
  , children
) where

import Data.List (isPrefixOf)
import Data.Char (isSpace, isAlphaNum)
import Control.Monad (liftM2)

import Debug.Trace

data XmlElement = XmlNode XmlTag [XmlAttr] [XmlElement] 
                | XmlComment String 
                | XmlText String 
                | XmlParseError String 
type XmlTag = String 
data XmlAttr = XmlAttr String String
type ParserState = String
type Parser = ParserState -> (XmlElement, ParserState)

instance Show XmlAttr where
  show (XmlAttr a b) = " "++show a ++ "=" ++ show b

instance Show XmlElement where
  show (XmlNode t a e) = "<"++t++concatMap show a++">\r"++concatMap (\x->'\r' : show x) e++"</"++t++">"
  show (XmlComment x) = "<!--" ++ x ++ "-->"
  show (XmlText x) = x
  show (XmlParseError x) = let (a,b) = splitAt 100 x
                            in "*** XmlParseError: " ++ a ++ if null b then "" else " ... "

trim = dropWhile isSpace

split :: String -> String -> (String, String)
split x y = let (a,b) = split_ ("",y) in (reverse a, b)
  where split_ (q, r) = if x `isPrefixOf` r || null r
                          then (q, (drop (length x) r))
                          else split_ (head r : q, tail r)

parseXml :: ParserState -> XmlElement
parseXml str = let (xmlh, rest) = popXh str
                   (schm, r2) = schema rest
                   (elm, r3) = xmlParser r2
                in if null (trim r3) then elm
                   else case elm of
                          XmlParseError _ -> elm
                          _ -> XmlParseError ("excess unparsed input: " ++ r3)

xmlParser :: Parser 
xmlParser = xmlParser_ . trim
  where xmlParser_ r =
          let (a,b) = comment r
              (j,k) = parseNode b
              q = case a of
                Nothing -> j
                Just c -> prepn c j
           in (q, k)

prepn :: XmlElement -> XmlElement -> XmlElement
prepn e (XmlNode t a c) = XmlNode t a (e:c)
prepn e f@(XmlParseError _) = f
prepn e _ = XmlParseError "unable to prepend element"

comment :: ParserState -> (Maybe XmlElement, ParserState) 
comment str = if "<!--" `isPrefixOf` str 
              then let (a, b) = split "-->" (drop 4 str)
                    in (Just (XmlComment a), b)
              else (Nothing, str)

parseNode :: Parser
parseNode str = let r = trim str in openTag r
     
schema :: String -> (String, String)
schema str = let r = trim str
              in if "<!" `isPrefixOf` r
                    then split ">" (drop 2 r)
                    else ("", r)

popXh :: String -> (String, String)
popXh str = let r = trim str
             in if "<?xml" `isPrefixOf` r 
                   then split "?>" (drop 5 r)
                   else ("", r)
                   
openTag :: Parser
openTag str = let r = trim str
               in if '<' == head r then ot (tail r)
                  else (XmlParseError "'<' not found when expected", r)
  where ot r = let (a,b) = span (liftM2 (||) isAlphaNum (==':')) r
                in if null a then (XmlParseError "invalid tag", b)
                   else keyValue a b 

-- closeTag :: ParserState -> (XmlTag, ParserState)
-- closeTag str = string "</" >> spaces >> string str >> spaces >> char '>' >> spaces >> return ()

closeTag e@(XmlNode t a es) s =
        if (t ++ ">") `isPrefixOf` s then (e, drop (1+length t) s)
        else (XmlParseError ("closing tag does not match open tag: "++t), s)

-- | The currently parsing outer node is passed in, as well as the 
-- string being parsed.  A node is popped off the string and added
-- to the element-list of the outer node.  This recurses until the
-- closing tag is found
parseBody :: XmlElement -> ParserState -> (XmlElement, ParserState)
parseBody e@(XmlNode t a es) s = 
        let s1 = trim s
         in if null s1 then (XmlParseError ( "premature termination (in "++t++")" ) , s1)
            else let (c1, s2) = comment s1
                  in case c1 of 
                       Nothing -> ncpb s1
                       Just c2 -> parseBody (XmlNode t a (c2:es)) s2
  where ncpb s1 =                       
          if '<' == head s1 
          then if (not . null . tail) s1 && '/' == (head . tail) s1 
               then closeTag e (drop 2 s1)
               else let (q,r) = openTag s1
                     in case q of 
                          XmlParseError _ -> (q, r)
                          _ -> parseBody (XmlNode t a (q:es)) r
          else let (j,k) = span (/= '<') s1
                in parseBody (XmlNode t a (XmlText j:es)) k 

-- | Parse key/value attributes in an open tag
-- If successful, this leaves us at the end of the open tag
-- and parsing will continue with the body of the node.
keyValue :: XmlTag -> ParserState -> (XmlElement, ParserState)
keyValue tag = kv_ [] . trim
  where kv_ a r  
            | "/>" `isPrefixOf` r = (XmlNode tag (reverse a) [], drop 2 r)
            | ">" `isPrefixOf` r = parseBody (XmlNode tag (reverse a) []) (tail r)
            | otherwise = kvs a r
        kvs a r = 
          let (k, r2) = span (liftM2 (||) isAlphaNum (`elem` "-:") ) r
              r3 = trim r2
              r4 = trim ( tail r3 ) -- assuming r3 starts with '='
              (v, r5) = qsn r4
           in if null k then (XmlParseError "invalid attribute", r2)
              else if '=' /= head r3 then (XmlParseError "expecting attribute '='", r3) -- kv_ (XmlAttr k "" : a) r3
              else kv_ (XmlAttr k v : a) (trim r5)
        qsn x = if head x == '"' || head x == '\''
                   then quotedString x
                   else span (liftM2 (||) isAlphaNum (=='-') ) x

quotedString :: String -> (String, String)
quotedString str = if '"' == head str || '\'' == head str
                   then qs (head str) "" (tail str)
                   -- this should be an error
                   else ("", str)
  where qs c i s
           -- this should be an error
           | null s = ("", s) -- unterminated string
           | head s == c = ( (reverse i), tail s)
           | head s == '\\' && (not . null . tail) s = 
                   qs c (head ( tail s) : i) (tail (tail s))
           | True = qs c (head s : i) (tail s)

nodeMatches :: (XmlTag -> Bool) -> XmlElement -> Bool
nodeMatches f (XmlNode ztag _ _) = f ztag
nodeMatches _ _ = False

findNodesWith :: (XmlTag -> Bool) -> XmlElement -> [XmlElement]
findNodesWith f z = if nodeMatches f z then [z] else case z of
    XmlNode _ _ nodes -> concatMap (findNodesWith f ) nodes
    _ -> []

findNodes :: String -> XmlElement -> [XmlElement]
findNodes x = findNodesWith (==x)

children :: XmlElement -> [XmlElement]
children (XmlNode _ _ nodes) = nodes
children _ = []


