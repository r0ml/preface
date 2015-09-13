{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

module Preface.Xml (
    XmlElement (..)
  , XMLic (..)
  , XmlOptions(..)
  , XmlError(..)
  , parseXml
  , attrValue
  , findNodesWith
  , findNodes
  , firstNodeWith
  , firstNode
  , getText
  , children
  , deriveXmlic
  , defaultXmlOptions

  , consFromXML
) where

import qualified Data.Map as M (lookup)
import qualified Data.Text as T (pack)
import Preface.Imports hiding(split)
import Data.Char (isSpace)

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
  show (XmlNode t a e) = "<"++t++concatMap show a++">\n"++concatMap (\x->'\n' : show x) e++"</"++t++">"
  show (XmlComment x) = "<!--" ++ x ++ "-->"
  show (XmlText x) = x
  show (XmlParseError x) = let (a,b) = splitAt 100 x
                            in "*** XmlParseError: " ++ a ++ if null b then "" else " ... "

trim :: String -> String
trim = dropWhile isSpace

split :: String -> String -> (String, String)
split x y = let (a,b) = split_ ("",y) in (reverse a, b)
  where split_ (q, r) = if x `isPrefixOf` r || null r
                          then (q, (drop (length x) r))
                          else split_ (head r : q, tail r)

parseXml :: ParserState -> XmlElement
parseXml str = let (_xmlh, rest) = popXh str
                   (_schm, r2) = schema rest
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
prepn _e f@(XmlParseError _) = f
prepn _e _ = XmlParseError "unable to prepend element"

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

closeTag :: XmlElement -> ParserState -> (XmlElement, ParserState)
closeTag (XmlNode t _a _es) s =
        if (t ++ ">") `isPrefixOf` s then ( XmlNode t _a (reverse _es), drop (1+length t) s)
        else (XmlParseError ("closing tag does not match open tag: "++t), s)
closeTag _ _ = trace "closing tag cannot be called with non-XmlNode element" undefined


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
parseBody _ _ = trace "parseBody cannot be called with non-XmlNode element" undefined

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
nodeMatches f (XmlNode ztag _ _) = let res = f ztag in traceShow ("nodeMatches", ztag, res) res
nodeMatches _ _a = traceShow ("nodeMatchesNone", _a) False

attrValue :: String -> XmlElement -> Maybe String
attrValue x (XmlNode _ a _) = let z = filter (\(XmlAttr k _v) -> x == k) a in 
                               if null z then Nothing else Just ((\(XmlAttr _ v)->v) (head z))
attrValue _n _ = trace "non-XmlNode XmlElements dont have attributes" undefined

findNodesWith :: (XmlTag -> Bool) -> XmlElement -> [XmlElement]
findNodesWith f z = if nodeMatches f z then [z] else case z of
    XmlNode _ _ nodes -> concatMap (findNodesWith f ) nodes
    _ -> []

firstNodeWith :: (XmlTag -> Bool) -> XmlElement -> Maybe XmlElement
firstNodeWith f z = let a = findNodesWith f z in if null a then Nothing else Just $ head a

firstNode :: String -> XmlElement -> Maybe XmlElement
firstNode x z = traceShow ("firstNode", x, z) $ firstNodeWith (==x) z

findNodes :: String -> XmlElement -> [XmlElement]
findNodes x = findNodesWith (==x)

children :: XmlElement -> [XmlElement]
children (XmlNode _ _ nodes) = nodes
children _ = []

getText :: XmlElement -> String
getText (XmlNode _ _ [XmlText x]) = x
getText _ = error "not a text node"
-- ----------------------------------------------
-- auto derivation of xml-ability

data XmlError = XmlError String
instance Show XmlError where
        show (XmlError a) = a

xmlError :: String -> Either XmlError a
xmlError a = Left $ XmlError $ trace a a

class XMLic a where
        toXML :: a -> XmlElement
        fromXML :: XmlElement -> Either XmlError a

instance XMLic Int where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    _ -> xmlError ( "not an XmlText (int): " ++ show x) 
                    
instance XMLic Integer where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    _ -> xmlError ("not an XmlText (integer): "++ show x)

instance XMLic Bool where
      toXML x = XmlText (if x then "1" else "0")
      fromXML x = case x of 
                    XmlText y -> let xx = dropWhile isSpace y
                                     fc = if null xx then ' ' else head xx
                                  in Right (fc `elem` "1YyTt")
                    XmlNode _t _a [XmlText c] -> Right (if null c then False else ((head c) `elem` "1YyTt"))
                    _ -> xmlError ("not an XmlText (bool): " ++ show x)


instance XMLic String where
        toXML = XmlText 
        fromXML x = case x of 
                      XmlText y -> Right y
                      XmlNode _t _a [XmlText c] -> Right c
                      _ -> xmlError ("not an XmlText (string): " ++ show x)

instance XMLic Double where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    _ -> xmlError ("not an XmlText (double): "++ show x)

instance XMLic UTCTime where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    _ -> xmlError ("not an XmlText (utcTime): "++ show x)


deriveXmlic :: XmlOptions -> Name -> Q [Dec]
deriveXmlic opts name = 
      withType name $ \tvbs cons -> fmap (:[]) $ fromCons tvbs cons
  where
    fromCons :: [TyVarBndr] -> [Con] -> Q Dec
    fromCons tvbs cons = 
      instanceD (applyCon ''XMLic typeNames)
                ( traceShow (tvbs, cons) ((conT ''XMLic) `appT` instanceType))
                [ funD 'toXML
                        [ clause []
                                 (normalB $ consToXML opts cons)
                                 []
                        ],
                  funD 'fromXML
                        [ clause []
                                 (normalB $ consFromXML name opts cons)
                                 []
                        ]
                 ]
      where typeNames = map tvbName tvbs
            instanceType = foldl' appT (conT name) $ map varT typeNames


consToXML :: XmlOptions -> [Con] -> Q Exp
consToXML _ [] = error $ "consToXML: Not a single constructor"
consToXML opts [con] = do
        value <- trace "consToXML" $ newName "value"
        lam1E (varP value) $ caseE (varE value) (trace "consToXML lam1E" [encodeArgs opts False con])
consToXML opts cons = do
        value <- newName "value"
        lam1E (varP value) $ caseE (varE value) matches
  where matches
          | xmlAllNullaryToStringTag opts && all isNullary cons = trace "consToXML allNullaryToStringTag"
                  [ match (conP cName []) (normalB $ constrNode opts cName) []
                  | con <- cons
                  , let cName = getConName con
                  ]
          | otherwise = trace "consToXML otherwise" [encodeArgs opts True con | con <- cons]

data XmlOptions = XmlOptions {
          xmlAllNullaryToStringTag :: Bool
        , xmlOmitNothingFields :: Bool
        , xmlFieldLabelModifier :: String -> String
        , xmlConstructorTagModifier :: String -> String
        }

defaultXmlOptions :: XmlOptions
defaultXmlOptions = XmlOptions { xmlAllNullaryToStringTag = True
                               , xmlOmitNothingFields = True
                               , xmlFieldLabelModifier = id
                               , xmlConstructorTagModifier = id
                               }
-- -------------------------------------------------------------
-- -------------------------------------------------------------

applyCon :: Name -> [Name] -> Q [Pred]
applyCon con typeNames = return (map apply typeNames) 
  where apply t = AppT (ConT con) (VarT t)

tvbName :: TyVarBndr -> Name
tvbName (PlainTV name) = name
tvbName (KindedTV name _) = name

getConName :: Con -> Name
getConName (NormalC name _) = name
getConName (RecC name _) = name
getConName (InfixC _ name _) = name
getConName (ForallC _ _ con) = getConName con

isNullary :: Con -> Bool
isNullary (NormalC _ []) = True
isNullary _ = False

fieldLabelExp :: XmlOptions -> Name -> Q Exp
fieldLabelExp opts = litE . stringL . xmlFieldLabelModifier opts . nameBase

-- ------------------------------------------------------

parseArgs :: Name -> XmlOptions -> Con -> Either (String, Name) Name -> Q Exp
parseArgs tName _ (NormalC cName []) (Left (valFieldName, obj)) =
        getValField obj valFieldName $ parseNullaryMatches tName cName

parseArgs tName _ (NormalC cName []) (Right valName) = 
        caseE (varE valName) $ parseNullaryMatches tName cName

parseArgs _ _ (NormalC cName _) (Left (valFieldName, obj)) = 
        getValField obj valFieldName $ parseUnaryMatches cName

parseArgs _ _ (NormalC cName _) (Right valName) =
        caseE (varE valName) $ parseUnaryMatches cName

-- Records.
parseArgs tName opts (RecC cName ts) (Left (_, obj)) =
    parseRecord opts tName cName ts obj
parseArgs tName opts (RecC cName ts) (Right valName) = do
  -- obj <- newName "recObj"
  caseE (varE valName)
    [ match (conP 'XmlNode [ {-varP obj-} wildP, wildP, wildP ]) (normalB $ parseRecord opts tName cName ts {- obj -} valName ) []
    , matchFailed tName cName "XmlNode"
    ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.

parseArgs _tName _ (InfixC _ _cName _) (Left (_valFieldName, _obj)) = undefined
--    getValField obj valFieldName $ parseProduct tName conName 2
parseArgs _tName _ (InfixC _ _cName _) (Right _valName) = undefined
--    caseE (varE valName) $ parseProduct tName conName 2

-- Existentially quantified constructors. We ignore the quantifiers
-- and proceed with the contained constructor.
parseArgs tName opts (ForallC _ _ con) contents =
    parseArgs tName opts con contents

-- the names are mostly needed for error reporting
parseRecord :: XmlOptions -> Name -> Name -> [VarStrictType] -> Name -> ExpQ
parseRecord opts tName cName ts obj =
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE cName) [|(<$>)|] x)
           xs
    where
      x:xs = traceShow ("parseRecord",tName,cName,ts,obj) $ [ kind tx
            `appE` fieldLabelExp opts field | (field, _, tx) <- ts
             ]
      kind t = case t of 
                  AppT (ConT ax) (ConT bt) -> traceShow ("appt cont", t) $ if ax == ''Maybe then traceShow "maybe" $ cxs [|lookupMaybeField|] bt else traceShow "not maybe" $ cxs [|lookupField|] t
                  AppT ListT bt -> cxs [|lookupArrayField |] bt
                  bt -> cxs [|lookupField|] bt
      cxs x2 _y = x2
          `appE` (litE $ stringL $ show tName)
          `appE` (litE $ stringL $ xmlConstructorTagModifier opts $ nameBase cName)
          `appE` (varE obj)

lookupArrayField :: XMLic a => String -> String -> XmlElement -> String -> Either XmlError [a]
lookupArrayField _tName _rec obj key =
        let fl = findNodes key obj
            flm = map fromXML fl :: XMLic a => [Either XmlError a]
            (ls, rs) = partitionEithers flm
         in if null ls then Right rs else xmlError $ concatMap (\(XmlError x) -> x++"\n") ls

lookupField :: XMLic a => String -> String -> XmlElement -> String -> Either XmlError a
lookupField tName rec obj key = 
        case firstNode key obj of
          Nothing -> unknownFieldFail tName rec key
          -- so the point here is either:
          -- an array of children -- if I'm looking for an array
          -- a child if I'm looking for a child
          Just v  -> fromXML v 
          -- _z -> traceShow ("lookupField", obj) $ xmlError $ printf "lookupField: did not match XmlNode with single child when parsing key %s of %s (%s)" key rec (show _z)

lookupMaybeField :: XMLic a => String -> String -> XmlElement -> String -> Either XmlError (Maybe a)
lookupMaybeField _tName rec obj key =
        case firstNode key obj of
          Nothing -> Right Nothing
          Just (XmlNode _t _ [v]) -> either Left (Right . Just) (fromXML v)
          _ -> xmlError $ printf "lookupMaybeField: did not match XmlNode with single child when parsing key %s of %s" key rec

unknownFieldFail :: String -> String -> String -> Either XmlError fail
unknownFieldFail tName rec key =
    xmlError $ printf "unknownFieldFail: When parsing the record %s of type %s the key %s was not present."
                  rec tName key

-- -----------------------------------------------------

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches = undefined

parseUnaryMatches :: Name -> [Q Match]
parseUnaryMatches _conName  = undefined

-- ------------------------------------------------------

getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
        val <- newName "val"
        doE [ bindS (varP val) $ [|M.lookup|] `appE` (varE obj)
                  `appE` ([|T.pack|] `appE` (litE $ stringL valFieldName))
            , noBindS $ caseE (varE val) matches
            ]

-- ------------------------------------------------------

withType :: Name -> ([TyVarBndr] -> [Con] -> Q a) -> Q a
withType name f = do
        info <- reify name
        case info of 
          TyConI dec -> 
            case dec of
              DataD _ _ tvbs cons _ -> f tvbs cons
              NewtypeD _ _ tvbs con _ -> f tvbs [con]
              other -> error $ "XMLic withType: Unsupported type: " ++ show other
          _ -> error "XMLic withType: I need the name of a type."

data XmlArray a = XmlArray [a] -- undefined

-- -------------------------------------------------------------

-- | Generates code to generate the XML encoding of a single constructor.
encodeArgs :: XmlOptions -> Bool -> Con -> Q Match
encodeArgs opts multiCons (NormalC cName []) = 
        trace "ea 1" $ match (conP cName []) 
              (normalB (encodeSum opts multiCons cName [e|toXML ([] :: [()]) |] ))
              []


-- Polyadic constructors with special case for unary constructors.
encodeArgs opts multiCons (NormalC cName ts) = do
        let len = length ts
        args <- trace "ea 2" $ mapM newName ["arg"++show n | n <- [1..len]]
        xml <- case [ [|toXML|] `appE` varE arg | arg <- args] of
                 [e] -> return e
                 es -> do
                         _ <- do
                            p<- mapM runQ es
                            trace (pprint p) [|()|]
                         return $ [|XmlArray|] `appE` listE es

        match (conP cName $ map varP args)
              (normalB $ encodeSum opts multiCons cName xml)
              []

-- Record constructor
-- wants to generate 
-- XmlNode conName [] . (map (\x -> XmlNode fldnam [] [toXML x]))
encodeArgs opts multiCons (RecC cName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let exp2 = [| \z -> XmlNode z [] |] `appE` constrStr opts cName `appE` pairs 

        pairs | xmlOmitNothingFields opts = infixApp maybeFields [|(++)|] restFields
              | otherwise = listE $ map toPair argCons

        argCons = zip args ts

        maybeFields = [|catMaybes|] `appE` listE (map maybeToPair maybes)

        restFields = listE $ map toPair rest

        (maybes, rest) = partition isMaybe argCons

        isMaybe (_, (_, _, AppT (ConT t) _)) = t == ''Maybe
        isMaybe _ = False

        maybeToPair (arg, (field, _, _)) =
            [|\x y -> XmlNode x [] [toXML y] |] `appE` toFieldName field `appE` varE arg

        toPair (arg, (field, _, _)) = 
            [|\x y -> XmlNode x [] [toXML y] |] `appE` toFieldName field `appE` varE arg

        toFieldName field = fieldLabelExp opts field

    match (conP cName $ map varP args)
          ( normalB
          $ if multiCons 
            then [| (\x -> XmlNode x []) |] `appE` constrStr opts cName `appE` listE [exp2]
            else exp2
          ) []

-- Infix constructor
encodeArgs opts multiCons (InfixC _ cName _ ) = do
        a1 <- newName "argL"
        ar <- newName "argR"
        trace "ea 4" $ match (infixP (varP a1) cName (varP ar))
              ( normalB
              $ encodeSum opts multiCons cName
              $ [|toXML|] `appE` listE [ [|toXML|] `appE` varE a
                                       | a <- [a1, ar]
                                       ]
              )
              []

encodeArgs opts multiCons (ForallC _ _ con) = encodeArgs opts multiCons con

-- ----------------------------------------------------------
constrNode :: XmlOptions -> Name -> Q Exp
constrNode opts = appE [|(\x -> XmlNode x [] []) |] . stringE . xmlConstructorTagModifier opts . nameBase

constrStr :: XmlOptions -> Name -> Q Exp
constrStr opts = stringE . xmlConstructorTagModifier opts . nameBase
-- ----------------------------------------------------------

encodeSum :: XmlOptions -> Bool -> Name -> Q Exp -> Q Exp
encodeSum _opts multiCons _cName exp2
   | multiCons = [|XmlText|] `appE` exp2
               -- [|XmlNode|] `appE` (listE [constrStr opts conName, exp2])
   | otherwise = undefined            


-- -------------------------------------------------------------
noStringFail :: String -> String -> Either XmlError fail
noStringFail t o = xmlError $ printf "When parsing %s expected XmlNode but got %s." t o

noMatchFail :: String -> String -> Either XmlError fail
noMatchFail t o =
    xmlError $ printf "When parsing %s expected an XmlNode but got %s." t o

-- | The name of the outermost 'JSON' constructor.
valueConName :: XmlElement -> String
valueConName (XmlNode _ _ _) = "XmlNode"
valueConName (XmlText  _) = "XmlText"
valueConName (XmlParseError _) = "XmlParseError"
valueConName _ = "UnknownXmlConstructor"
-- =============================================================

consFromXML :: Name -> XmlOptions -> [Con] -> Q Exp
consFromXML _ _ [] = error $ "consFromXML: not a single constructor"
consFromXML tName opts [con] = do
        value <- trace "consFromXML" $  newName "value"
        lam1E (varP value) (parseArgs tName opts con (Right value))

consFromXML tName opts cons = do
  value <- newName "value"
  lam1E (varP value) $ caseE (varE value) $
    if xmlAllNullaryToStringTag opts && all isNullary cons
    then xmlAllNullaryMatches
    else mixedMatches

  where
    xmlAllNullaryMatches =
      [ do txt <- newName "txt"
           match (conP 'XmlNode [varP txt, listP [], listP []] )
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           (( stringE . xmlConstructorTagModifier opts . nameBase) cName)
                               )
                               ([|Right|] `appE` conE cName)
                  | con <- cons
                  , let cName = getConName con
                  ]
                  ++
                  [ liftM2 (,)
                      (normalG [|otherwise|])
                      ( [|noMatchFail|]
                        `appE` (litE $ stringL $ show tName)
                        `appE` (varE txt)
                      )
                  ]
                 )
                 []
      , do other <- newName "other"
           match (varP other)
                 (normalB $ [|noStringFail|]
                    `appE` (litE $ stringL $ show tName)
                    `appE` ([|valueConName|] `appE` varE other)
                 )
                 []
      ]

    mixedMatches = undefined
 {-       case sumEncoding opts of
          TaggedObject {tagFieldName=tf, contentsFieldName=cf} ->
            parseObject $ parseTaggedObject tf cf
          ObjectWithSingleField ->
            parseObject $ parseObjectWithSingleField
          TwoElemArray ->
            [ do arr <- newName "array"
                 match (conP 'JsonArray [varP arr])
                       (guardedB $
                        [ liftM2 (,) (normalG $ infixApp ([|length|] `appE` varE arr)
                                                         [|(==)|]
                                                         (litE $ integerL 2))
                                     (parse2ElemArray arr)
                        , liftM2 (,) (normalG [|otherwise|])
                                     (([|not2ElemArray|]
                                       `appE` (litE $ stringL $ show tName)
                                       `appE` ([|length|] `appE` varE arr)))
                        ]
                       )
                       []
            , do other <- newName "other"
                 match (varP other)
                       ( normalB
                         $ [|noArrayFail|]
                             `appE` (litE $ stringL $ show tName)
                             `appE` ([|valueConName|] `appE` varE other)
                       )
                       []
            ]
-}

{-
    parseObject f =
        [ do obj <- newName "obj"
             match (conP 'JsonObject [varP obj]) (normalB $ f obj) []
        , do other <- newName "other"
             match (varP other)
                   ( normalB
                     $ [|noObjectFail|]
                         `appE` (litE $ stringL $ show tName)
                         `appE` ([|valueConName|] `appE` varE other)
                   )
                   []
        ]

    parseTaggedObject typFieldName valFieldName obj = do
      conKey <- newName "conKey"
      doE [ bindS (varP conKey) 
-- actual should be  case M.lookup key obj of  { Nothing -> pure Nothing; Just v -> parseJSON v } 
-- better known as   fmap parseJSON (M.lookup key obj) 
                  ( [|M.lookup|] `appE` ([|T.pack|] `appE` stringE typFieldName) `appE` (varE obj) )
          , noBindS $ parseContents conKey (Left (valFieldName, obj)) 'conNotFoundFailTaggedObject
          ]

    parse2ElemArray arr = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      let letIx n ix =
              valD (varP n)
                   (normalB ( infixApp (varE arr)
                                       [|(!!)|] 
                                       (litE (integerL ix))))
                   []
      letE [ letIx conKey 0
           , letIx conVal 1
           ]
           (caseE (varE conKey)
                  [ do txt <- newName "txt"
                       match (conP 'JsonString [varP txt])
                             (normalB $ parseContents txt
                                                      (Right conVal)
                                                      'conNotFoundFail2ElemArray
                             )
                             []
                  , do other <- newName "other"
                       match (varP other)
                             ( normalB
                               $ [|firstElemNoStringFail|]
                                     `appE` (litE $ stringL $ show tName)
                                     `appE` ([|valueConName|] `appE` varE other)
                             )
                             []
                  ]
           )

    parseObjectWithSingleField obj = do
      conKey <- newName "conKey"
      conVal <- newName "conVal"
      caseE ([e|M.toList|] `appE` varE obj)
            [ match (listP [tupP [varP conKey, varP conVal]])
                    (normalB $ parseContents conKey (Right conVal) 'conNotFoundFailObjectSingleField)
                    []
            , do other <- newName "other"
                 match (varP other)
                       (normalB $ [|wrongPairCountFail|]
                                  `appE` (litE $ stringL $ show tName)
                                  `appE` ([|show . length|] `appE` varE other)
                       )
                       []
            ]

    parseContents conKey contents errorFun =
        caseE (varE conKey)
              [ match wildP
                      ( guardedB $
                        [ do g <- normalG $ infixApp (varE conKey)
                                                     [|(==)|]
                                                     ([|T.pack|] `appE`
                                                        conNameExp opts con)
                             e <- parseArgs tName opts con contents
                             return (g, e)
                        | con <- cons
                        ]
                        ++
                        [ liftM2 (,)
                                 (normalG [e|otherwise|])
                                 ( varE errorFun
                                   `appE` (litE $ stringL $ show tName)
                                   `appE` listE (map ( litE
                                                     . stringL
                                                     . xmlConstructorTagModifier opts
                                                     . nameBase
                                                     . getConName
                                                     ) cons
                                                )
                                   `appE` ([|T.unpack|] `appE` varE conKey)
                                 )
                        ]
                      )
                      []
              ]
-}
{-
parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches tName conName =
    [ do arr <- newName "arr"
         match (conP 'JsonArray [varP arr])
               (guardedB $
                [ liftM2 (,) (normalG $ [|null|] `appE` varE arr)
                             ([|pure|] `appE` conE conName)
                , liftM2 (,) (normalG [|otherwise|])
                             (parseTypeMismatch tName conName
                                (litE $ stringL "an empty JsonArray")
                                (infixApp (litE $ stringL $ "JsonArray of length ")
                                          [|(++)|]
                                          ([|show . length|] `appE` varE arr)
                                )
                             )
                ]
               )
               []
    , matchFailed tName conName "JsonArray"
    ]
-}
{-
parseUnaryMatches :: Name -> [Q Match]
parseUnaryMatches conName =
    [ do arg <- newName "arg"
         match (varP arg)
               ( normalB $ infixApp (conE conName)
                                    [|(<$>)|]
                                    ([|fromJSON|] `appE` varE arg)
               )
               []
    ]
-}
{-
getValField :: Name -> String -> [MatchQ] -> Q Exp
getValField obj valFieldName matches = do
  val <- newName "val"
  doE [ bindS (varP val) $ [|M.lookup|] `appE` (varE obj) `appE`
                                    ([|T.pack|] `appE`
                                       (litE $ stringL valFieldName))
      , noBindS $ caseE (varE val) matches
      ]
-}


{-
-- | Generates code to parse the JSON encoding of a single constructor.
parseArgs :: Name -- ^ Name of the type to which the constructor belongs.
          -> Options -- ^ Encoding options.
          -> Con -- ^ Constructor for which to generate JSON parsing code.
          -> Either (String, Name) Name -- ^ Left (valFieldName, objName) or
                                        --   Right valName
          -> Q Exp
-- Nullary constructors.
parseArgs tName _ (NormalC conName []) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseNullaryMatches tName conName
parseArgs tName _ (NormalC conName []) (Right valName) =
  caseE (varE valName) $ parseNullaryMatches tName conName

-- Unary constructors.
parseArgs _ _ (NormalC conName [_]) (Left (valFieldName, obj)) =
  getValField obj valFieldName $ parseUnaryMatches conName
parseArgs _ _ (NormalC conName [_]) (Right valName) =
  caseE (varE valName) $ parseUnaryMatches conName

-- Polyadic constructors.
parseArgs tName _ (NormalC conName ts) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName $ genericLength ts
parseArgs tName _ (NormalC conName ts) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName $ genericLength ts
-}

{-
-- | Generates code to parse the JSON encoding of an n-ary
-- constructor.
parseProduct :: Name -- ^ Name of the type to which the constructor belongs.
             -> Name -- ^ 'Con'structor name.
             -> Integer -- ^ 'Con'structor arity.
             -> [Q Match]
parseProduct tName conName numArgs =
    [ do arr <- newName "arr"
         -- List of: "parseJSON (arr `V.unsafeIndex` <IX>)"
         let x:xs = [ [|fromJSON|]
                      `appE`
                      infixApp (varE arr)
                               [|(!!)|]
                               (litE $ integerL ix)
                    | ix <- [0 .. numArgs - 1]
                    ]
         match (conP 'JsonArray [varP arr])
               (normalB $ condE ( infixApp ([|length|] `appE` varE arr)
                                           [|(==)|]
                                           (litE $ integerL numArgs)
                                )
                                ( foldl' (\a b -> infixApp a [|(<*>)|] b)
                                         (infixApp (conE conName) [|(<$>)|] x)
                                         xs
                                )
                                ( parseTypeMismatch tName conName
                                    (litE $ stringL $ "JsonArray of length " ++ show numArgs)
                                    ( infixApp (litE $ stringL $ "JsonArray of length ")
                                               [|(++)|]
                                               ([|show . length|] `appE` varE arr)
                                    )
                                )
               )
               []
    , matchFailed tName conName "JsonArray"
    ]

-}
--------------------------------------------------------------------------------
-- Parsing errors
--------------------------------------------------------------------------------

matchFailed :: Name -> Name -> String -> MatchQ
matchFailed tName cName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName cName expected
                      ([|valueConName|] `appE` varE other)
        )
        []
parseTypeMismatch :: Name -> Name -> String -> ExpQ -> ExpQ
parseTypeMismatch tName cName expected actual =
    [| xmlError |] `appE` (foldl appE
          [| printf "When parsing the constructor %s of type %s expected %s but got %s"|]
          [ litE $ stringL $ show tName
          , litE $ stringL $ nameBase cName
          , litE $ stringL expected
          , actual
          ])

