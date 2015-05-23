{-# LANGUAGE TemplateHaskell, FlexibleInstances, UndecidableInstances #-}

module Preface.Xml (
    XmlElement (..)
  , XMLic (..)
  , parseXml
  , findNodesWith
  , findNodes
  , children
  , deriveXmlic
  , defaultOptions
) where

import Data.List (isPrefixOf, foldl', partition)
import Data.Char (isSpace, isAlphaNum)
import Data.Maybe (catMaybes)
import Control.Monad (liftM2)
import Text.Printf
import Data.Time (UTCTime)

import qualified Data.Map as M
import qualified Data.Text as T

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (VarStrictType)

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
  show (XmlNode t a e) = "<"++t++concatMap show a++">\r"++concatMap (\x->'\n' : show x) e++"</"++t++">"
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

firstNodeWith :: (XmlTag -> Bool) -> XmlElement -> Maybe XmlElement
firstNodeWith f z = let a = findNodesWith f z in if null a then Nothing else Just $ head a

firstNode :: String -> XmlElement -> Maybe XmlElement
firstNode x = firstNodeWith (==x)

findNodes :: String -> XmlElement -> [XmlElement]
findNodes x = findNodesWith (==x)

children :: XmlElement -> [XmlElement]
children (XmlNode _ _ nodes) = nodes
children _ = []

-- ----------------------------------------------
-- auto derivation of xml-ability

type XmlError = String

class XMLic a where
        toXML :: a -> XmlElement
        fromXML :: XmlElement -> Either XmlError a

instance XMLic Int where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    otherwise -> Left ( "not an XmlText: " ++ show x) 
                    
instance XMLic Integer where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    otherwise -> Left ("not an XmlText: "++ show x)

instance XMLic Double where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    otherwise -> Left ("not an XmlText"++ show x)

instance XMLic UTCTime where
      toXML = XmlText . show 
      fromXML x = case x of
                    XmlText y -> Right (read y)
                    otherwise -> Left ("not an XmlText: "++ show x)

deriveXmlic :: Options -> Name -> Q [Dec]
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


consToXML :: Options -> [Con] -> Q Exp
consToXML _ [] = error $ "consToXML: Not a single constructor"
consToXML opts [con] = do
        value <- trace "consToXML" $ newName "value"
        lam1E (varP value) $ caseE (varE value) (trace "consToXML lam1E" [encodeArgs opts False con])
consToXML opts cons = do
        value <- newName "value"
        lam1E (varP value) $ caseE (varE value) matches
  where matches
          | allNullaryToStringTag opts && all isNullary cons = trace "consToXML allNullaryToStringTag"
                  [ match (conP conName []) (normalB $ constrNode opts conName) []
                  | con <- cons
                  , let conName = getConName con
                  ]
          | otherwise = trace "consToXML otherwise" [encodeArgs opts True con | con <- cons]

data Options = Options {
          allNullaryToStringTag :: Bool
        , omitNothingFields :: Bool
        , fieldLabelModifier :: String -> String
        , constructorTagModifier :: String -> String
        }

defaultOptions = Options { allNullaryToStringTag = True
                         , omitNothingFields = True
                         , fieldLabelModifier = id
                         , constructorTagModifier = id
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

fieldLabelExp :: Options -> Name -> Q Exp
fieldLabelExp opts = litE . stringL . fieldLabelModifier opts . nameBase

-- ------------------------------------------------------

parseArgs :: Name -> Options -> Con -> Either (String, Name) Name -> Q Exp
parseArgs tName _ (NormalC conName []) (Left (valFieldName, obj)) =
        getValField obj valFieldName $ parseNullaryMatches tName conName

parseArgs tName _ (NormalC conName []) (Right valName) = 
        caseE (varE valName) $ parseNullaryMatches tName conName

parseArgs _ _ (NormalC conName [_]) (Left (valFieldName, obj)) = 
        getValField obj valFieldName $ parseUnaryMatches conName

parseArgs _ _ (NormalC conName [_]) (Right valName) =
        caseE (varE valName) $ parseUnaryMatches conName

-- Records.
parseArgs tName opts (RecC conName ts) (Left (_, obj)) =
    parseRecord opts tName conName ts obj
parseArgs tName opts (RecC conName ts) (Right valName) = do
  obj <- newName "recObj"
  caseE (varE valName)
    [ match (conP 'XmlNode [ {-varP obj-} wildP, listP [], wildP ]) (normalB $ parseRecord opts tName conName ts {- obj -} valName ) []
    , matchFailed tName conName "XmlNode"
    ]

-- Infix constructors. Apart from syntax these are the same as
-- polyadic constructors.
{-
parseArgs tName _ (InfixC _ conName _) (Left (valFieldName, obj)) =
    getValField obj valFieldName $ parseProduct tName conName 2
parseArgs tName _ (InfixC _ conName _) (Right valName) =
    caseE (varE valName) $ parseProduct tName conName 2
-}

-- Existentially quantified constructors. We ignore the quantifiers
-- and proceed with the contained constructor.
parseArgs tName opts (ForallC _ _ con) contents =
    parseArgs tName opts con contents

-- the names are mostly needed for error reporting
parseRecord :: Options -> Name -> Name -> [VarStrictType] -> Name -> ExpQ
parseRecord opts tName conName ts obj =
    foldl' (\a b -> infixApp a [|(<*>)|] b)
           (infixApp (conE conName) [|(<$>)|] x)
           xs
    where
      x:xs = [ [| lookupField |]
               `appE` (litE $ stringL $ show tName)
               `appE` (litE $ stringL $ constructorTagModifier opts $ nameBase conName)
               `appE` (varE obj)
               `appE` ( fieldLabelExp opts field )
             | (field, _, _) <- ts
             ]

lookupField tName rec obj key = 
        case firstNode key obj of
          Nothing -> unknownFieldFail tName rec key
          Just (XmlNode t _ [v])  -> fromXML v 
          _ -> Left $ printf "did not match XmlNode with single child when parsing key %s of %s" key rec

unknownFieldFail :: String -> String -> String -> Either String fail
unknownFieldFail tName rec key =
    Left $ printf "When parsing the record %s of type %s the key %s was not present."
                  rec tName key

-- -----------------------------------------------------

parseNullaryMatches :: Name -> Name -> [Q Match]
parseNullaryMatches = undefined

parseUnaryMatches :: Name -> [Q Match]
parseUnaryMatches conName  = undefined

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
encodeArgs :: Options -> Bool -> Con -> Q Match
encodeArgs opts multiCons (NormalC conName []) = 
        trace "ea 1" $ match (conP conName []) 
              (normalB (encodeSum opts multiCons conName [e|toXML ([] :: [()]) |] ))
              []


-- Polyadic constructors with special case for unary constructors.
encodeArgs opts multiCons (NormalC conName ts) = do
        let len = length ts
        args <- trace "ea 2" $ mapM newName ["arg"++show n | n <- [1..len]]
        xml <- case [ [|toXML|] `appE` varE arg | arg <- args] of
                 [e] -> return e
                 es -> do
                         do
                            p<- mapM runQ es
                            trace (pprint p) [|()|]
                         return $ [|XmlArray|] `appE` listE es

        match (conP conName $ map varP args)
              (normalB $ encodeSum opts multiCons conName xml)
              []

-- Record constructor
-- wants to generate 
-- XmlNode conName [] . (map (\x -> XmlNode fldnam [] [toXML x]))
encodeArgs opts multiCons (RecC conName ts) = do
    args <- mapM newName ["arg" ++ show n | (_, n) <- zip ts [1 :: Integer ..]]
    let exp = [| \z -> XmlNode z [] |] `appE` constrStr opts conName `appE` pairs 

        pairs | omitNothingFields opts = infixApp maybeFields [|(++)|] restFields
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

    match (conP conName $ map varP args)
          ( normalB
          $ if multiCons 
            then [| (\x -> XmlNode x []) |] `appE` constrStr opts conName `appE` listE [exp]
            else exp
          ) []

-- Infix constructor
encodeArgs opts multiCons (InfixC _ conName _ ) = do
        a1 <- newName "argL"
        ar <- newName "argR"
        trace "ea 4" $ match (infixP (varP a1) conName (varP ar))
              ( normalB
              $ encodeSum opts multiCons conName
              $ [|toXML|] `appE` listE [ [|toXML|] `appE` varE a
                                       | a <- [a1, ar]
                                       ]
              )
              []

encodeArgs opts multiCons (ForallC _ _ con) = encodeArgs opts multiCons con

-- ----------------------------------------------------------
constrNode :: Options -> Name -> Q Exp
constrNode opts = appE [|(\x -> XmlNode x [] []) |] . stringE . constructorTagModifier opts . nameBase

constrStr :: Options -> Name -> Q Exp
constrStr opts = stringE . constructorTagModifier opts . nameBase
-- ----------------------------------------------------------

encodeSum :: Options -> Bool -> Name -> Q Exp -> Q Exp
encodeSum opts multiCons conName exp
   | multiCons = [|XmlText|] `appE` exp
               -- [|XmlNode|] `appE` (listE [constrStr opts conName, exp])
               


-- -------------------------------------------------------------
noStringFail :: String -> String -> Either String fail
noStringFail t o = Left $ printf "When parsing %s expected XmlNode but got %s." t o

noMatchFail :: String -> String -> Either XmlError fail
noMatchFail t o =
    Left $ printf "When parsing %s expected an XmlNode but got %s." t o

-- | The name of the outermost 'JSON' constructor.
valueConName :: XmlElement -> String
valueConName (XmlNode _ _ _) = "XmlNode"
valueConName (XmlText  _) = "XmlText"
valueConName (XmlParseError _) = "XmlParseError"
valueConName _ = "UnknownXmlConstructor"
-- =============================================================

consFromXML :: Name -> Options -> [Con] -> Q Exp
consFromXML _ _ [] = error $ "consFromXML: not a single constructor"
consFromXML tName opts [con] = do
        value <- trace "consFromXML" $  newName "value"
        lam1E (varP value) (parseArgs tName opts con (Right value))

consFromXML tName opts cons = do
  value <- newName "value"
  lam1E (varP value) $ caseE (varE value) $
    if allNullaryToStringTag opts && all isNullary cons
    then allNullaryMatches
    else mixedMatches

  where
    allNullaryMatches =
      [ do txt <- newName "txt"
           match (conP 'XmlNode [varP txt, listP [], listP []] )
                 (guardedB $
                  [ liftM2 (,) (normalG $
                                  infixApp (varE txt)
                                           [|(==)|]
                                           (( stringE . constructorTagModifier opts . nameBase) conName)
                               )
                               ([|Right|] `appE` conE conName)
                  | con <- cons
                  , let conName = getConName con
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
                                                     . constructorTagModifier opts
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
matchFailed tName conName expected = do
  other <- newName "other"
  match (varP other)
        ( normalB $ parseTypeMismatch tName conName expected
                      ([|valueConName|] `appE` varE other)
        )
        []
parseTypeMismatch :: Name -> Name -> String -> ExpQ -> ExpQ
parseTypeMismatch tName conName expected actual =
    [| Left |] `appE` (foldl appE
          [| printf "When parsing the constructor %s of type %s expected %s but got %s"|]
          [ litE $ stringL $ show tName
          , litE $ stringL $ nameBase conName
          , litE $ stringL expected
          , actual
          ])

