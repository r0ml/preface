
{-
strToExpMap :: String -> [(String, Exp)]
strToExpMap x = case P.parse parseExpMap "map" x of { Left e -> fail (show e); Right t -> t }
-}

mapToExp :: Monad m => [(String, Exp)] -> m Exp
mapToExp lp = return $ ListE ( map (\(x,y) -> (TupE [ LitE (StringL x) , y ])) lp )


{- | hash is a quasiquoter for defining a list of key/value tuples.  The comma or newline separated pairs
     are of the form @key:value@ or @key=value@, where the key is a string, and the value is evaluated is
     a haskell expression in the current context.

     For example:

@
>>> let a = sqrt 10 in [hash|one:3-2, two:5/2, three:a|]
[("one",1.0),("two",2.5),("three",3.1622776601683795)]
@
-}

-- hash :: QuasiQuoter
-- hash = quasiQuoter (mapToExp . strToExpMap )


otkc :: String -> P.Parsec String u Char
otkc x = P.spaces >> P.oneOf x

{-
parseExpMap :: forall u. P.Parsec String u [(String,Exp)]
parseExpMap = ( pair `P.sepBy` otkc ",\n") >>= return 
  where pair = do 
                  k <- key
                  _ <- otkc ":="
                  x <- P.many (P.noneOf "\n,")
                  case parseExp x of { Right e -> return (k, e); Left b -> fail b }
-}

{-
readUrl :: String -> IO ByteString
readUrl x = do
   case parse purl "url" x of
     Right (a,b,c,d) -> TW.get (HTTPS == a) b (show c) [] d >>= return . TW.body
     Left z -> fail (show z)
-}

{-
-- parse a URL into protocol, host, port, path
purl :: P.Parsec String () (ProtocolType, String, Int, String)
purl = do
  a <- many1 (P.noneOf ":") -- protocol
  string "://"
  b <- many1 (P.noneOf "/") `sepBy` char ':' -- domain : port
  let e = head b
      bt = tail b
      ab = case a of
            "http" -> HTTP
            "https" -> HTTPS
            "ftp" -> FTP
            _ -> UNKNOWN
      f = if null bt then case a of
            "http" -> 80
            "https" -> 443
            _ -> 0
          else read (head bt) :: Int
  c <- many anyChar
  return (ab, e, f, c)
-}

-- TODO: Could also add support for a regular expression QuasiQuoter (like the *rex* cabal package

{-
enum :: QuasiQuoter
enum =  QuasiQuoter { quoteDec = qd } where
  pair (k:v:[]) = (k, v)
  pair _ = error "Constructor value pair entries for enums"
  qd s = do
    let lists = filter (not . null) $ map words $ lines s
        pairs = map pair lists
        prs = [|pairs|]
-}

{- | script is a quasiquoter which interpolates the filename, reads the contents at runtime, and then
     evaluates the contents as a haskell script (basically a 'runhaskell').
 -}
-- script = quasiQuoter scriptqq

