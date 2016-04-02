
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}

-- |
-- At the beginning of the @main@ function, @$'initHFlags' \"program
-- description\"@ has to be called to initialize the flags.  All flags
-- will be initialized that are transitively reachable via imports
-- from @main@.  This means, that any Haskell package can easily
-- define command line flags with @HFlags@.  This feature is
-- demonstrated by
-- <http://github.com/errge/hflags/blob/master/examples/ImportExample.hs>
-- and <http://github.com/errge/hflags/tree/master/examples/package>.
--
-- At 'initHFlags' time, the library also tries to gather flags out of
-- environment variables.  @HFLAGS_verbose=True@ is equivalent to
-- specifying @--verbose=True@ on the command line.  This environment
-- feature only works with long options and the user has to specify a
-- value even for @Bool@s.
--

module Preface.HFlags (
  defineCustomFlag,
  FlagType(..),
  initHFlags,
  Flag(..),
  globalFlags,
  dFlag,
  ) where

import Preface.Imports hiding (moduleName)
import Preface.Stringy
import Preface.Misc
import Preface.StrUtils (deComma, stripComments)


{-
runQ [d| data CmdOptions = CmdOptions { opt_name :: String, opt_repeat :: !Int } deriving Show; defaultOptions = CmdOptions { opt_name = "abc", opt_repeat = 7} |]

[DataD [] CmdOptions_1 [] [RecC CmdOptions_2 [(opt_name_3,NotStrict,ConT GHC.Base.String),(opt_repeat_4,IsStrict,ConT GHC.Types.Int)]] [GHC.Show.Show],ValD (VarP defaultOptions_0) (NormalB (RecConE CmdOptions_2 [(opt_name_3,LitE (StringL "abc")),(opt_repeat_4,LitE (IntegerL 7))])) []]



runQ [d| data CmdOptions = CmdOptions { opt_name :: String, opt_repeat :: !Int } deriving Show; defaultOptions = CmdOptions { opt_name = "abc", opt_repeat = 7}; options :: [OptDescr (CmdOptions -> CmdOptions)]; options = [ Option [] ["name"] (ReqArg (\d opts -> opts { opt_name = d }) "NAME") "Who to greet.",Option [] ["repeat"] (ReqArg (\d opts -> opts { opt_repeat = read d }) "REPEATS") "Number of times to repeat the message" ] |]
[DataD [] CmdOptions_7 [] [RecC CmdOptions_8 [(opt_name_9,NotStrict,ConT GHC.Base.String),(opt_repeat_10,IsStrict,ConT GHC.Types.Int)]] [GHC.Show.Show],ValD (VarP defaultOptions_6) (NormalB (RecConE CmdOptions_8 [(opt_name_9,LitE (StringL "abc")),(opt_repeat_10,LitE (IntegerL 7))])) [],SigD options_5 (AppT ListT (AppT (ConT System.Console.GetOpt.OptDescr) (AppT (AppT ArrowT (ConT CmdOptions_7)) (ConT CmdOptions_7)))),ValD (VarP options_5) (NormalB (ListE [AppE (AppE (AppE (AppE (ConE System.Console.GetOpt.Option) (ListE [])) (ListE [LitE (StringL "name")])) (AppE (AppE (ConE System.Console.GetOpt.ReqArg) (LamE [VarP d_11,VarP opts_12] (RecUpdE (VarE opts_12) [(opt_name_9,VarE d_11)]))) (LitE (StringL "NAME")))) (LitE (StringL "Who to greet.")),AppE (AppE (AppE (AppE (ConE System.Console.GetOpt.Option) (ListE [])) (ListE [LitE (StringL "repeat")])) (AppE (AppE (ConE System.Console.GetOpt.ReqArg) (LamE [VarP d_13,VarP opts_14] (RecUpdE (VarE opts_14) [(opt_repeat_10,AppE (VarE Text.Read.read) (VarE d_13))]))) (LitE (StringL "REPEATS")))) (LitE (StringL "Number of times to repeat the message"))])) []]


-}

dFlag :: QuasiQuoter
dFlag = QuasiQuoter { quoteExp = undefined, quotePat = undefined, quoteDec = qd, quoteType = undefined }
  where {-
        qd s = let tmx = map trim $ lines (stripComments s)
                   tmz = mapM genFlag tmx 
                in sequence $ tmz
        -}
        qd s = let tmx = filter (not . strNull) $ map trim $ lines (stripComments s)
                   tmz = map genFlag tmx 
                in [dataD [] (mkName "CmdOptions") [] [recC (mkName "CmdOptions")
                      (genFields tmx)] ['Show],
                    valD (varP (mkName "defaultOptions") (normalB (recConE (mkName "CmdOptions") (genDefaults tmx) ))) []  ]

        genFlag :: String -> Q Stmt
        genFlag l = let (a,b) = strBrk (isSpace) l
                        c = strDrop 1 b
                        n = mkName ("flag__"++a)
                        val = c 
        --             in ( sigD n (conT (mkName "String"))
        --                ,  valD (varP n) (normalB (litE (stringL val))) [] )
                     in bindS (varP n) (litE (stringL val))

-- | Data type for storing every property of a flag.
data FlagData = FlagData
            { fName :: String
            , fShort :: Maybe Char
            , fDefValue :: String
            , fArgType :: String
            , fDescription :: String
            , fModuleName :: String
            , fCheck :: IO () -- ^ function to evaluate in @initFlags@
                              -- to force syntax check of the argument.
            }

instance Show FlagData where
  show fd = show (fName fd, fShort fd, fDefValue fd, fArgType fd, fDescription fd, fModuleName fd)

-- | Every flag the program supports has to be defined through a new
-- phantom datatype and the Flag instance of that datatype.
--
-- But users of the library shouldn't worry about this class or the
-- implementation details behind these functions, just use the
-- @defineFlag@ Template Haskell function for defining new flags.
class Flag a where
  getFlagData :: a -> FlagData

-- |
-- The parameters:
--
--   * name of the flag (@l:long@ syntax if you want to have the short option @l@ for this flag),
--   * expression quoted and type signed default value,
--   * help string identifying the type of the argument (e.g. INTLIST),
--   * read function, expression quoted,
--   * show function, expression quoted,
--   * help string for the flag.
defineCustomFlag :: String -> ExpQ -> String -> ExpQ -> ExpQ -> String -> Q [Dec]
defineCustomFlag name' defQ argHelp readQ showQ description =
  do (name, short) <-
                      return $ if length name' == 0 then (name', Nothing)
                      else if length name' == 1 then (name', Just $ head name')
                      else if length name' == 2 then (name', Nothing)
                      else if name' !! 1 == ':' then (drop 2 name', Just $ head name')
                      else (name', Nothing)
     defE <- defQ
     flagType <- case defE of
       SigE _ flagType -> return $ return flagType
       _ -> fail "Default value for defineCustomFlag has to be an explicitly typed expression, like (12 :: Int)"
     moduleName <- fmap loc_module location
     let accessorName = mkName $ "flags_" ++ name
     -- attention: formatting of the dataName matters here, initHFlags
     -- parses the name, so the generation here and the parsing in
     -- initHFlags has to be consistent.
     let dataName = mkName $ "HFlag_" ++ name
     let dataConstrName = mkName $ "HFlagC_" ++ name
     -- Note: support for splicing inside [d| |] would make all this a lot nicer
     dataDec <- dataD (cxt []) dataName [] [normalC dataConstrName []] []
     instanceDec <- instanceD
                    (cxt [])
                    (appT (conT ''Flag) (conT dataName))
                      [funD 'getFlagData [clause [wildP]
                                          (normalB
                                           [| FlagData
                                              name
                                              short
                                              $(appE showQ defQ)
                                              argHelp
                                              description
                                              moduleName
                                              -- seq'ng the constructor name, so it's not unused in the generated code
                                              ($(conE dataConstrName) `seq` evaluate $(varE accessorName) >> return ())
                                           |]) []]]
     flagPragmaDec <- pragInlD accessorName NoInline FunLike AllPhases
     flagSig <- sigD accessorName flagType
     flagDec <- funD accessorName [clause [] (normalB $ appE readQ [| lookupFlag name moduleName |]) []]
     return [dataDec, instanceDec, flagPragmaDec, flagSig, flagDec]

-- | Class of types for which the easy 'defineFlag' syntax is supported.
class FlagType t where
  -- | The @defineFlag@ function defines a new flag.
  --   * name of the flag (@l:long@ syntax if you want to have the short option @l@ for this flag),,
  --   * default value,
  --   * help string for the flag.
  defineFlag :: String -> t -> String -> Q [Dec]

boolShow :: Bool -> String
boolShow True = "true"
boolShow False = "false"

boolRead :: String -> Bool
boolRead = boolRead' . map toLower
  where
    boolRead' ('y':_) = True
    boolRead' ('t':_) = True
    boolRead' ('1':_) = True
    boolRead' ('n':_) = False
    boolRead' ('f':_) = False
    boolRead' ('0':_) = False
    boolRead' s = error $ "Unable to parse string as boolean: " ++ s

instance FlagType Bool where
  defineFlag n v = defineCustomFlag n [| v :: Bool |] "BOOL" [| boolRead |] [| boolShow |]

charShow :: Char -> String
charShow x = x:[]

charRead :: String -> Char
charRead [x] = x
charRead s = error $ "Unable to parse string as char: " ++ s

instance FlagType Char where
  defineFlag n v = defineCustomFlag n [| v :: Char |] "CHAR" [| charRead |] [| charShow |]

instance FlagType Int where
  defineFlag n v = defineCustomFlag n [| v :: Int |] "INT" [| read |] [| show |]

instance FlagType Integer where
  defineFlag n v = defineCustomFlag n [| v :: Integer |] "INTEGER" [| read |] [| show |]

instance FlagType String where
  defineFlag n v = defineCustomFlag n [| v :: String |] "STRING" [| id |] [| id |]

instance FlagType Double where
  defineFlag n v = defineCustomFlag n (sigE (litE (RationalL (toRational v))) [t| Double |] ) "DOUBLE" [| read |] [| show |]

instance FlagType Text where
  defineFlag n v =
    -- defer lifting of Data.Text.Text to String lifting
    let s = unpack v
    in defineCustomFlag n [| pack s :: Text |] "TEXT" [| pack |] [| unpack |]

-- | A global 'IORef' for the communication between 'initHFlags' and
-- @flags_*@.  This is a map between flag name and current value.
{-# NOINLINE globalFlags #-}
globalFlags :: IORef (Maybe (Map String String, [String], [String]))
globalFlags = unsafePerformIO $ newIORef Nothing

lookupFlag :: String -> String -> String
lookupFlag fName fModuleName = unsafePerformIO $ do
  flags <- readIORef globalFlags
  case flags of
    Just (flagmap, _, _) -> case mapLookup fName flagmap of
      Just v -> return v
      Nothing -> error $ "Flag " ++ fName ++ " not found at runtime"
    Nothing -> error $ "Flag " ++ fName ++ " (from module: " ++ fModuleName ++ ") used before calling initHFlags."

-- | Initializes 'globalFlags' and returns the non-option arguments.
initFlags :: String -> [FlagData] -> [String] -> IO [String]
initFlags progDescription flags args = do
  doHelp
  let (opts, nonopts, undefopts, errs)
        | doUndefok = getOpt' Permute getOptFlags args
        | otherwise = (\(a,b,c) -> (a,b,[],c)) $ getOpt Permute getOptFlags args
  when (not $ null errs) $ do
    mapM_ (hPutStrLn stderr) errs
    exitFailure
  let defaults = map (\FlagData { fName, fDefValue } -> (fName, fDefValue)) flags
  env <- getEnvironment
  let envDefaults = map (mapFst (fromJust . stripPrefix "HFLAGS_")) $ filter ((isPrefixOf "HFLAGS_") . fst) env
  writeIORef globalFlags $ Just (mapFromList $ defaults ++ envDefaults ++ opts, nonopts, undefopts)
  mapM_ forceFlag flags
  return nonopts
    where
      mapFst f (a, b) = (f a, b)
      helpOption = Option "h" ["help", "usage", "version"] (NoArg ("", "")) "Display help and version information."
      doHelp = case getOpt Permute [helpOption] args of
        ([], _, _) -> return ()
        _ -> do putStrLn $ usageInfo (progDescription ++ "\n") (helpOption:getOptFlags)
                exitFailure

      undefokOption = Option "" ["undefok"] (NoArg ("", "")) "Whether to fail on unrecognized command line options."
      doUndefok = case getOpt Permute [undefokOption] args of
        ([], _, _) -> False
        _ -> True

      flagToGetOptArgDescr FlagData { fName, fArgType }
        | fArgType == "BOOL" = OptArg (\a -> (fName, maybe "True" id a)) fArgType
        | otherwise = ReqArg (\a -> (fName, a)) fArgType

      -- compute GetOpt compatible [Option] structure from flags ([FlagData])
      getOptFlags = undefokOption:
        (flip map flags $ \flagData@(FlagData { fName, fShort, fDefValue, fDescription, fModuleName }) ->
         Option (maybeToList fShort) [fName]
                (flagToGetOptArgDescr flagData)
                (fDescription ++ " (default: " ++ fDefValue ++ ", from module: " ++ fModuleName ++ ")"))

      forceFlag FlagData { fName, fModuleName, fCheck } =
        fCheck `catch`
        (\e -> error $
               "Error while parsing argument for flag: " ++ fName ++
               ", value: " ++ lookupFlag fName fModuleName ++
               ", error: " ++ show (e :: ErrorCall))

-- | Gathers all the flag data from every module that is in (transitive) scope.
-- Type after splicing: @[FlagData]@.
getFlagsData :: ExpQ -- [FlagData]
getFlagsData = do
  ClassI _ instances <- reify ''Flag
  case dupes instances of
    [] -> return ()
    (dupe:_) -> fail ("Multiple definition of flag " ++ (snd $ head dupe) ++
                       ", modules: " ++ (show $ map fst dupe))
  listE $ map instanceToFlagData instances
    where
      instanceToFlagData (InstanceD _ (AppT _ inst) _) = [| getFlagData (undefined :: $(return inst)) |]
      instanceToFlagData _ = error "Shouldn't happen"
      -- Duplicate checking is based on the generated `data HFlag_...'
      -- names, and not on FlagData, because we want to do the checks
      -- at compile time.  It's not possible in TH, to run getFlagData
      -- on the just reified instances.
      instanceToModuleNamePair (InstanceD _ (AppT _ (ConT inst)) _) =
        let (flagrev, modrev) = span (/= '.') $ reverse $ show inst
            modName = reverse $ drop 1 modrev
            flag = drop 1 $ dropWhile (/= '_') $ reverse $ flagrev
        in (modName, flag)
      instanceToModuleNamePair _ = error "Shouldn't happen"
      dupes instances = filter ((>1) . length) $
                        groupBy ((==) `on` snd) $
                        sortBy (compare `on` snd) $
                        map instanceToModuleNamePair instances


-- | Has to be called from the main before doing anything else:
--
-- > main = do args <- $initHFlags "Simple program v0.1"
-- >           ...
--
-- Internally, it uses Template Haskell trickery to gather all the
-- instances of the Flag class and then generates a call to
-- @initFlags@ with the appropriate data gathered together from those
-- instances to a list.
--
-- Type after splicing is @String -> IO [String]@.
initHFlags :: ExpQ -- (String -> IO [String])
initHFlags = do
  [| \progDescription -> getArgs >>= initFlags progDescription $getFlagsData |]

