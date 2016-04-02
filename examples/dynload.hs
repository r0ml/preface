import GHC
import GHC.Paths (libdir)
import DynFlags
import Unsafe.Coerce

import Control.Monad.IO.Class (liftIO)

dynload str = 
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    x <- runGhc (Just libdir) $ do 
      dflagsx <- getSessionDynFlags 

--    let m = mkModule (thisPackage dflags) (mkModuleName str)

      let pkgs = map PkgConfFile ["/Volumes/Storage/Repositories/apache/mod_ghc/mod_git/.cabal-sandbox/x86_64-osx-ghc-7.8.2-packages.conf.d"]
    
      let dflags = dflagsx { ghcLink = LinkInMemory, hscTarget = HscInterpreted, traceLevel = 9,
                             extraPkgConfs = (pkgs ++) . extraPkgConfs dflagsx }

      let ppp = "/Users/r0ml/Repositories/mod_ghc/mod_git"

      setSessionDynFlags dflags {importPaths = [ppp], libraryPaths = [ppp], frameworkPaths = [ppp] }
    -- setContext [IIModule (mkModuleName str)]

      setTargets =<< sequence [guessTarget str Nothing
    	       	   	     ]
      load LoadAllTargets

      setContext [IIDecl ((simpleImportDecl . mkModuleName) str)]
    
--    setContext [IIModule $ mkModuleName str]

      app <- compileExpr (str ++ "." ++ "moduleMain")
      return ( unsafeCoerce app :: [(String,String)] -> IO () )

  -- print x
    return x


main = do
  app <- dynload  "Apache.Git"
  app [("done", "one"),("yeah","awesome")]
