{-# LANGUAGE QuasiQuotes #-}

import Preface.R0ml

assertEqualG :: (Eq a, Show a) => IORef Bool -> String -> a -> a -> IO ()
assertEqualG g str a b = do
  let t = a == b
  if t then putStr "passing " else putStr ("failing (" ++ show a ++ " /= " ++ show b ++ ") ")
  putStrLn str
  modifyIORef g (&& t)

zz :: SomeException -> IO [(String,String)]
zz _e = return []

main = do
  global <- newIORef True

  -- ( "one", do 
  --    a <- [url|http://acme.com|]
  --    assertEqual "str url (acme.com)" 9570 (strLen a) ),
  catch [opts|$HOME/.aws.hs|] zz >>= assertEqualG global "opts .aws.hs"  5 . length

-- this tests to make sure that Strings and Ints interpolate properly (qqshow)
  let b = "clem"
      c = 334 :: Int
  u <- do { a <- lookupEnv "HOME" ; return (fromMaybe "." a) }
  let v = [iostr|$HOME/$b-$c|]
  assertEqualG global "str $HOME/$b-$c" (u++"/clem-334") v 

  assertEqualG global "str No interpolation" "No interpolation" [str|No interpolation|]

  [file|$HOME/.profile|] >>= assertEqualG global "file $HOME/.profile" False . null

  w <- [sh|uname -s|]
  assertEqualG global "sh uname -s" (Right "Darwin") w


{-  ,("seven", TestCase $ do
    a <- [python|import os,sys; print os.isatty(0)|]
    assertEqual "python os.isatty(0)" (Right "False") a)
    -}


  z <- readIORef global
  if z then exitSuccess else exitFailure


-- $(interpolate =<< runIO [file|$HOME/clem|]


-- let ff x = [sh|$x|]
-- (either (putStrLn . show) putStrLn ) =<< (ff "ls -lR")

