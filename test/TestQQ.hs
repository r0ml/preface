{-# LANGUAGE QuasiQuotes #-}

import Preface.R0ml

assertEqualG :: (Eq a, Show a) => IORef Bool -> String -> a -> a -> IO ()
assertEqualG g xstr a b = do
  let t = a == b
  if t then putStr "passing " else putStr ("failing (" ++ show a ++ " /= " ++ show b ++ ") ")
  putStrLn xstr
  modifyIORef g (&& t)

zz :: SomeException -> IO [(String,String)]
zz _e = return []

main :: IO ()
main = do
  globl <- newIORef True

  -- ( "one", do 
  --    a <- [url|http://acme.com|]
  --    assertEqual "str url (acme.com)" 9570 (strLen a) ),
  catch [opts|$HOME/.aws.hs|] zz >>= assertEqualG globl "opts .aws.hs"  5 . length

-- this tests to make sure that Strings and Ints interpolate properly (qqshow)
  let b = "clem"
      c = 334 :: Int
  u <- do { a <- lookupEnv "HOME" ; return (fromMaybe "." a) }
  let v = [istr|$HOME/$b-$c|]
  assertEqualG globl "istr $HOME/$b-$c" (u++"/clem-334") v 

  assertEqualG globl "str No interpolation" "No interpolation" [qqstr|No interpolation|]

  [qqfile|$HOME/.profile|] >>= assertEqualG globl "qqfile $HOME/.profile" False . null

  (e, r, w) <- [qqsh|uname -s|]
  assertEqualG globl "qqsh uname -s" "Darwin" w


{-  ,("seven", TestCase $ do
    a <- [python|import os,sys; print os.isatty(0)|]
    assertEqual "python os.isatty(0)" (Right "False") a)
    -}


  z <- readIORef globl
  if z then exitSuccess else exitFailure


-- $(interpolate =<< runIO [qqfile|$HOME/clem|]


-- let ff x = [qqsh|$x|]
-- (either (putStrLn . show) putStrLn ) =<< (ff "ls -lR")

