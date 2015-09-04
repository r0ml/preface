{-# LANGUAGE OverloadedStrings #-}

import Preface.R0ml
-- import Data.ByteString
-- import Bindings.Zlib 
-- import System.Environment

main :: IO ()
main = do
  args <- getArgs

  if Prelude.null args then do
    putStrLn "one"
    let zz = pack [75,44,78,73,75,4,98,32,2,0,0,0,255,255] ::ByteString
    inf <- inflater 
    zPut inf zz  
  
    print =<< zGet inf

    zDone inf

    print =<< zGet inf
  else do
      putStrLn "two"
      let zz = "This is a string of things" :: ByteString
      def <- deflater

      putStrLn "a" 
      zPut def zz
    
      putStrLn "b"
      print =<< zGet def

      putStrLn "c"
      zDone def

      hh <- zGet def

      inf <- inflater
      
      q <- case hh of 
        Right h -> zPut inf h
        Left h -> print h

      print =<< zGet inf
      zDone inf
      print =<< zGet inf

      -- Data.ByteString.putStrLn . right =<< zGet def

{-
right :: Either a b -> b
right (Right x) = x
  -}  


