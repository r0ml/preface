{-# LANGUAGE OverloadedStrings #-}

import Preface.R0ml
-- import Data.ByteString
-- import Bindings.Zlib 
-- import System.Environment

main :: IO ()
main = do
  args <- getArgs

  if Prelude.null args then do
    print "one"
    let zz = pack [75,44,78,73,75,4,98,32,2,0,0,0,255,255] ::ByteString
    inf <- inflater 
    zPut inf zz  
  
    print =<< zGet inf

    zDone inf

    print =<< zGet inf
  else do
      print "two"
      let zz = "This is a string of things" :: ByteString
      def <- deflater

      print "a" 
      zPut def zz
    
      print "b"
      print =<< zGet def

      print "c"
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

right (Right x) = x
    


