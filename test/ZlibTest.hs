{-# LANGUAGE OverloadedStrings #-}

import Preface.R0ml

main :: IO ()
main = do
  args <- getArgs

  if null args then do
    putStrLn "one"
    let zz = pack [75,44,78,73,75,4,98,32,2,0,0,0,255,255] ::ByteString
    inf <- inflater 
    zPut inf zz  
  
    print =<< zGet inf

    zDone inf

    print =<< zGet inf
  else case head args of
    "one" -> do putStrLn "two"
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

    "two" -> do a <- inflater2
                c <- strReadFile (head ( tail args))
                zPut a c
                zDone a
--                 _ <- either (const False) ((0 ==) . strLen) |-> zGet a --< putStrLn . (either show show)
--                 return ()

                whilex (either (const True) ((0 ==) . strLen)) (zGet a) (\x -> strPut (either (asByteString . show) id x))
                
                
whilex :: (a -> Bool) -> IO a -> (a -> IO ()) -> IO ()
whilex p f d = go
    where go = do
            x <- f
            if p x
                then return ()
                else d x >> go

