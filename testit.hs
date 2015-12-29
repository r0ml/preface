#!/usr/bin/env runhaskell -ddump-splices
{-# LANGUAGE QuasiQuotes #-}
import Preface

main = do

  z <-  ObjcId <$> nsString "this is a test"
  a1 <- [objc|z length|] :: IO Int
  print a1

  a3 <- [objc|z cString|] :: IO String
  print a3

  let nssi = objcClassObject $ objcClass "NSString"
      sarg = "another Test"
  a2<-[objc|nssi stringWithUTF8String: sarg|] :: IO ObjcId 
  print a2

  nsBundleLoad "Cocoa"
{-
  d <- objcWrapOO (\x y z -> do { a <- peek (castPtr z) :: IO CInt; print (x,y,a);  return nullPtr } )
  a <- withCString "GLFWContentView" $ \y -> objc_allocateClassPair (objcClass "NSView") y 16
  bb <- withCString "v@:@" $ class_addMethod a (objcSel "initWithCallback:") (castFunPtr d)
  objc_registerClassPair a
  let p = objcClass "GLFWContentView"
  z <- objc_msgSendO p (objcSel "alloc")
  with (CInt 808) $ objc_msgSendOO z (objcSel "initWithCallback:") . castPtr 
-}

