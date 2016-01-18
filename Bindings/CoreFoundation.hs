{-# LANGUAGE QuasiQuotes #-}
-- {-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module Bindings.CoreFoundation

where

import Preface.Imports
import Preface.FFITemplates
import Preface.Misc 

-- --------------------------------------------------------
-- CFArray
-- --------------------------------------------------------
foreign import ccall "CFArrayCreate" c_CFArrayCreate :: Ptr () -> Ptr CFStringRef -> CInt -> Ptr () -> IO CFArrayRef

foreign import ccall "CFStringCreateWithCString" c_CFStringCreateWithCString :: Ptr () -> CString -> CFStringEncoding -> IO CFStringRef

kCFStringEncodingUTF8 :: CFStringEncoding
kCFStringEncodingUTF8 = 0x08000100

type CFArrayRef = Ptr CFArray
data CFArray

type CFStringEncoding = CInt

cfStringCreate :: String -> IO CFStringRef
cfStringCreate s = withCString s $ \x -> c_CFStringCreateWithCString nullPtr x kCFStringEncodingUTF8

foreign import ccall "CFStringGetMaximumSizeForEncoding" c_CFStringGetMaximumSizeForEncoding :: CFIndex -> CFStringEncoding -> IO CFIndex


foreign import ccall "CFStringGetCString" c_CFStringGetCString ::
  CFStringRef -> Ptr CChar -> CInt -> CFStringEncoding -> IO CBool

foreign import ccall "CFStringGetLength" c_CFStringGetLength :: CFStringRef -> IO CFIndex

stringFromCFString :: CFStringRef -> IO String
stringFromCFString v = do
    n <- c_CFStringGetLength v
    size <- c_CFStringGetMaximumSizeForEncoding (fromIntegral n) kCFStringEncodingUTF8
    allocaArray (fromIntegral size + 1) $ \x -> do
      _ <- c_CFStringGetCString v x (fromIntegral size) kCFStringEncodingUTF8
      peekCString x
 
cfArrayOfStrings :: [String] -> IO CFArrayRef
cfArrayOfStrings z = do
    let n = length z
    p <- mallocArray n :: IO (Ptr CFStringRef)
    res <- sets p z (c_CFArrayCreate nullPtr p (fromIntegral n) nullPtr)
    return res
  where sets p s f = if null s then f
          else withCString (head s) $ \sp -> do
                   spx <- c_CFStringCreateWithCString nullPtr sp kCFStringEncodingUTF8
                   poke p spx
                   sets (plusPtr p (sizeOf (undefined::CString))) (tail s) f

data CFRunLoop
type CFRunLoopRef = Ptr CFRunLoop

type CFIndex = CLong

foreign import ccall "CFArrayGetCount" c_CFArrayGetCount :: CFArrayRef -> IO CFIndex

foreign import ccall "CFArrayGetValueAtIndex" c_CFArrayGetValueAtIndex :: CFArrayRef -> CFIndex -> IO (Ptr ())

data CFDictionary
type CFDictionaryRef = Ptr CFDictionary

data CFMutableDictionary
type CFMutableDictionaryRef = Ptr CFMutableDictionary

foreign import ccall "CFDictionaryGetValue" c_CFDictionaryGetValue :: CFDictionaryRef -> Ptr () -> IO (Ptr ())



data CFString
type CFStringRef = Ptr CFString

foreign import ccall "CFRunLoopGetCurrent" c_CFRunLoopGetCurrent :: IO CFRunLoopRef

foreign import ccall "CFRunLoopRun" c_CFRunLoopRun :: CFRunLoopRef -> IO ()

foreign import ccall "CFRelease" c_CFRelease :: Ptr () -> IO ()


type CFTimeInterval = CDouble

[storable| Mach_timebase_info_data
  numer CUInt
  denom CUInt
|]

foreign import ccall mach_absolute_time :: IO CULong
foreign import ccall mach_timebase_info :: Ptr Mach_timebase_info_data -> IO ()

