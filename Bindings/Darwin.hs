{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Bindings.Darwin (
    ObjcClass, ObjcId, ObjcIvar, ObjcMethod, ObjcSel
  , objcClass
  , objcSel
  , objcGetIvarList
  , objcGetMethodList
  , objcGetSuperclasses
  , objcGetIvarInfo
  , objcGetMethodTypeEncoding
  , objcGetMethodTypes
--  , objcGetMethod
  , nsBundleLoad
--  , module Bindings.Darwin
) where

import Preface.Imports
import Preface.Pipes
import Preface.Misc

type Objc_Id = Ptr ()
type SEL = Ptr Objc_Selector
data Objc_Selector

type IMP = FunPtr ( Objc_Class -> SEL -> IO Objc_Id )

data Objc_StructMethod
type Objc_Method = Ptr Objc_StructMethod

type Objc_Class = Objc_Id

data Objc_StructIvar
type Objc_Ivar = Ptr Objc_StructIvar

type NSString = Objc_Id

newtype ObjcId = ObjcId Objc_Id
newtype ObjcClass = ObjcClass Objc_Class
newtype ObjcIvar = ObjcIvar Objc_Ivar
newtype ObjcMethod = ObjcMethod Objc_Method
newtype ObjcSel = ObjcSel SEL

instance Show ObjcClass where
  show (ObjcClass a) = unsafePerformIO $ peekCString $ class_getName a 

instance Show ObjcIvar where
  show (ObjcIvar a) = unsafePerformIO $ ivar_getName a >>= peekCString

instance Show ObjcMethod where
  show (ObjcMethod a) = unsafePerformIO $ sel_getName (method_getName a) >>= peekCString  

instance Show ObjcSel where
  show (ObjcSel a) = unsafePerformIO $ sel_getName a >>= peekCString

-- foreign import ccall "wrapper" mkPathEvent :: EventStreamCallbackType -> IO (FunPtr EventStreamCallbackType)

type Objc_FnOO = Objc_Id -> SEL -> Objc_Id -> IO Objc_Id
foreign import ccall "wrapper" objcWrapOO :: Objc_FnOO -> IO (FunPtr Objc_FnOO)

type Objc_FnO = Objc_Id -> SEL -> IO Objc_Id
foreign import ccall "wrapper" objcWrapO :: Objc_FnO -> IO (FunPtr Objc_FnO)

foreign import ccall "objc_getClass" c_objc_getClass :: CString -> IO Objc_Class

foreign import ccall class_getName :: Objc_Class -> CString



foreign import ccall class_getSuperclass :: Objc_Class -> Objc_Class
objcGetSuperclasses :: ObjcClass -> [ObjcClass]
objcGetSuperclasses (ObjcClass a) = 
  let b = class_getSuperclass a
      c = ObjcClass b
   in if b == nullPtr then [] else c : objcGetSuperclasses c


foreign import ccall class_setSuperclass :: Objc_Class -> Objc_Class -> IO Objc_Class
foreign import ccall class_isMetaClass :: Objc_Class -> CBool
foreign import ccall class_getInstanceSize :: Objc_Class -> CLong
foreign import ccall class_getInstanceVariable :: Objc_Class -> CString -> IO Objc_Ivar
foreign import ccall class_getClassVariable :: Objc_Class -> CString -> IO Objc_Ivar

foreign import ccall class_addIvar :: Objc_Class -> CString -> CLong -> CChar -> CString -> CBool
{-
 class_addIvar(c, "firstName", sizeof(id), log2(sizeof(id)), @encode(id));
 class_addIvar(c, "lastName", sizeof(id), log2(sizeof(id)), @encode(id));
 class_addIvar(c, "age", sizeof(NSUInteger), log2(sizeof(NSUInteger)), @encode(NSUInteger));
-}

foreign import ccall class_copyIvarList :: Objc_Class -> Ptr CUInt -> IO (Ptr Objc_Ivar)
objcGetIvarList :: ObjcClass -> IO [ObjcIvar]
objcGetIvarList (ObjcClass c) = alloca $ \x -> do 
         y <- class_copyIvarList c x
         b <- peek x
         d <- peekArray (fromEnum b) y
         free y
         return (map ObjcIvar d)

-- foreign import ccall class_getIvarLayout :: Objc_Class -> IO CString
-- foreign import ccall class_setIvarLayout :: Objc_Class -> CString -> IO ()

foreign import ccall class_addMethod :: Objc_Class -> SEL -> IMP -> CString -> IO CBool

foreign import ccall class_getInstanceMethod :: Objc_Class -> SEL -> IO Objc_Method
foreign import ccall class_getClassMethod :: Objc_Class -> SEL -> IO Objc_Method


-- Method class_getInstanceMethod(Class aClass, SEL aSelector)
-- Method class_getClassMethod(Class aClass, SEL aSelector)
--
foreign import ccall class_copyMethodList :: Objc_Class -> Ptr CUInt -> IO (Ptr Objc_Method)
objcGetMethodList :: ObjcClass -> IO [ObjcMethod]
objcGetMethodList (ObjcClass c) = alloca $ \x -> do 
         y <- class_copyMethodList c x
         b <- peek x
         d <- peekArray (fromEnum b) y
         free y
         return (map ObjcMethod d)

-- objcGetMethod :: ObjcClass -> ObjcSel -> ObjcMethod
-- objcGetMethod c s = do
  
objcGetMethodTypeEncoding :: ObjcMethod -> String
objcGetMethodTypeEncoding (ObjcMethod m) = unsafePerformIO (
  method_getTypeEncoding m >>= peekCString)

objcGetMethodTypes :: ObjcMethod -> [String]
objcGetMethodTypes (ObjcMethod mm) = unsafePerformIO $ do
   let a = method_getNumberOfArguments mm
   b <- method_copyReturnType mm
   c <- peekCString b
   free b
   e <- mapM (\x -> do 
                      bb <- method_copyArgumentType mm (fromIntegral x) 
                      cc <- peekCString bb
                      free bb
                      return cc
             ) [0..fromEnum a - 1]
   return (c:e)

foreign import ccall method_getName :: Objc_Method -> SEL

foreign import ccall method_getTypeEncoding :: Objc_Method -> IO CString

foreign import ccall method_copyReturnType :: Objc_Method -> IO CString
foreign import ccall method_copyArgumentType :: Objc_Method -> CUInt -> IO CString
foreign import ccall method_getNumberOfArguments :: Objc_Method -> CUInt

--
--

-- IMP class_replaceMethod(Class cls, SEL name, IMP imp, const char *types)
-- IMP class_getMethodImplementation(Class cls, SEL name) 
-- IMP class_getMethodImplementation_stret(Class cls, SEL name)
-- BOOL class_respondsToSelector(Class cls, SEL sel)
-- BOOL class_addProtocol(Class cls, Protocol *protocol)
-- BOOL class_conformsToProtocol(Class cls, Protocol *protocol)
-- Protocol ** class_copyProtocolList(Class cls, unsigned int *outCount)

foreign import ccall class_getVersion :: Objc_Class -> IO CUInt
foreign import ccall class_setVersion :: Objc_Class -> CUInt -> IO ()
--

foreign import ccall objc_allocateClassPair :: Objc_Class -> CString -> CLong -> IO Objc_Class
foreign import ccall objc_registerClassPair :: Objc_Class -> IO ()


-- void objc_disposeClassPair(Class cls)
--
-- id class_createInstance(Class cls, size_t extraBytes)
-- id objc_constructInstance(Class cls, void *bytes)
-- void objc_destructInstance(id obj)
--
-- id object_copy(id obj, size_t size)
-- id object_dispose(id obj)
-- Ivar object_setInstanceVariable(id obj, const char *name, void *value)
-- Ivar object_getInstanceVariable(id obj, const char *name, void **outValue)
-- void *object_getIndexedIvars(id obj)
--
-- id object_getIvar(id object, Ivar ivar)
-- void object_setIvar(id object, Ivar ivar, id value)

{-
  object_setIvar(self, firstNameIvar, firstName);
  object_setIvar(self, lastNameIvar, lastName);
            
  char *agePtr = ((char *)(__bridge void *)self) + ageOffset;
  memcpy(agePtr, &age, sizeof(age));
-}

foreign import ccall object_getClassName :: Objc_Id -> IO CString
objcGetClassName :: Objc_Id -> IO String
objcGetClassName x = object_getClassName x >>= peekCString

-- Class object_setClass(id object, Class cls)
-- int objc_getClassList(Class *buffer, int bufferLen)

foreign import ccall objc_copyClassList :: Ptr CUInt -> IO (Ptr Objc_Class)
objcGetClassList :: IO [ObjcClass]
objcGetClassList = alloca $ \x -> do 
         y <- objc_copyClassList x
         b <- peek x
         c <- peekArray (fromEnum b) y
         free y
         return (map ObjcClass c)

foreign import ccall objc_lookupClass :: CString -> Objc_Class
foreign import ccall objc_getMetaClass :: CString -> Objc_Class

foreign import ccall ivar_getName :: Objc_Ivar -> IO CString
foreign import ccall ivar_getTypeEncoding :: Objc_Ivar -> IO CString
foreign import ccall ivar_getOffset :: Objc_Ivar -> IO CLong

objcGetIvarInfo :: Objc_Ivar -> IO (String, String, Int)
objcGetIvarInfo x = do
  a <- ivar_getName x 
  b <- ivar_getTypeEncoding x
  c <- ivar_getOffset x
  a2 <- peekCString a
  b2 <- peekCString b
  return (a2, b2, fromEnum c)

--
-- double objc_msgSend_fpret(id self, SEL op, ...)
-- void objc_msgSend_stret(void * stretAddr, id theReceiver, SEL theSelector, ...)
-- id objc_msgSendSuper(struct objc_super *super, SEL op, ...)
-- void objc_msgSendSuper_stret(struct objc_super *super, SEL op, ...)
--
-- const char **objc_copyImageNames(unsigned int *outCount)

foreign import ccall class_getImageName :: Objc_Class -> IO CString

objcGetImageName :: Objc_Class -> IO String
objcGetImageName x = class_getImageName x >>= peekCString

-- const char **objc_copyClassNamesForImage(const char *image, unsigned int *outCount)
--
foreign import ccall sel_getName :: SEL -> IO CString
foreign import ccall sel_registerName :: CString -> IO SEL
foreign import ccall "sel_getUid" c_sel_getUid :: CString -> IO SEL

showSel :: SEL -> String
showSel a = unsafePerformIO $ sel_getName a >>= peekCString

-- BOOL sel_isEqual(SEL lhs, SEL rhs)
--


objc_Sel :: String -> SEL 
objc_Sel x = unsafePerformIO $ withCString x c_sel_getUid

objcSel :: String -> ObjcSel
objcSel = ObjcSel . objc_Sel

objc_Class :: String -> Objc_Class
objc_Class x = unsafePerformIO $ withCString x c_objc_getClass

objcClass :: String -> ObjcClass
objcClass = ObjcClass . objc_Class

nsString :: String -> IO NSString
nsString x = withCString x $ \y -> objc_msgSendOO (objc_Class "NSString") (objc_Sel "stringWithUTF8String:") (castPtr y)

foreign import ccall "objc_msgSend_stret" objc_msgSend_stret0 :: Ptr () -> Objc_Id -> SEL -> IO ()
foreign import ccall "objc_msgSend_stret" objc_msgSend_stret1 :: Ptr () -> Objc_Id -> SEL -> Objc_Id -> IO ()
foreign import ccall "objc_msgSend_stret" objc_msgSend_stret2 :: Ptr () -> Objc_Id -> SEL -> Objc_Id -> Objc_Id -> IO ()
foreign import ccall "objc_msgSend_stret" objc_msgSend_stret3 :: Ptr () -> Objc_Id -> SEL -> Objc_Id -> Objc_Id -> Objc_Id -> IO ()
foreign import ccall "objc_msgSend_stret" objc_msgSend_stret4 :: Ptr () -> Objc_Id -> SEL -> Objc_Id -> Objc_Id -> Objc_Id -> Objc_Id -> IO ()

foreign import ccall "objc_msgSend" objc_msgSendO :: Objc_Id -> SEL -> IO Objc_Id
foreign import ccall "objc_msgSend" objc_msgSendOO :: Objc_Id -> SEL -> Objc_Id -> IO Objc_Id
foreign import ccall "objc_msgSend" objc_msgSendOOO :: Objc_Id -> SEL -> Objc_Id -> Objc_Id -> IO Objc_Id

foreign import ccall "objc_msgSend" objc_msgSendI :: Objc_Id -> SEL -> IO CInt

foreign import ccall "objc_msgSend" objc_msgSendOI :: Objc_Id -> SEL -> CInt -> IO Objc_Id
foreign import ccall "objc_msgSend" objc_msgSendCI :: Objc_Id -> SEL -> CInt -> IO CChar
foreign import ccall "objc_msgSend" objc_msgSendCO :: Objc_Id -> SEL -> Objc_Id -> IO CChar

foreign import ccall "objc_msgSend" objc_msgSendV :: Objc_Id -> SEL -> IO ()
foreign import ccall "objc_msgSend" objc_msgSendVC :: Objc_Id -> SEL -> CChar -> IO ()
foreign import ccall "objc_msgSend" objc_msgSendVO :: Objc_Id -> SEL -> Objc_Id -> IO ()
foreign import ccall "objc_msgSend" objc_msgSendVOI :: Objc_Id -> SEL -> Objc_Id -> CInt -> IO ()
foreign import ccall "objc_msgSend" objc_msgSendVDD :: Objc_Id -> SEL -> CDouble -> CDouble -> IO ()

{-
printf :: (PrintfType r) => String -> r
printf fmts = spr fmts []

class PrintfType t where
    spr :: String -> [UPrintf] -> t

instance (IsChar c) => PrintfType [c] where
    spr fmts args = map fromChar (uprintf fmts (reverse args))

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
    spr fmts args = \a -> spr fmts (toUPrintf a : args)
-}
-- --------------------------------------------
{-

objc_msgSend :: Objc_MsgSendImpl r => Objc_Id -> SEL -> r
objc_msgSend idx selx = objc_msgSendImpl idx selx mempty

class Objc_MsgSendImpl r where
  objc_msgSendImpl :: Argumentative a => Objc_Id -> SEL -> [a] -> r

instance Objc_MsgSendImpl Objc_Id where
  objc_msgSendImpl self sel acc = constructActualResult self sel acc

instance (Argumentative a, Objc_MsgSendImpl r) => Objc_MsgSendImpl (a -> r) where
  objc_msgSendImpl self sel acc = \ab -> objc_msgSendImpl self sel (specialize ab `mappend` acc)
-}


{-
variadicFunction :: VariadicReturnClass r => RequiredArgs -> r
variadicFunction reqArgs = variadicImpl reqArgs mempty

class VariadicReturnClass r where
   variadicImpl :: RequiredArgs -> AccumulatingType -> r

instance VariadicReturnClass ActualReturnType where
   variadicImpl reqArgs acc = constructActualResult reqArgs acc

instance (ArgClass a, VariadicReturnClass r) => VariadicReturnClass (a -> r) where
   variadicImpl reqArgs acc = \ab -> variadicImpl reqArgs (specialize ab `mappend` acc)
-}
-- ----------------------------------------------------------
{-
class SumRes r where 
    sumOf :: Integer -> r

instance SumRes Integer where
    sumOf = id

instance (Integral a, SumRes r) => SumRes (a -> r) where
    sumOf x = sumOf . (x +) . toInteger



-}

nsMethodSignatureForSelector :: Bool -> String -> String -> IO [String]
nsMethodSignatureForSelector c cl se = do
  let clx = objc_Class cl
      sex = objc_Sel se
  r <- if c then objc_msgSendOO clx (objc_Sel "methodSignatureForSelector:") (castPtr sex)
       else objc_msgSendOO clx (objc_Sel "instanceMethodSignatureForSelector:") (castPtr sex)
  if r == nullPtr then return [] 
  else do
    n <- objc_msgSendI r (objc_Sel "numberOfArguments") 
    z <- mapM (fn r) [2..n-1]
    y <- objc_msgSendO r (objc_Sel "methodReturnType")
    h <- peekCString (castPtr y) 
    return (h : z)
 where fn r x = do
            a <- objc_msgSendOI r (objc_Sel "getArgumentTypeAtIndex:") x
            peekCString (castPtr a)

nsInvoke :: forall a. forall b. (Argumentative a, Storable b, Argumentative b) => ObjcId -> ObjcSel -> [a] -> IO b 

{-
nsInvoke (ObjcId x) sxi@(ObjcSel sx) [] = do
  ro <- objc_msgSendCO x (objc_Sel "respondsToSelector:") (castPtr sx)
  if ro /= 0 then do
     z <- objc_msgSendO x sx
     return z 
  else error ("invalid selector for this object: " ++ show sxi)
-}
foreign import ccall object_getClass :: Objc_Id -> IO Objc_Class

nsInvoke (ObjcId x) sxi@(ObjcSel sx) al = do
  let zz = undefined :: b
      max = map argTypeOf al
      sig = argTypeOf zz ++ "@:"++ concat max
  cls <- object_getClass x
  let mcq = class_isMetaClass cls
  m <- if fromEnum mcq == 0 then class_getInstanceMethod cls sx 
                   else class_getClassMethod x sx
  let ma = objcGetMethodTypes (ObjcMethod m)
  print (show max)
  print (show ma)

  ro <- objc_msgSendCO x (objc_Sel "respondsToSelector:") (castPtr sx)
  if ro /= 0 then do
    nsig <- withCString sig $ objc_msgSendOO (objc_Class "NSMethodSignature") (objc_Sel "signatureWithObjCTypes:") . castPtr

    inv <- objc_msgSendOO (objc_Class "NSInvocation") (objc_Sel "invocationWithMethodSignature:") nsig
    objc_msgSendVO inv (objc_Sel "setSelector:") (castPtr sx)

    
    mapM_ (\(aa,nn) -> asArg aa $ \z-> objc_msgSendVOI inv (objc_Sel "setArgument:atIndex") (castPtr z) (fromIntegral nn)) (zip al [2 .. (1 + length al) ])

    objc_msgSendVO inv (objc_Sel "invokeWithTarget:") x
    res <- alloca $ (\q -> objc_msgSendVO inv (objc_Sel "getReturnValue:") (castPtr q) >> fromArg q)
    return res
  else error ("invalid selector for this object: " ++ show sxi)

{-
nsInvocation :: ObjcId a -> String -> 
nsInvocation x = do

  (objclass "NSInvocation) (objSel "invocationWithMethodSignature:") 

  + (NSInvocation *)invocationWithMethodSignature:(NSMethodSignature *)signature
-}

class Argumentative a where
  type ArgRawType a
 
  argTypeOf :: a -> String
  -- argSizeOf :: a -> Int
  -- argSizeOf a = sizeOf a
  asArg :: a -> ( (Ptr (ArgRawType a) -> IO c) -> IO c )
  fromArg :: Storable (ArgRawType a) => (Ptr (ArgRawType a)) -> IO a 

-- instance Argumentative CChar where 
--   { argTypeOf _ = "c"; type ArgRawType CChar = CChar; asArg = with }
instance Argumentative Char where 
  argTypeOf _ = "c"
  type ArgRawType Char = CChar
  asArg = with . CChar . fromIntegral . ord
  fromArg = fmap (chr . fromEnum) . peek

-- instance Argumentative CInt where 
--   { argTypeOf _ = "i"; type ArgRawType CInt = CInt; asArg = with }
instance Argumentative Int where 
  argTypeOf _ = "i"
  type ArgRawType Int = CInt
  asArg = with . CInt . fromIntegral
  fromArg = fmap fromEnum . peek

{-
instance Argumentative CShort where { argTypeOf _ = "s" }
-- instance Argumentative CInt where { argTypeOf _ = "l" }
instance Argumentative CLong where { argTypeOf _ = "q" }
instance Argumentative CUChar where { argTypeOf _ = "C" }
instance Argumentative CUInt where { argTypeOf _ = "I" }
instance Argumentative CUShort where { argTypeOf _ = "S" }
-- instance Argumentative CUInt where { argTypeOf _ = "L" }
instance Argumentative CULong where { argTypeOf _ = "Q" }
instance Argumentative CFloat where { argTypeOf _ = "f" }
instance Argumentative CDouble where { argTypeOf _ = "d" }
-- instance Argumentative CBool where { argTypeOf _ = "B" }
-- instance Argumentative () where { argTypeOf _ = "v" }
-}

instance Argumentative String where
 argTypeOf _ = "*"
 type ArgRawType String = CChar
 asArg = withCString
 fromArg = peekCString

instance Argumentative ObjcId where
 argTypeOf _ = "@"
 type ArgRawType ObjcId = Objc_Id
 asArg (ObjcId a) = with a
 fromArg = fmap ObjcId . peek

-- instance Argumentative Objc_Class { where argTypeOf _ = "#" }
instance Argumentative ObjcSel where 
 argTypeOf _ = ":"
 type ArgRawType ObjcSel = SEL
 asArg (ObjcSel a) = with a
 fromArg = fmap ObjcSel . peek

-- instance (Argumentative a) => Argumentative [a] where { argTypeOf _ = "[": (argTypeOf (undefined :: a) ) ++ "]" }

instance Argumentative a => Argumentative (Ptr a) where 
 argTypeOf _ = '^':argTypeOf ( undefined :: a)
 type ArgRawType (Ptr a) = Ptr a
 asArg = with
 fromArg = peek

{-
 - Table 6-1  Objective-C type encodings
c A char
i An int
s A short
l A long l is treated as a 32-bit quantity on 64-bit programs.
q A long long
C An unsigned char
I An unsigned int
S An unsigned short
L An unsigned long
Q An unsigned long long
f A float
d A double
B A C++ bool or a C99 _Bool
v A void
* A character string (char *)
@ An object (whether statically typed or typed id)
# A class object (Class)
: A method selector (SEL)
[array type] An array
{name=type...} A structure
(name=type...) A union
bnum A bit field of num bits
^type A pointer to type
?  An unknown type (among other things, this code is used for function pointers)
-}
{-
Code Meaning
r const
n in
N inout
o out
O bycopy
R byref
V oneway
 -} 
nsBundleLoad :: String -> IO Bool
nsBundleLoad x = do
  let f = \y -> objc_msgSendOO (objc_Class "NSBundle") (objc_Sel "bundleWithPath:") =<< (nsString (y ++ x ++ ".framework"))
      p = ["/System/Library/Frameworks/", "/Library/Frameworks/"]
  b <- first (nullPtr /= ) (map f p)
  case b of
     Nothing -> return False
     Just bb -> objc_msgSendV bb (objc_Sel "load") >> return True


  
