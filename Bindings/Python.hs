{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bindings.Python (
         -- * Initialization
         pyInitialize
         -- * Types
       , PyObject(..)
         -- * Errors
       , PythonException(..)
         -- * Modules
       , importModule
         -- * Object access
       , getAttr
       , callObject
         -- * Value conversion
       , Pythonic(..)
       , initModule
       , PyCFunction
       , PyMethodDef(..)
       , toPyObject
       , RawPyObject
       , pyRunString
       , py_None
       , asTuple
       , asDict
       , fromTuple
) where

-- #include <Python.h>
import Preface.Imports
import Preface.Stringy

import Data.ByteString (useAsCStringLen)
import Foreign.C (withCAString)

type RawPyObject = Ptr ()
type PyInt = CInt -- #type int
type PySSizeT = CLong -- #type Py_ssize_t

-- PyInt_AsLong
-- PyInt_FromLong

foreign import ccall unsafe "Python.h &_Py_NoneStruct"
  py_None :: RawPyObject

foreign import ccall unsafe "Python.h &Py_DecRef"
  pyDecRef :: FunPtr (RawPyObject -> IO ())

foreign import ccall unsafe "Python.h Py_IncRef"
  pyIncRef :: RawPyObject -> IO ()

foreign import ccall unsafe "Python.h PyImport_ImportModule"
  pyImport_ImportModule :: CString -> IO RawPyObject

foreign import ccall unsafe "Python.h Py_InitializeEx"
  pyInitializeEx :: PyInt -> IO ()

foreign import ccall unsafe "Python.h PyString_FromStringAndSize"
  pyString_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyString_AsStringAndSize"
  pyString_AsStringAndSize :: RawPyObject -> Ptr CString -> Ptr PySSizeT -> IO PyInt

foreign import ccall unsafe "Python.h PyUnicodeUCS2_AsUTF8String"
  pyUnicode_AsUTF8String :: RawPyObject -> IO RawPyObject

foreign import ccall unsafe "Python.h PyUnicodeUCS2_FromStringAndSize"
  pyUnicode_FromStringAndSize :: CString -> PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyTuple_New"
  pyTuple_New :: PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyTuple_SetItem"
  pyTuple_SetItem :: RawPyObject -> PySSizeT -> RawPyObject -> IO PyInt

foreign import ccall unsafe "Python.h PyTuple_GetItem"
  pyTuple_GetItem :: RawPyObject -> PySSizeT -> IO RawPyObject

foreign import ccall unsafe "Python.h PyTuple_Size"
  pyTuple_Size :: RawPyObject -> IO PySSizeT

foreign import ccall unsafe "Python.h PyDict_New"
  pyDict_New :: IO RawPyObject

foreign import ccall unsafe "Python.h PyDict_SetItem"
  pyDict_SetItem :: RawPyObject -> RawPyObject -> RawPyObject -> IO PyInt

foreign import ccall safe "Python.h PyObject_Call"
  pyObject_Call :: RawPyObject -> RawPyObject -> RawPyObject -> IO RawPyObject

foreign import ccall unsafe "Python.h PyRun_String"
  pyRun_String :: CString -> CInt -> RawPyObject -> RawPyObject -> IO RawPyObject

foreign import ccall unsafe "Python.h PyObject_GetAttrString"
  pyObject_GetAttrString :: RawPyObject -> CString -> IO RawPyObject

foreign import ccall unsafe "Python.h PyErr_PrintEx"
  pyErr_PrintEx :: PyInt -> IO ()

foreign import ccall unsafe "Python.h PyErr_Occurred"
  pyErr_Occurred :: IO RawPyObject

foreign import ccall unsafe "Python.h Py_InitModule4_64"
  py_InitModule :: CString -> Ptr () -> CString -> CString -> CInt -> IO RawPyObject

foreign import ccall unsafe "Python.h PyFloat_AsDouble"
  pyFloat_AsDouble :: RawPyObject -> IO CDouble

foreign import ccall unsafe "Python.h PyFloat_FromDouble"
  pyFloat_FromDouble :: CDouble -> IO RawPyObject

foreign import ccall unsafe "Python.h PyInt_AsLong"
  pyInt_AsLong :: RawPyObject -> IO CLong

foreign import ccall unsafe "Python.h PyInt_FromLong"
  pyInt_FromLong :: CLong -> IO RawPyObject

foreign import ccall unsafe "Python.h PyErr_Clear"
  pyErr_Clear :: IO ()

instance Pythonic Double where
  toPy x = pyFloat_FromDouble (realToFrac x) >>= toPyObjectChecked
  fromPy x = 
      withPyObject x $ \s -> do
        a <- pyFloat_AsDouble s
        throwCurrentPythonException
        return (realToFrac a)

instance Pythonic Int where
  toPy x = pyInt_FromLong (fromIntegral x) >>= toPyObjectChecked
  fromPy x = 
     withPyObject x $ \s -> do
       a <- pyInt_AsLong s
       if a == -1 then (throwCurrentPythonException >> return (-1 :: Int))
       else return (fromIntegral a)
 
-- by convention, insist on METH_VARARGS
-- if needed, a dictionary can be passed explicitly
data PyMethodDef = PyMethodDef String PyCFunction String | PyMethodDefEndMarker
instance Storable PyMethodDef where
  poke p PyMethodDefEndMarker = do
       poke (castPtr p) nullPtr
       poke (plusPtr p (sizeOf (undefined::CString))) nullPtr
       poke (plusPtr p (sizeOf (undefined::CString) + sizeOf nullPtr)) (0 :: CLong)
       poke (plusPtr p (sizeOf (undefined::CString) + sizeOf nullPtr + sizeOf (0::CLong))) nullPtr
  poke p (PyMethodDef a b c) = 
     withCAString a $ \ax ->
       withCAString c $ \cx -> do
         bx <- py_funcWrap b
         poke (castPtr p) ax
         poke (plusPtr p (sizeOf ax)) (bx :: FunPtr PyCFunction)
         poke (plusPtr p (sizeOf ax + sizeOf bx)) (1 :: CLong)
         poke (plusPtr p (sizeOf ax + sizeOf bx + sizeOf (1 :: CLong))) cx
  sizeOf _ = (2 * sizeOf (undefined::CString)) + sizeOf (undefined::FunPtr ()) + sizeOf (undefined :: CLong)
  alignment _ = alignment (undefined :: CInt)

foreign import ccall safe "wrapper"
  py_funcWrap :: PyCFunction -> IO (FunPtr PyCFunction)

initModule :: String -> [PyMethodDef] -> String -> IO PyObject
initModule nam md doc = 
  withCAString nam $ \namx ->
    withCAString doc $ \docx -> do
      pxx <- newArray0 PyMethodDefEndMarker md
      px <- py_InitModule namx (castPtr pxx) docx nullPtr 1013
      toPyObjectChecked px

type PyCFunction = RawPyObject -> RawPyObject -> IO RawPyObject

newtype PyObject = PyObject (ForeignPtr ())
data PythonException = PythonException deriving (Typeable,Show)

instance Exception PythonException

-- |@'toPyObject' object@ converts the raw @object@ into a managed pointer.
--
-- A managed point will automatically de-reference the object pointed to when it
-- goes out of scope.
toPyObject :: RawPyObject -> IO PyObject
toPyObject raw = liftM PyObject (newForeignPtr pyDecRef raw)

-- |Like 'toPyObject', but checks for Python exceptions when the object is
-- 'nullPtr'.
toPyObjectChecked :: RawPyObject -> IO PyObject
toPyObjectChecked obj = do
  when (obj == nullPtr) throwCurrentPythonException
  toPyObject obj

-- |@'withPyObject' object action@ runs @action@ with the unwrapped @object@.
withPyObject :: PyObject -> (RawPyObject -> IO a) -> IO a
withPyObject (PyObject ptr) = withForeignPtr ptr

-- |Throw an exception representing the current Python exception.
throwCurrentPythonException :: IO ()
throwCurrentPythonException = do
  errorOccurred <- pyErr_Occurred
  if errorOccurred == nullPtr then return ()
  else do 
    pyErr_PrintEx 0
    throwIO PythonException

-- |@'initialize' signalHandlers@ initializes the interpreter.
--
-- When @signalHandlers@ is true, install Python's signal handlers.
pyInitialize :: Bool -> IO ()
pyInitialize True = pyInitializeEx 1
pyInitialize False = pyInitializeEx 0

-- |@'importModule' name@ imports the Python module with the given 'name'.
--
-- Throw a 'PythonException' if the import failed.
importModule :: String -> IO PyObject
importModule modName =
  withCAString modName pyImport_ImportModule >>= toPyObjectChecked


-- |@'getAttr' object attribute@ gets the value of @attribute@ from @object@.
--
-- Throw a 'PythonException' if the attribute access failed.
getAttr :: PyObject -> String -> IO PyObject
getAttr obj attr = withPyObject obj $ \raw ->
  withCAString attr (pyObject_GetAttrString raw) >>= toPyObjectChecked

-- |@'callObject' object args kwargs@ calls a callable @object@ with the given
-- @args@ and @kwargs@.
--
-- Throw a 'PythonException' if the call failed.
callObject :: PyObject -> [PyObject] -> [(PyObject, PyObject)] -> IO PyObject
callObject obj args kwargs = do
  argsObj <- asTuple args
  kwargsObj <- asDict kwargs
  withPyObject argsObj $ \rawArgsObj ->
    withPyObject kwargsObj $ \rawKwargsObj ->
    withPyObject obj $ \raw ->
    pyObject_Call raw rawArgsObj rawKwargsObj >>= toPyObjectChecked

pyRunString :: String -> IO PyObject
pyRunString s = do
  withCAString s $ \ss -> do
    r <- pyRun_String ss 258 py_None py_None
    toPyObject r

asTuple :: [PyObject] -> IO PyObject
asTuple objects = do
  tuple <- pyTuple_New (fromIntegral (length objects)) >>= toPyObjectChecked
  withPyObject tuple (setItems objects 0)
  return tuple
  where
    setItems :: [PyObject] -> PySSizeT -> RawPyObject -> IO ()
    setItems [] _ _ = return ()
    setItems (x:xs) index tuple = withPyObject x $ \item -> do
      pyIncRef item             -- setItem steals the reference!
      result <- pyTuple_SetItem tuple index item
      unless (result == 0) throwCurrentPythonException
      setItems xs (index + 1) tuple

fromTuple :: PyObject -> IO [PyObject]
fromTuple t = withPyObject t $ \tt -> do
    a <- fmap fromIntegral $ pyTuple_Size tt
    ft 0 a tt
  where 
    ft :: Int -> Int -> RawPyObject -> IO [PyObject]
    ft n a z = if n >= a then return [] else do
       b <- pyTuple_GetItem z (fromIntegral n) 
       d <- toPyObject b
       c <- ft (n+1) a z
       return (d : c)

asDict :: [(PyObject, PyObject)] -> IO PyObject
asDict items = do
  dict <- pyDict_New >>= toPyObjectChecked
  withPyObject dict (addItems items)
  return dict
  where
    addItems [] _ = return ()
    addItems ((key, value):rest) dict =
      withPyObject key $ \rawKey ->
      withPyObject value $ \rawValue -> do
      result <- pyDict_SetItem dict rawKey rawValue
      unless (result == 0) throwCurrentPythonException
      addItems rest dict

class Pythonic a where
  toPy :: a -> IO PyObject
  fromPy :: PyObject -> IO a

instance Pythonic ByteString where
  toPy s = useAsCStringLen s $ \(buffer, len) ->
    pyString_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy s =
    alloca $ \s_buffer_ptr ->
    alloca $ \s_len_ptr ->
    withPyObject s $ \raw -> do
      result <- pyString_AsStringAndSize raw s_buffer_ptr s_len_ptr
      unless (result == 0) throwCurrentPythonException
      buffer <- peek s_buffer_ptr
      len <- peek s_len_ptr
      packCStringLen (buffer, fromIntegral len)

instance Pythonic String where
  toPy s = useAsCStringLen (asByteString s) $ \(buffer, len) ->
    pyUnicode_FromStringAndSize buffer (fromIntegral len) >>= toPyObjectChecked
  fromPy o = do
    s <- withPyObject o pyUnicode_AsUTF8String >>= toPyObjectChecked
    fromPy s

