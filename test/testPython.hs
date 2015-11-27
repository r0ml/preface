
import Preface.R0ml

square :: RawPyObject -> RawPyObject -> IO RawPyObject 
square _i x = do
   cd <- toPyObject x
   ren <- fromTuple cd
   z <- fromPy (head ren) :: IO Double
   let a = z * z
   PyObject p <- toPy (a :: Double)
   withForeignPtr p $ \q -> return q

main = do
  initialize False
  pygments <- importModule "os"
  py_statvfs <- getAttr pygments "statvfs"
  pathObj <- toPy "/"
  builtin <- importModule "__builtin__"
  str <- getAttr builtin "str"
  z <- callObject py_statvfs [pathObj] []
  y <- callObject str [z] []
  x <- fromPy y :: IO ByteString
  print x
 
  m <- initModule "foo" [PyMethodDef "sqqq" square "This is the doc"] "More doc"
  eval <- getAttr builtin "eval"
  mm <- importModule "__main__"
  gg <- getAttr m "__dict__"

  ss <- toPy "str(sqqq(float(19)))"
  -- ss <- toPy "str(numargs())"
  pn <- toPyObject py_None
  f <- callObject eval [ss,gg,gg] []
  h <- fromPy f :: IO ByteString
  print h

