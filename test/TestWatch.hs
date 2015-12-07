
import Preface

dumpChan a = do
  b <- readChan a
  print b
  dumpChan a

main = do
  a <- fsEventStreamCreate ["/"] 0.1 [FSEventStreamCreateFlagFileEvents] -- a is the Channel where events will be written
  dumpChan a
 
