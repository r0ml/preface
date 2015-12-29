
import Preface

main = do
  a <- runWithTimeout 10 "/bin/ls" [] ""
  print a
  b <- runWithTimeout 2 "/bin/sleep" ["3"] ""
  print b

