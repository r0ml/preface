
import Preface.R0ml

main :: IO ()
main = do 
     putStrLn $ "Hash is:   " ++ stringDigest (md5 strEmpty )
     putStrLn $ "Should Be: d41d8cd98f00b204e9800998ecf8427e" 
