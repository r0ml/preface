
import MD5
import qualified Data.ByteString as B (empty)

test :: IO ()
test = do 
     putStrLn $ "Hash is:   " ++ show (stringMD5 . md5 $ B.empty )
     putStrLn $ "Should Be: d41d8cd98f00b204e9800998ecf8427e" 
