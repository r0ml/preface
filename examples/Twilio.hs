
{-
module API.Twilio (

) where
-}

import Preface

twilio x = let a = maybe "TWILIO_SID not set" id $ unsafePerformIO $ lookupEnv "TWILIO_SID"
               d = strUntilStr "%%" x
            in case d of 
                  Nothing -> undefined
                  Just (b,c) -> ("https://api.twilio.com/2010-04-01/" ++) b ++ a ++ (drop 1 c)

main = do
  (a:b:c:_) <- getArgs
  withCurlDo $ \x -> do
    let opts = [curloptSetUsername $ maybe "TWILIO_SID not set" id $ unsafePerformIO $ lookupEnv "TWILIO_SID"
               ,curloptSetPassword $ maybe "TWILIO_AuthToken not set" id $ unsafePerformIO $ lookupEnv "TWILIO_AUTHTOKEN"]
    
    cd <- curlPost (twilio "/Accounts/%%/Messages") opts ["To="++a, "From=+13474275355","Body="++b] x
    traceShow cd (return ())
    return cd

