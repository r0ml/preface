

--------------------------------------------------------------------------------
import Preface.R0ml

--------------------------------------------------------------------------------
main :: IO ()
main = do
--     wsConnectTo "echo.websocket.org" 80 "/" app
     wsConnectTo "localhost" 9090 "/" app
  
app :: WebSocket -> IO ()  
app x = do

    putStrLn "Connected!"

    -- Fork a thread that writes WS data to stdout
    _ <- forkIO $ forever $ do
        print "wait for it"
        msg <- readChan (wsRecv x) --  :: IO ByteString 
        print ("how come?" ++ show msg)

    print "Done"
    -- Read from stdin and write to WS

    let loop = do
            line <- getLine
            traceIO ("got "++line)
            unless (null line) $ sendMessage x (Text line) >> loop

    loop
    sendMessage x (Text "Bye!")
    sendMessage x Close

--------------------------------------------------------------------------------
