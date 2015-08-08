{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
{-# LANGUAGE QuasiQuotes #-}

import WebSocket
import Preface.R0ml

main :: IO ()
main = do
  args <- getArgs
  let port = (read . head) args :: Int
  putStrLn ("Starting on port " ++ show port)
  y <- runServer port login (`seq` return ()) receiver
  m <- newChan
  _ <- forkOS $ finally (forever $ sender y) (writeChan m ("Done"::String) )
  _ <- readChan m
  exitSuccess

-- |this function gets called in a loop
--  it will send anything typed on stdin to every connected websocket
--  the argument is the an IORef containing the list of every connected websocket
--  it needs to be an IORef because it wants to get the list after each line to stdin -- and the list
--  might have changed while I was waiting for stdin
sender :: IORef [WebSocket a] -> IO ()
sender wss = do
  line <- catch getLine (\(e::SomeException) -> fail (show e))
  if null line then exitSuccess else do
    rf <- readIORef wss
    print ("the ioref length is "++ show (length rf))
    mapM_ (flip sendMessage (Text line)) rf
    
receiver :: WebSocketMessage -> WebSocket Int -> IO ()
receiver x ws = do
  let st = wsState ws -- the IORef containing my session info ( database connection in this case)
      msg = asText $ case x of 
             WebSocketTextMessage z -> z
             WebSocketBinaryMessage z -> asString z
             _ -> ""
    
  traceShow ("msg", msg) $ if ("login:" == strTake 6 msg) then do
    print (strConcat ["attempting connection " , msg])
    -- a <- PG.connectToDb (strDrop (6::Integer) msg)
    writeIORef st (Just 7)
    -- modifyIORef' st (\m -> mapInsert ws a m)
    print ("connection attempted"::Text)
  else do
--    cc <- readIORef cns
--    let abc = mapLookup ws cc
    abc <- readIORef st
    if abc == Nothing then do
       print "not connected to db"
       sendMessage ws (Text "error: not connected to db")
    else let Just z = abc in do
       b <- traceShow msg [sh|$msg|]
       sendMessage ws (Text $ show b)

-- Much as I would like to log in to the database when the connection is established,
-- I can't if I have to wait for the userid/password
-- Alternatively, I can rely on a cookie being set -- so the login is based on the cookie
login :: HTTPHeaders -> WebSocket a -> IO ()
login hs ws = do
  -- 1) connect to the database (using the sessioner id) and retrieve the credentials
  -- 2) create a connection to the database using the credentials retrieved in step 1)
  print hs
  sendMessage ws (Text "login:")
