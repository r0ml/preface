module Preface.Typing (
  autotype, autotypeLn, autotypeDiff
) where

import Preface.Imports
import Preface.Diff
import Preface.Console
import Preface.Stringy

autotype :: String -> IO ()
autotype ss = if null ss then return () else do 
   putChar (head ss)
   threadDelay 60000
   autotype (tail ss)

autotypeLn :: String -> IO ()
autotypeLn = (>> putStrLn "") . (>> threadDelay 500000) . autotype

autotypeDiff :: String -> String -> IO ()
autotypeDiff x y = let dfo = diff x y in do
  putStr x
  strPut $ strConcat (take (length x) (cycle [consoleBackwardChar]))
  -- putStr (asString (setColumn 0)) -- only if starting at left
  threadDelay 250000
  mapM_ autotypeDiffx dfo
  threadDelay 500000
  putStrLn ""
  where autotypeDiffx :: DiffOperation Char -> IO ()
        autotypeDiffx (Deletion _n a _m) = sequence_ (take (length a) (repeat (strPut consoleDeleteChar >> threadDelay 60000)))
        autotypeDiffx (Addition _n a _m) = mapM_ ((>>threadDelay 60000) . strPut . consoleInsertChar . asByte ) a
        autotypeDiffx (Change _n _a _m _b) = undefined
        autotypeDiffx (Unchanged _n a _m _b) = sequence_ (take (length a) (repeat (strPut consoleForwardChar >> threadDelay 60000)))

