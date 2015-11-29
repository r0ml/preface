
module Httpd
    ( Request, Response, Server
    , mkResponse
    , reqMethod, reqURI, reqHeaders, reqBody
    -- , shed
    )
    where

import Preface.R0ml

import Control.Applicative
import Control.Arrow ( (***) )
import Control.Monad
import Control.Monad.Trans ( liftIO )
import qualified Data.ByteString            as B
import qualified Data.ByteString.Lazy       as BL
import qualified Data.ByteString.Char8      as BC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.Maybe ( fromJust )
import Network.URI ( URI, parseRelativeReference )

{-
import Network.Socket
    ( getAddrInfo, AddrInfo, defaultHints, addrAddress, addrFamily
      , addrFlags, addrSocketType, AddrInfoFlag(AI_PASSIVE), socket, Family(AF_UNSPEC,AF_INET6)
      , defaultProtocol, SocketType(Stream), listen, setSocketOption, SocketOption(ReuseAddr)
    )
import Network.Socket ( bindSocket, Socket, SockAddr )
-}
{-
import qualified Network.Shed.Httpd as Shed
    ( Request, Response(Response), initServer
    , reqMethod, reqURI, reqHeaders, reqBody
    )
-}
data Request = Request
    {
     reqMethod :: String,
     reqURI :: URI,
     reqHeaders :: [(String, String)],
     reqBody :: String
    }

data Response = Response
    {
     respStatus :: Int,
     respHeaders :: [(String, String)],
     respBody :: String
    }

mkResponse :: Int -> [(String, String)] -> String -> Response
mkResponse = Response

type Server = Int -> (Request -> IO Response) -> IO ()

{-
shed :: Server
shed port handler =
    () <$ Shed.initServer
           port
           (liftM responseToShed . handler . requestFromShed)
  where
     responseToShed (Response status hdrs body) =
         Shed.Response status hdrs body
     chomp = reverse . strip '\r' . reverse
     strip c (c':str) | c == c' = str
     strip c str = str
     requestFromShed request =
         Request
         {
          reqMethod = Shed.reqMethod request,
          reqURI = Shed.reqURI request,
          reqHeaders = map (id *** chomp) $ Shed.reqHeaders request,
          reqBody = Shed.reqBody request
         }
-}
