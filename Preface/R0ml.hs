{- | This is r0ml's extended preface for GHC.
-}

module Preface.R0ml (module X
  -- | A class which implements String functions for various representations (UTF8, ByteString, and String)
  ,Stringy(..)
  -- | A class which implements Char functions for Word8, Char8 and Char
  , Chary(..)
 ) where

import Preface.Imports as X
import Preface.Stringy as X hiding()
import Preface.Byter as X hiding ()

import Preface.Binary as X
import Preface.Arrayed as X
import Debug.Trace as X

import Preface.Misc as X
import Preface.Math as X

import Preface.Runner as X

import Preface.QuasiQuotes as X
import Preface.FFITemplates as X
import Preface.FFITemplates2 as X
import Preface.FFITemplates3 as X
import Preface.IOQuotes as X

import Preface.StrUtils as X
import Preface.Symbols as X
import Preface.SecureHash as X

import Preface.Timings as X

import Preface.Xml as X
import Preface.JSONic as X

import Preface.IO as X

import Preface.Typing as X
import Preface.Diff as X
import Preface.Console as X

import Preface.ASN1 as X


import Bindings.Curl as X
import Bindings.Posix as X

import Bindings.Zlib as X

import Preface.Pipes as X

import Preface.SCGI as X
import Preface.WebSocket as X

import Preface.Testing as X

