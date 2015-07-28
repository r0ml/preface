{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bindings.Util (
   enumIx8
 )
 where

import Preface.Str (enumI)
import Language.Haskell.TH.Quote (QuasiQuoter)

-- | This function multiplies the enum value by 8
--  it is used only for syslog 
enumIx8 :: QuasiQuoter
enumIx8 = enumI ((8*) . read)

