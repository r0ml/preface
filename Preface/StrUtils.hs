{-# LANGUAGE FlexibleInstances, UndecidableInstances #-}

module Preface.StrUtils (
    shell_escape
  , NSShow(..)
  , deComma
  , stripComments
) where

import Preface.Imports
import qualified Data.ByteString.Char8 as BC
import qualified Data.Text as T

shell_escape :: String -> String
shell_escape = concat . ("'" : ) . (:["'"]) . concatMap esq where esq c = if c == '\'' then "'\"'\"'" else [c]

-- | For many interpolations, we want to use Strings as-is, but convert non-Strings via Show.  We do not, however,
-- want to use Show on Strings.  The class NSShow (and the function nsShow) do that.
-- This business requires OverlappingInstances and the order of the definitions matters
class Show a => NSShow a where
  nsShow :: NSShow a => a -> String
  nsShow = show

instance {-# OVERLAPPABLE #-} Show a => NSShow a where nsShow = show
instance {-# OVERLAPPING #-} NSShow String where nsShow x = x
instance NSShow ByteString where nsShow = BC.unpack
instance NSShow Text where nsShow = T.unpack


stripComments :: String -> String
stripComments = stripComments' True where
  stripComments' _ [] = []
  stripComments' True s = if take 2 s == "/*" then ' ' : stripComments' False (drop 2 s)
                                             else head s : stripComments' True (tail s)
  stripComments' False s = if take 2 s == "*/" then stripComments' True (drop 2 s)
                                              else stripComments' False (tail s)

deComma :: String -> String
deComma = map (\x -> if x == ',' then ' ' else x) 


