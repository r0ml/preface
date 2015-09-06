
module Preface.StrUtils (
  shell_escape
) where

import Preface.Imports

shell_escape :: String -> String
shell_escape = concat . ("'" : ) . (:["'"]) . concatMap esq where esq c = if c == '\'' then "'\"'\"'" else [c]

