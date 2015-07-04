
module Preface.StrUtils 

where

shell_escape :: String -> String
shell_escape = concat . ("'" : ) . (:["'"]) . concatMap esq where esq c = if c == '\'' then "'\"'\"'" else [c]

