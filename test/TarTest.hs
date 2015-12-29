{-# LANGUAGE OverloadedStrings #-}

import Preface

main :: IO ()
main = do
  args <- getArgs

  mapM_ putStrLn =<< (sort <$> (tarList "test/data/cabal.tar"))


