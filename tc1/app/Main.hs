module Main where

import qualified CLI 

main :: IO ()
main = do 
  putStrLn "tc1 https://github.com/dmarcia/tt"
  CLI.run
