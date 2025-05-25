module Main where

import qualified MyLib (parseAndPrint)

main :: IO ()
main = do
  MyLib.parseAndPrint
