module Main where

import System.Environment (getArgs)

import Test.DocTest
  ( mainFromCabal
  )

main :: IO ()
main = do
  mainFromCabal
    "effable"
    =<< getArgs
