module Main where

import Interpreter
import System.Environment

main :: IO ()
main = getArgs >>= mapM_ act
  where
    act = pretty . interpret . parse <$> readFile
