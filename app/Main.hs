module Main where

import Interpreter
import System.Environment
import Chi
import Control.Monad

main :: IO ()
main = getArgs >>= (`forM_` (act >=> putStrLn))
  where
    act :: FilePath -> IO String
    act f = pretty . interpret . parse <$> readFile f
