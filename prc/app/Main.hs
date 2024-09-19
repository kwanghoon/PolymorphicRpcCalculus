module Main (main) where

import Example
import Interp
import Env

main :: IO ()
main = do let v = valueOf ex1 EmptyEnv client2
          putStrLn (show ex1)
          putStrLn ""
          putStrLn (show v)
