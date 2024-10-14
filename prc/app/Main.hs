module Main (main) where

import Example
import Interp(valueOf)
import InterpK(valueOfProgramK)
import Env

main :: IO ()
main = do let v = valueOfProgramK ex1_2 client2
          print ex1_2
          putStrLn ""
          print v


          let w = valueOf ex1_2 EmptyEnv client2
          print ex1_2
          putStrLn ""
          print w
