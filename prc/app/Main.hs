module Main (main) where

import Example
import Interp
import Env
import Expr

main :: IO ()
-- main = do let v = valueOf ex1 EmptyEnv client2
--           putStrLn (show ex1)
--           putStrLn ""
--           putStrLn (show v)


main = do
    -- 평가할 표현식 목록
    let expressions = 
            [ ("ex1", ex1)
            , ("ex2", ex2)
            , ("ex3", ex3)
            , ("ex1_1", ex1_1)
            , ("ex4", ex4)
            , ("ex5", ex5)
            , ("ex6", ex6)
            , ("ex7", ex7)
            , ("ex8", ex8)
            , ("ex9", ex9)
            , ("ex10", ex10)
            , ("ex11", ex11)
            , ("ex12", ex12)
            , ("ex13", ex13)
            ]

    -- 평가하고 결과를 출력
    mapM_ printResult expressions

printResult :: (String, Expr) -> IO ()
printResult (name, expr) = do
    let result = valueOf expr EmptyEnv client2
    putStrLn $ "Evaluating: " ++ name
    putStrLn $ "Expression: " ++ show expr
    putStrLn $ "Result: " ++ show result
    putStrLn ""
