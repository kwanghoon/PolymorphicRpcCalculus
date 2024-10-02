module Env where

import Expr
import Type
import Location


data ExpVal = 
    NumVal {expValInt :: Int}
  | BoolVal {expValBool :: Bool}
  | FunVal {expValProc :: Proc}

  deriving (Show, Eq)

-- type TypeVal = Type
-- type LocationVal = Location


-- Closure
data Proc = 
    ValProc Location String Expr Env
  | TypeProc String Expr Env
  | LocProc String Expr Env
  
  deriving (Show, Eq)

data Env = 
    EmptyEnv
  | ExtendEnv String ExpVal Env
  | ExtendTypeEnv String Type Env
  | ExtendLocEnv String Location Env
  
  deriving (Show, Eq)


lookupEnv :: String -> Env -> ExpVal
lookupEnv x EmptyEnv = error ("Not found: " ++ x)
lookupEnv x (ExtendEnv y v env) = 
    if x == y then v else lookupEnv x env
lookupEnv x (ExtendTypeEnv _ _ env) = lookupEnv x env
lookupEnv x (ExtendLocEnv _ _ env) = lookupEnv x env


lookupTypeEnv :: String -> Env -> Type
lookupTypeEnv x EmptyEnv = error ("Not found: " ++ x)
lookupTypeEnv x (ExtendEnv _ _ env) = lookupTypeEnv x env
lookupTypeEnv x (ExtendTypeEnv y v env) = 
    if x == y then v else lookupTypeEnv x env
lookupTypeEnv x (ExtendLocEnv _ _ env) = lookupTypeEnv x env


lookupLocEnv :: String -> Env -> Location
lookupLocEnv x EmptyEnv = error ("Not found: " ++ x)
lookupLocEnv x (ExtendEnv _ _ env) = lookupLocEnv x env
lookupLocEnv x (ExtendTypeEnv _ _ env) = lookupLocEnv x env
lookupLocEnv x (ExtendLocEnv y v env) = 
    if x == y then v else lookupLocEnv x env
