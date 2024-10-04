module Expr where

import Location
import Type


data Expr = 
    Var String                      -- variable
  | ConstInt Int
  | ConstBool Bool

  | App Expr Expr                   -- function application L M
  | Abs Location String Expr        -- located Lamda abstraction

  | TypeApp Expr Type               -- type application M [A]
  | TypeAbs String Expr             -- Type abstraction

  | LocApp Expr Location            -- location application M [Loc]
  | LocAbs String Expr              -- location abstraction

  deriving (Show, Eq)

isValue :: Expr -> Bool
isValue (Var _) = True
isValue (Abs _ _ _) = True
isValue (TypeAbs _ _) = True
isValue (LocAbs _ _) = True
isValue _ = False
