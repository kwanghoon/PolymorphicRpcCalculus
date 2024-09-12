module Interp where

import Expr
import Env
import Location

valueOf :: Expr -> Env -> Location -> ExpVal
valueOf (Var x) env atloc = lookupEnv x env

valueOf (ConstInt n) env atloc = NumVal n
valueOf (ConstBool b) env atloc = BoolVal b

valueOf (Abs loc x e) env atloc = FunVal (ValProc loc x e env)
valueOf (App e1 e2) env atLoc = 
    let v1 = valueOf e1 env atLoc
        v2 = valueOf e2 env atLoc

        p = expvalProc v1
    in applyProcedure p v2 atLoc

-- Todo : valueOf for TypeAbs, TypeApp, LocAbs, LocApp


applyProcedure :: Proc -> ExpVal -> Location -> ExpVal
applyProcedure (ValProc loc arg body env) argval atLoc =
    valueOf body (ExtendEnv arg argval env) loc
applyProcedure (TypeProc loc arg body env) argval atLoc =
    error ("type nono")
applyProcedure (LocProc loc arg body env) argval atLoc =
    error ("Location nono")

-- Todo : applyTypeProcedure, applyLoc Procedure