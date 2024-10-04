module Interp where

import Expr
import Env
import Location
import Type

valueOf :: Expr -> Env -> Location -> ExpVal
valueOf (Var x) env atloc = lookupEnv x env

valueOf (ConstInt n) env atloc = NumVal n
valueOf (ConstBool b) env atloc = BoolVal b

valueOf (Abs loc x e) env atloc = FunVal (ValProc loc x e env)
valueOf (App e1 e2) env atLoc = 
    let v1 = valueOf e1 env atLoc
        v2 = valueOf e2 env atLoc

        p = expValProc v1
    in applyProcedure p v2 atLoc

valueOf (TypeAbs x e) env atLoc = FunVal (TypeProc atLoc x e env)
valueOf (TypeApp e t) env atLoc = 
    let v = valueOf e env atLoc
        p = expValProc v
    in applyTypeProcedure p t atLoc

valueOf (LocAbs x e) env atLoc = FunVal (LocProc atLoc x e env)
valueOf (LocApp e l) env atLoc = 
    let v = valueOf e env atLoc
        p = expValProc v
    in applyLocProcedure p l atLoc



applyProcedure :: Proc -> ExpVal -> Location -> ExpVal
applyProcedure (ValProc loc arg body env) argval atLoc =
    valueOf body (ExtendEnv arg argval env) loc
applyProcedure _ _ _ = error "Expected procedure"

applyTypeProcedure :: Proc -> Type -> Location -> ExpVal
applyTypeProcedure (TypeProc loc arg body env) argval atLoc =
    valueOf body (ExtendTypeEnv arg argval env) loc
applyTypeProcedure _ _ _ =  error "Expected type procedure"

applyLocProcedure :: Proc -> Location -> Location -> ExpVal
applyLocProcedure (LocProc loc arg body env) argval atLoc =
    valueOf body (ExtendLocEnv arg argval env) loc
applyLocProcedure _ _ _ = error "Expected location procedure"