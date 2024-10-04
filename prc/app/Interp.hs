module Interp where

import Expr
import Env
import Location
import Type

-- Continuation

data Cont = 
    EndCont

applyCont :: Cont -> ExpVal -> ExpVal
applyCont EndCont v = v


valueOf :: Expr -> Env -> Location -> ExpVal
valueOf (Var x) env atloc = lookupEnv x env

valueOf (ConstInt n) env atloc = NumVal n
valueOf (ConstBool b) env atloc = BoolVal b

valueOf (Abs loc x e) env atloc = FunVal (ValProc loc x e env)
valueOf (App e1 e2) env atLoc = 
    let v1 = valueOf e1 env atLoc
        v2 = valueOf e2 env atLoc

        p = expValProc v1
    in applyProcedureK p v2 atLoc

valueOf (TypeAbs x e) env atLoc = FunVal (TypeProc x e env)
valueOf (TypeApp e t) env atLoc = 
    let v = valueOf e env atLoc
        p = expValProc v
    in applyTypeProcedureK p t atLoc

valueOf (LocAbs x e) env atLoc = FunVal (LocProc x e env)
valueOf (LocApp e l) env atLoc = 
    let v = valueOf e env atLoc
        p = expValProc v
    in applyLocProcedureK p l atLoc



applyProcedureK :: Proc -> ExpVal -> Location -> ExpVal
applyProcedureK (ValProc loc arg body env) argval atLoc =
    valueOf body (ExtendEnv arg argval env) loc
applyProcedureK _ _ _ = error "Expected procedure"

applyTypeProcedureK :: Proc -> Type -> Location -> ExpVal
applyTypeProcedureK (TypeProc arg body env) argty atLoc =
    valueOf body (ExtendTypeEnv arg argty env) atLoc
applyTypeProcedureK _ _ _ =  error "Expected type procedure"

applyLocProcedureK :: Proc -> Location -> Location -> ExpVal
applyLocProcedureK (LocProc arg body env) argloc atLoc =
    valueOf body (ExtendLocEnv arg argloc env) atLoc
applyLocProcedureK _ _ _ = error "Expected location procedure"