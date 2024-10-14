module InterpK where

import Expr
import Env
import Location
import Type
-- import Language.Haskell.TH (Loc)

-- Continuation

data Cont = 
    RatorCont Expr Env Cont  --  App [  ]  Expr 
  | RandCont ExpVal Cont --  App Val [  ]
  | TypeRatorCont Type Cont -- TypeApp [  ] Type
  | LocRatorCont Location Cont -- LocApp [  ] Location
  | EndCont

--
applyCont :: Cont -> ExpVal -> Location -> ExpVal
applyCont (RatorCont expr env cont) v atLoc = 
    valueOfK expr env atLoc (RandCont v cont)

applyCont (RandCont clo cont) v atLoc = 
    let p = expValProc clo in
      applyProcedureK p v atLoc cont

applyCont (TypeRatorCont t cont) clo atLoc =
    let p = expValProc clo in 
        applyTypeProcedureK p t atLoc cont

applyCont (LocRatorCont l cont) clo atLoc =
    let p = expValProc clo in 
        applyLocProcedureK p l atLoc cont        

applyCont EndCont v atloc = v


--
valueOfK :: Expr -> Env -> Location -> Cont -> ExpVal
valueOfK (Var x) env atloc cont = applyCont cont (lookupEnv x env) atloc

valueOfK (ConstInt n) env atloc cont = applyCont cont (NumVal n) atloc

valueOfK (ConstBool b) env atloc cont = applyCont cont (BoolVal b) atloc

valueOfK (Abs loc x e) env atloc cont = 
    applyCont cont (FunVal (ValProc loc x e env)) atloc

valueOfK (App e1 e2) env atLoc cont = 
    valueOfK e1 env atLoc (RatorCont e2 env cont)

valueOfK (TypeAbs x e) env atLoc cont = 
    applyCont cont (FunVal (TypeProc x e env)) atLoc

valueOfK (TypeApp e t) env atLoc cont = 
    valueOfK e env atLoc (TypeRatorCont t cont)

valueOfK (LocAbs x e) env atLoc cont = 
    applyCont cont (FunVal (LocProc x e env)) atLoc

valueOfK (LocApp e l) env atLoc cont = 
    valueOfK e env atLoc (LocRatorCont l cont)

valueOfProgramK :: Expr -> Location -> ExpVal
valueOfProgramK e atLoc = valueOfK e EmptyEnv atLoc EndCont

--
applyProcedureK :: Proc -> ExpVal -> Location -> Cont -> ExpVal
applyProcedureK (ValProc loc arg body env) argval atLoc cont=
    valueOfK body (ExtendEnv arg argval env) loc cont
applyProcedureK _ _ _ _ = error "Expected procedure"

applyTypeProcedureK :: Proc -> Type -> Location -> Cont -> ExpVal
applyTypeProcedureK (TypeProc arg body env) argty atLoc cont =
    valueOfK body (ExtendTypeEnv arg argty env) atLoc cont
applyTypeProcedureK _ _ _ _ =  error "Expected type procedure"

applyLocProcedureK :: Proc -> Location -> Location -> Cont -> ExpVal
applyLocProcedureK (LocProc arg body env) argloc atLoc cont =
    valueOfK body (ExtendLocEnv arg argloc env) atLoc cont
applyLocProcedureK _ _ _ _ = error "Expected location procedure"