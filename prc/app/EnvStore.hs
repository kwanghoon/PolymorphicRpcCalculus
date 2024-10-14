module EnvStore where

import Location
import Expr
import Type
import Queue

data ExpVal = 
    NumVal {expValInt :: Int}
  | BoolVal {expValBool :: Bool}
  | FunVal {expValProc :: Proc}
  | ListVal {expValList :: [ExpVal]}
  | MutexVal {expValMutex :: Mutex}
  | QueueVal {expValQueue :: Queue Thread}

instance Show ExpVal where
  show (NumVal n) = show n
  show (BoolVal b) = show b
  show (FunVal p) = show p
  show (ListVal l) = show l
  show (MutexVal m) = show m
  show (QueueVal _) = "queue"

instance Eq ExpVal where
  NumVal n1 == NumVal n2     = n1 == n2
  BoolVal b1 == BoolVal b2   = b1 == b2
  FunVal p1 == FunVal p2     = p1 == p2
  ListVal l1 == ListVal l2   = l1 == l2
  MutexVal m1 == MutexVal m2 = m1 == m2
  QueueVal _ == QueueVal _   = True
  _ == _                     = False

-- Reference Addresses
type Address = Integer


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

-- Mutex values : boolean and thread queue
data Mutex = Mutex Address Address -- binary semaphores: Loc to Bool, Loc to (Queue Thread)
             deriving (Show, Eq)

-- Scheduler states
data SchedState =
  SchedState {
   theReadyQueue :: Queue Thread,
   theFinalAnswer :: Maybe ExpVal,
   theMaxTimeSlice :: Integer,
   theTimeRemaining :: Integer
  }

-- Threads
type Thread = Store -> SchedState -> (ExpVal, Store)

--
type Store = (Address, [(Address,ExpVal)]) -- Next new reference address

newref :: Store -> ExpVal -> (Address,Store)
newref store@(next,s) v = (next,(next+1,(next,v):s))

deref :: Store -> Address -> ExpVal
deref store@(next,s) loc =
  case [v | (loc',v) <- s, loc==loc'] of
    (v:_) -> v
    _     -> error ("Reference address not found: " ++ show loc)

setref :: Store -> Address -> ExpVal -> Store
setref store@(next,s) loc v = (next,update s)
  where update [] = error ("Invalid reference address: " ++ show loc)
        update ((loc',w):s')
          | loc==loc' = (loc,v):s'
          | otherwise = (loc',w):update s'

initStore :: Store
initStore = (1,[])