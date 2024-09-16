module Type where

import Location


data Type = 
    TypeVar String
  | ConType String -- constant type
  | FunType Type Location Type  -- A -Location - B
  | TypeAbsType String Type -- forall a : type. A
  | LocAbsType String Type -- forall l : location.Loc
  
  deriving (Show, Eq)