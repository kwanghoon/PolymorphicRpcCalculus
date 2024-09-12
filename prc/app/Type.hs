module Type where

import Location


data Type = 
    TypeVar String
  | FunType Type Location Type  -- A -Location - B
  | TypeAbsType String Type -- forall a : type. A
  | LocAbsType String Type -- forall l : location.Loc
  | ConType String -- constant type
    deriving (Show, Eq)