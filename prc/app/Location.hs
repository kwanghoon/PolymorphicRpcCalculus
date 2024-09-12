module Location where


data Location =
    Location String
  | LocVar String
    deriving (Show, Eq)