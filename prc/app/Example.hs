module Example where

import Location
import Type
import Expr


client1, client2, client3, server1, server2 :: Location
client1 = Location "client1"
client2 = Location "client2"
client3 = Location "client3"

server1 = Location "server1"
server2 = Location "server2"


ex1, ex2, ex3 :: Expr
ex1 = Abs client1 "x" (Var "x") -- int -client1 -> int (x가 int면)
ex2 = Abs client2 "x" (Var "x") 
ex3 = Abs client3 "x" (Var "x")

ex1_1 =  App ex1 (ConstInt 1) -- ex1_1 :: int



intty, boolty :: Type
intty = ConType "int"
boolty = ConType "bool"

ty1 = FunType intty client1 intty -- ex1 :: ty1
ty2 = FunType intty client2 intty -- ex2 :: ty2
ty3 = FunType intty client3 intty -- ex3 :: ty3

ex4 :: Expr
ex4 = LocAbs "l" (Abs (LocVar "l") "x" (Var "x")) -- forall l:Loc, int -l -> int l은 polymorphic한 무언가.. Generic함수 생각해보자

ex5 = LocApp ex4 client1  -- ex5 :: ty1
ex6 = LocApp ex4 client2  -- ex6 :: ty2
ex7 = LocApp ex4 client3  -- ex7 :: ty3


-- ex8 :: forall a:Type. forall l:Loc, a-l -> a
ex8 = TypeAbs "a"
        (LocAbs "l"
            (Abs(LocVar "l") "x" (Var "x")))

ex9 = TypeApp ex8 intty -- ex9 :: forall l:loc. int -l-> int

ex10 = LocApp ex9 client1 -- ex10 :: int -client1 -> int

ex11 = LocAbs "l"
        (TypeAbs "a"
            (Abs(LocVar "l") "x" (Var "x")))

ex12 = TypeApp (LocApp ex11 client1) intty

ex13 = LocApp (TypeApp ex8 intty) client1