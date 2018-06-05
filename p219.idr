--1
same_cons : {xs : List a} -> {ys : List a} -> xs = ys -> (::) x xs = (::) x ys
same_cons Refl = Refl

--Refl1 :
-- Refl2 :

--2
same_lists : {xs : List a} -> {ys : List a} ->
             x = y -> xs = ys -> (::) x xs = (::) y ys
same_lists Refl Refl = Refl

-- Refl1 :
-- Refl2 :
-- Refl3 :  


--3
data ThreeEq : a -> b -> c -> Type where
  AllSame : ThreeEq x x x

--4
allSameS : (x, y, z : Nat) -> ThreeEq x y z -> ThreeEq (S x) (S y) (S z)
allSameS z z z AllSame = AllSame
