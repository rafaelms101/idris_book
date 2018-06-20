--1

data Elem : a -> List a -> Type where
  Here : Elem x (x :: xs)
  There : (later : Elem x xs) -> Elem x (y :: xs)

--2
data Last : List a -> a -> Type where
  LastOne : Last [value] value
  LastCons : (prf : Last xs value) -> Last (x :: xs) value

lastNotNil : Last [] value -> Void
lastNotNil LastOne impossible
lastNotNil (LastCons _) impossible

test : (Last xs value -> Void) -> Last (x :: xs) value -> Void
test contra a = ?test_rhs


isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No lastNotNil
isLast (x :: xs) value =
  case isLast xs value of
    Yes prof => Yes (LastCons prof)
    No contra => ?b

--contra : Last xs value -> Void
--Last (x :: xs) value -> Void
