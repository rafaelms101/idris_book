data Expr num = Val num
  | Add (Expr num) (Expr num)
  | Sub (Expr num) (Expr num)
  | Mul (Expr num) (Expr num)
  | Div (Expr num) (Expr num)
  | Abs (Expr num)

Num ty => Num (Expr ty) where
    (+) = Add
    (*) = Mul
    fromInteger = Val . fromInteger

Neg ty => Neg (Expr ty) where
  negate x = 0 - x
  (-) = Sub

eval : (Neg num, Integral num, Abs num) => Expr num -> num
eval (Val x) = x
eval (Add x y) = eval x + eval y
eval (Sub x y) = eval x - eval y
eval (Mul x y) = eval x * eval y
eval (Div x y) = eval x `div` eval y
eval (Abs x) = abs (eval x)


--1
paren : String -> String
paren str = "(" ++ str ++ ")"

Show num => Show (Expr num) where
  show (Val x) = show x
  show (Add x y) = paren (show x ++ " + " ++ show y)
  show (Sub x y) = paren (show x ++ " - " ++ show y)
  show (Mul x y) = paren (show x ++ " * " ++ show y)
  show (Div x y) = paren (show x ++ " / " ++ show y)
  show (Abs x) = "|" ++ show x ++ "|"

--2
(Neg ty, Integral ty, Eq ty, Abs ty) => Eq (Expr ty) where
  (==) x y = eval x == eval y

--3
(Neg num, Integral num, Abs num) => Cast (Expr num) num where
  cast orig = eval orig
