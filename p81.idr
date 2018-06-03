import Data.Vect

-- 1
createEmpties : Vect n (Vect 0 elem)
createEmpties = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = createEmpties
transposeMat (x :: xs) =
  let xsTrans = transposeMat xs in
    zipWith (\x, y => x :: y) x xsTrans

--2
addVector : Num a => Vect n a -> Vect n a -> Vect n a
addVector [] [] = []
addVector (x :: xs) (y :: ys) = (x + y) :: addVector xs ys

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) ->
                     Vect n (Vect m a)
addMatrix [] [] = []
addMatrix (x :: xs) (y :: ys) = addVector x y :: addMatrix xs ys

--3
multLineLine : Num a => Vect q a -> Vect q a -> a
multLineLine [] [] = 0
multLineLine (y :: xs) (x :: ys) = x * y + multLineLine xs ys

multLineMat : Num a => Vect q a -> Vect s (Vect q a) -> Vect s a
multLineMat v [] = []
multLineMat v (x :: xs) = multLineLine v x :: multLineMat v xs

multMatrixAux : Num a => Vect p (Vect q a) ->
                         Vect s (Vect q a) ->
                         Vect p (Vect s a)
multMatrixAux [] ys = []
multMatrixAux (x :: xs) ys =
  multLineMat x ys :: multMatrixAux xs ys


multMatrix : Num a => Vect p (Vect q a) -> Vect q (Vect s a) ->
                      Vect p (Vect s a)
multMatrix m1 m2 =
  let tm2 = transposeMat m2 in
  multMatrixAux m1 tm2
