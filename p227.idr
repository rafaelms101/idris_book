import Data.Vect

--1
reverseEq : x = y -> y = x
reverseEq Refl = Refl

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m =
  let prof = plusZeroRightNeutral m
  in reverseEq prof

myPlusCommutes (S k) m =
  rewrite myPlusCommutes k m in
  plusSuccRightSucc m k



--2
proof_nil : Vect n a -> Vect (plus n 0) a
proof_nil {n} xs = rewrite plusZeroRightNeutral n in xs

proof_xs : Vect (S (plus n len)) a -> Vect (plus n (S len)) a
proof_xs {n} {len} xs = rewrite sym (plusSuccRightSucc n len) in xs


myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n + m) a
        reverse' acc [] = proof_nil acc
        reverse' acc (x :: xs) = ?proof_xs (reverse' (x :: acc) xs)


-- ret: Vect (plus n 0) a
