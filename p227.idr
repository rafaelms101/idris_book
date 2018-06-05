--1
reverseEq : x = y -> y = x
reverseEq Refl = Refl

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m =
  let prof = plusZeroRightNeutral m
  in reverseEq prof

myPlusCommutes (S k) m =
  let rec = myPlusCommutes k m
  in let prof1 = plusSuccRightSucc k m
  in ?test

--test : plus k (S m) = plus m (S k)
--test : S (plus k m) = plus m (S k)

--In the case of S k , you can rewrite with a recur-sive call to myPlusCommutes k m , and rewrites can be nested.
