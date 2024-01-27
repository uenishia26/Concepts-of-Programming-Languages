(* Perfect numbers

   A positive integer `n` is perfect if it is equal to the sum of its
   proper divisors.

   Please implement the function `is_perfect` of type `int -> bool`
   which, given an positive integer `n`, returns `true` if `n` is
   perfect and `false` otherwise.

   Examples:
   let _ = assert (is_perfect 6)        (* 1 + 2 + 3 = 6 *)
   let _ = assert (is_perfect 28)       (* 1 + 2 + 4 + 7 + 14 = 28 *)
   let _ = assert (not (is_perfect 24)) (* 1 + 2 + 3 + 4 + 6 + 8 + 12 <> 24 *)

 *)

let is_perfect (n : int) : bool =
  if n <= 1 then false (*Taking care of base cases *)
  else 
    let rec loop div sum: bool = (*Recursive function*)
      if div = 1 then (*Base case when divisor reaches 1*)
        if sum+1 = n then true (*Check if sum = n /Need + 1 cause 1 divsor of all numbers >1*)
        else false 
      else 
        if n mod div = 0 then loop (div-1) (sum+(div)) (*If number can be divided we add it otherwise move to next divsior (-1 current divisor)*)
        else loop (div-1) (sum)
    in loop(n-1) 0;; (*Start with n-1 and 0 as the sum*)


