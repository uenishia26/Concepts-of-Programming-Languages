(* Primality check

   Please implement a function `is_prime` of type `int -> bool` such that
   when given an integer `n` greater than or equal to 0, it returns a boolean value.
   If the input is prime return true, otherwise return false.

   Note 1: 0 and 1 are not considered prime numbers.
   Note 2: In OCaml, the modulo operation is written a `4 mod 3`. This specific instance will result in 1. 

   Examples:
   is_prime 0 = false
   is_prime 1 = false
   is_prime 2 = true
   is_prime 37 = true
   is_prime 57 = false
   is_prime 97 = true

*)

let rec is_prime (n : int) : bool =
   if n = 1 || n = 0
      then false 
   else if n = 2 (*Three base canse for when n = 0,1,2*)
      then true
   else
      let rec loop i = (*Way to create a recursive loop*)
         if i >= n then (*regular loop but using a recursive function *)
            true
         else if(n mod i) = 0 then (*This is the defnition of prime number. If any number exists divisible from 2 to n-1 then its false*)
            false
         else 
            loop(i+1) 
         in loop 2

