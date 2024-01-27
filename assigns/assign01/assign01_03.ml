(* Fast Fibonacci

   The Fibonacci numbers as defined as follows:
   - F(0) = 1
   - F(1) = 1
   - F(n) = F(n - 1) + F(n - 2)

   Please implement the function `fast_fib` of type `int -> int`
   which, given a nonnegative integer `n`, returns the nth Fibonacci
   number F(n). You must give a TAIL-RECURSIVE implementation.

   Hint: In the tail-recursive version, you cannot make two recursive
   calls. See the associated problem the textbook (OCP 2.9) for
   further details.

   Examples:
   let _ = assert (fast_fib 0 = 1)
   let _ = assert (fast_fib 1 = 1)
   let _ = assert (fast_fib 2 = 2)
   let _ = assert (fast_fib 3 = 3)
   let _ = assert (fast_fib 4 = 5)
   let _ = assert (fast_fib 5 = 8)

 *)
(*This implementation is *)
let fast_fib (n : int) : int =
  if n = 0 || n = 1 then 1 (*If n = 0 or 1 just return 1*)
  else
    let rec fibrec x y sum counter: int = (*x and y start with 1 and 1*)
      if counter = n then sum  (*return sum*)
      else
        fibrec y sum (y+sum) (counter+1) (*Keeps track of previous fib value, x, y, counter. Correctly computes*)
    in fibrec 1 1 2 2;; (*Starting with values of fib 2*)
    

