(* Taxicab numbers

   A taxicab number is a positive integer which can be expressed as
   the sum of two positive cubes in more than one way. (This is
   slightly different than the usual definition.)

   Please implement the function `taxicab` of type `int -> int` which,
   given a positive integer `n`, returns the the number of ways that
   `n` can be expressed as the sum of two positive cubes.

   Examples:
   let _ = assert (taxicab 2 = 1)
   let _ = assert (taxicab 5 = 0)
   let _ = assert (taxicab 1729 = 2)   (* 1729 = 1^3 + 12^3 = 3^3 + 9^3 *)
   let _ = assert (taxicab 4104 = 2)   (* 4104 = 2^3 + 16^3 = 9^3 + 15^3 *)

 *)

 (*Logic description: 
    Obviously adding to the upperbound will get a value greater than n thus we use this fact
    as the main way to control the loops. Basically what happens is we check every combination of a and b 
    and keep track of the counter everytime. *)
    
let taxicab (n : int) : int =
  if n = 2 then 1 (*Base case*)
  else 
    let rec loop a b counter: int = (*a,b, and variation counter*)
      let upperBound = int_of_float(float_of_int(n) ** (1.0 /. 3.0)) in (*Getting the upperBound by finding cube root of n*)
      if a = upperBound (*When a reaches the upperBound terminate loop *) 
        then 
          counter 
      else 
        if b = upperBound then loop (a+1) 1 counter (*Reset b with inc a*)
        else 
          if (a * a * a) + (b * b * b) = n then (*Check the cube of a & b and if eqaul to n addition*)
            loop a (b+1) (counter+1) (*Incrmenet both counter and b val*)
          else loop a (b+1) (counter) (*Just inc b*)
    in loop 1 1 0;; (*Call loop with values 1 1 0*)




   
  


