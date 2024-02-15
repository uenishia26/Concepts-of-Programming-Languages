(* Concatenation Lists

   A `concatlist` is a list whose constructors are based on
   concatenation instead of cons.  It has a constructor for the empty
   list (Nil), a single element list (Single) and the concatentation
   of two lists (Concat).

   Implement a function `sort` which given

     l : a `concatlist`

   returns a regular list with the same element as `l` but in sorted
   order.  You should do this WITHOUT trying to first converting `l`
   into a regular list.  In particular, you CANNOT use the function
   `List.sort`.

   Example:
   let l = Concat (Concat (Single 3, Single 2), Concat (Single 1, Single 10))
   let _ = assert (sort l = [1;2;3;10])

*)

type 'a concatlist
  = Nil
  | Single of 'a
  | Concat of 'a concatlist * 'a concatlist


    
(* Merges two sorted lists into a single sorted list *)
let rec merge l1 l2 =
  match (l1, l2) with
  | [], _ -> l2  (* If the first list is empty, return the second list *)
  | _, [] -> l1  (* If the second list is empty, return the first list *)
  | h1::t1, h2::t2 ->
      if h1 < h2 then
        (* If the first element of l1 is smaller, it's included first *)
        h1 :: merge t1 l2
      else
        (* Otherwise, the first element of l2 is smaller or equal and included first *)
        h2 :: merge l1 t2

let sort (l : 'a concatlist) : 'a list =
  let rec recSort concatlist= 
    match concatlist with
      | Nil -> []
      | Single x -> [x]
      | Concat (left, right) -> 
        let leftArry = recSort left in
        let rightArry = recSort right in
        merge leftArry rightArry

    in recSort l

      
          
        
              
        
            
        
      

      


  
