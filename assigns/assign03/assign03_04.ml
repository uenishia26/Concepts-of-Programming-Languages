(* Matrices

   In this problem you will be building a (very) small interface for
   matrices.  A matrix is represented as a record which keeps track of
   its numbers of rows and columns as well as the values represented
   as a list of rows (i.e., a list of lists).  You will be
   implementing some error handling by working with `result`s.

   ************

   Task 1: Implement a function `mkMatrix` which given

     rs : a list of lists

   returns a matrix represented by this list of lists if it is valid
   or an `error` otherwise.  The error conditions are:

   * If the lengths of the rows in `rs` are not all equal then
   `mkMatrix` should return `Error UnevenRows`.

   * If `rs` is empty then `mkMatrix` should return `Error ZeroRows`.

   * If `rs` contains only empty lists, then `mkMatrix` should reutrn
   `Error ZeroCols`.

   In other words, `mkMatrix` should only return a matrix if `rs`
   represents a nonempty rectangular grid of values.

   Example:
   let l = [[1;2;3];[4;5;6]]
   let rm = mkMatrix l
   let _ = match rm with
     | Ok m ->
       let _ = assert (m.num_rows = 2) in
       let _ = assert (m.num_cols = 3) in
       ()
     | _ -> assert false

   let r = [[1;2;3;4];[1;2;3]]
   let rm' = mkMatrix r
   let _ = match rm' with
     | Ok _ -> assert false
     | Error e -> assert (e = UnevenRows)

   ************

   Task 2: Implement the function `transpose` which, given Worked with Moryan 

     m : a matrix

   returns the transpose of `m`.

   Example:
   let _ = match rm with
     | Ok m ->
       let tm = transpose m in
       let _ = assert (tm.num_rows = 3) in
       let _ = assert (tm.num_cols = 2) in
       let _ = assert (tm.rows = [[1;4];[2;5];[3;6]]) in
       ()
     | _ -> assert false

   ************

   Task 3: Implement the function `multiply` which, given

     m : a matrix
     n : a matrix

   returns the product of `m` and `n` if it is possible to multiply
   them. The error condition:

   * If the number of columns of `m` is not the same as the number of
   rows of `n`, then `multiply` should return `Error MulMismatch`

   Example:
   let a =
     { num_rows = 2 ;
       num_cols = 2 ;
       rows = [[1.;2.];[3.;4.]] ;
     }

   let b =
     { num_rows = 1 ;
       num_cols = 2 ;
       rows = [[1.; 2.]] ;
     }

   let _ = assert (multiply a a = Ok {
     num_rows = 2 ;
     num_cols = 2 ;
     rows = [[7.;10.];[15.;22.]] ;
    })

   let _ = assert (multiply a b = Error MulMismatch)

   ************

   References:
   https://en.wikipedia.org/wiki/Matrix_multiplication
   https://en.wikipedia.org/wiki/Transpose
   https://www.cs.bu.edu/fac/crovella/cs132-book/landing-page.html

*)

type error
   = UnevenRows
   | ZeroRows
   | ZeroCols
   | MulMismatch

type 'a matrix = {
  num_rows : int ;
  num_cols : int ;
  rows : ('a list) list ;
}

let mkMatrix (rs : 'a list list) : ('a matrix, error) result =
  let rec row_len hd rs =
    match rs with
    | [] -> true 
    | row :: rest ->
      List.length row = List.length hd && row_len hd rest
  in
  match rs with
  | [] -> Error ZeroRows
  | hd :: _ ->
    (match hd with
     | [] -> Error ZeroCols
     | _ -> if not (row_len hd rs) then Error UnevenRows
       else Ok { num_rows = List.length rs; num_cols = List.length hd; rows = rs }
    )

let transpose (m : 'a matrix): 'a matrix  =
  let rec first_column (m : 'a list list): 'a list  =
    match m with
    | [] -> []
    | h :: t -> 
      match h with
      | [] -> []
      | h1 :: t1 -> 
        match first_column t with
        | [] -> [h1]
        | t2 -> h1 :: t2
  and rest_columns (m : 'a list list): 'a list list =
    match m with
    | [] -> []
    | h :: t -> 
      match h with
      | [] -> []
      | h1 :: t1 -> 
        match rest_columns t with
        | [] -> [t1]
        | t2 -> t1 :: t2
  in
  let rec helper ret matrix =
    match first_column matrix with
    | [] -> List.rev ret
    | col ->
      let new_matrix = rest_columns matrix in
      helper (col :: ret) new_matrix
  in 
  let transposing m =
  match m.rows with
  | [] -> {num_rows=0;num_cols=0; rows=[]}
  | _ -> {num_rows= m.num_cols; num_cols = m.num_rows; rows=helper [] m.rows}
  in transposing m  
    
let multiply (m : float matrix) (n : float matrix) : (float matrix, error) result =
  let rec dot_product (a: float list) (b: float list): float = 
    match (a,b) with
    | ((h1::t1),(h2::t2)) -> (h1 *. h2) +. dot_product t1 t2
    | (_,_) -> 0.
  in
  let rec mv_mul (a: float list list) (b: float list): float list =
    match a with
    | [] -> []
    | (h::t) -> dot_product h b :: mv_mul t b
  in
  let rec mul (a: float list list) (b: float list list) (c: float list list): float list list = 
    match b with
      | [] -> List.rev c
      | h::t -> 
      let new_matrix = mv_mul a h in mul a t (new_matrix :: c)
  in 
  let multiplication (a: float matrix) (b: float matrix): (float matrix, error) result = 
    if (a.rows = [] && b.rows = []) then Ok {num_rows=0;num_cols=0; rows = []}
    else if (a.num_cols <> b.num_rows) then Error MulMismatch
    else Ok (transpose {num_rows=b.num_cols;num_cols=a.num_rows; rows = mul a.rows (transpose b).rows []})
  in multiplication m n 