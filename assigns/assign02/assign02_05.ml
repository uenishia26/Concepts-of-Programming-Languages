(* Listing Paths

   Implement a function `all_paths` which given

     len : a nonnegative integer
     stp : a point (see below)
     endp : a point

   returns a list of `(dir * int)` lists, where each list contains
   sequence of directions to get from `stp` to `endp` in exactly `len`
   steps.

   Notes:
   A sequence of directions is a list of `dir * int` pairs with the
   property that no adjacent elements have the same `dir`.  For example,

     [(N, 1); (S, 1); (N, 3)]

   is a valid sequence of directions but

     [(N, 1); (N, 3); (S, 1)]

   is not. Instead, this should be

     [(N, 4), (S, 1)]

   Examples:
   let origin = {x=0;y=0}
   let _ = assert (all_paths 0 origin origin = [[]])
   let _ = assert (all_paths 1 origin origin = [])
   let _ = assert (all_paths 2 origin origin =
       [[(N,1);(S,1)] ;
        [(S,1);(N,1)] ;
        [(E,1);(W,1)] ;
        [(W,1);(E,1)] ])
   let _ = assert (all_paths 3 origin origin = [])
   let _ = assert (List.mem [(N,2);(S,2)] (all_paths 4 origin origin))
   let _ = assert (List.mem [(N,1);(E,1);(W,1);(S,1)] (all_paths 4 origin origin))

*)

type dir = N | S | E | W

type point = {
  x: int;
  y: int;
}
type dir = N | S | E | W

type point = {
  x: int;
  y: int;
}

let rec all_paths (len: int) (stp: point) (endp: point): (dir * int) list list =
  let rec move (p: point) d dx dy endp len path =
    if len = 0 then
      if p = endp then
        [path]
      else
        []
    else
      let new_point = { x = p.x + dx; y = p.y + dy } in
      direct new_point endp (len - 1) (path @ [(d, 1)])
  and direct str endp len path =
    if len = 0 then
      if str = endp then
        [path]
      else
        []
    else
      let north = move str N 0 1 endp len path in
      let south = move str S 0 (-1) endp len path in
      let east = move str E 1 0 endp len path in
      let west = move str W (-1) 0 endp len path in
      let allMoves = north @ south @ east @ west in
      let rec combine_dir acc lst =
        match lst with
        | (d1, n1) :: (d2, n2) :: tl when d1 = d2 -> combine_dir acc ((d1, n1 + n2) :: tl)
        | hd :: tl -> combine_dir (hd :: acc) tl
        | [] -> List.rev acc
      in
      let rec combine lst =
        match lst with
        |[] -> []
        |h :: t -> (combine_dir [] h) :: t
      in
      combine allMoves
  in
  direct stp endp len []
