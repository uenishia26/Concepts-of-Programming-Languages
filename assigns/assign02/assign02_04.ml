(* Icy Hot

   Implement the function `reduce` which given

     l : a list of `temp`s

   returns a new list of `temp`s gotten by the following reduction rule:

   If `Hot i` and `Icy i` are adjacent (in particular, they must be
   carrying the same value) in any order, then they cancel out and are
   removed from the list.

   This rule should be carried out until it not possible to reduce the
   list any further.

   Examples:
   let _ = assert (reduce [Hot 0;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Icy 1;Hot 1;Icy 0] = [])
   let _ = assert (reduce [Hot 0;Hot 1;Icy 2;Hot 2;Hot 0;Icy 1] = [Hot 0;Hot 1;Hot 0;Icy 1])

*)

(*This function removes the last element
   [1;2;3;4] -> [1;2;3] but cause reversed [3;2;1]*)

let reverse_list revList =
  let rec reverseFormList rev newList =
    match rev with
    | [] -> newList
    | h :: t -> reverseFormList t (h :: newList)
  in reverseFormList revList [];;

type temp
  = Hot of int
  | Icy of int

let canAdd cv toNewList =  (*cv = int value of currentHead; toNewList = the newList that its checking cv against*)
  match toNewList with
  | [] -> true
  | head :: tail -> (*Match the top value of the toNewList with cv number*)
      match head with 
        | Hot hint -> 
            if cv = hint then false else true 
        | Icy iint -> 
            if cv = iint then false else true
            let reduce (l : temp list) : temp list =
              let rec reducer iterate_list newList =
                match iterate_list with
                | [] -> List.rev newList (* Reverse the newList to maintain original order before returning *)
                | head :: tail ->
                    (match head with
                     | Hot hint ->
                         (match newList with
                          | [] -> reducer tail (head :: newList) (* If newList is empty, just add the head *)
                          | Icy iint :: t when hint = iint -> reducer tail t (* Cancel out Hot and Icy with same value *)
                          | _ -> reducer tail (head :: newList)) (* Default case: add the head to newList *)
                     | Icy iint ->
                         (match newList with
                          | [] -> reducer tail (head :: newList) (* If newList is empty, just add the head *)
                          | Hot hint :: t when iint = hint -> reducer tail t (* Cancel out Icy and Hot with same value *)
                          | _ -> reducer tail (head :: newList))) (* Default case: add the head to newList *)
              in
              reducer l [];;




          
 
        


              

