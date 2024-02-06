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

let isComptable cv toNewList =  (*cv = int value of currentHead; toNewList = the newList that its checking cv against*)
  match toNewList with
  | [] -> true
  | head :: tail -> 
      match head with 
        | Hot hint -> 
            if cv <> hint then true else false 
        | Icy iint -> 
            if cv <> iint then true else false




let reduce (l : temp list) : temp list =
  let rec reducer iterate_list newList = (*cotains the passed int value and the newList being created through recusion*)
    match iterate_list with (*We iterate through every item of the temp list l*)
      | [] -> newList (*If empty we reached base case *)
      | head :: tail -> (*Look at the first item *)
      match head with
        | Hot hint -> 
          if isComptable hint newList then reducer tail (head :: newList)
          else
            match newList with  (*nh = newList Head; nt = newList Tail *)
              | nh :: nt -> reducer tail nt  (*Just don't add the head will remove the top item*)
              | [] -> reducer tail (newList 
            
        | Icy iint -> 
          if isComptable iint newList then reducer tail (head :: newList)
          else
          match newList with  (*nh = newList Head; nt = newList Tail *)
            | nh :: nt -> reducer tail nt  (*Just don't add the head will remove the top item*)
            | [] -> reducer tail newList 
          



  in reducer l [];; 
          
 
        


              

