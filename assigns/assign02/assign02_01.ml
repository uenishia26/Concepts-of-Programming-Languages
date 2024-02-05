(* Grouping Integers and Strings

   Implement a function `convert` which given

     l : a list of `int_or_string`s

   returns a list of `int_list_or_string_list`s such that adjacent
   `int`s and `string`s are grouped together.

   Example:
   let test_in = [Int 2; Int 3; String "a"; String "b"; Int 4; String "c"]
   let test_out = [IntList [2;3]; StringList ["a";"b"]; IntList [4]; StringList ["c"]]
   let _ = assert (convert test_in = test_out)

*)

type int_list_or_string_list
  = IntList of int list
  | StringList of string list

type int_or_string
  = Int of int
  | String of string

(*Helper function *)
let reverse_list revList =
  let rec reverseFormList rev newList =
    match rev with
    | [] -> newList
    | h :: t -> reverseFormList t (h :: newList)
  in reverseFormList revList [];;
  




let convert (l : int_or_string list) : int_list_or_string_list list =
  if l = [] then (*If its an empty list passed through just return empty list*)
    []
else 
  let rec loop int_or_string current_int current_string acc = 
    match int_or_string with
      | [] ->  (*If the int_or_string list empty *) (*Have to reverse accordingly*)
        if current_int <> [] then (*Append any remaining items either in current_int or current_string list*)
          reverse_list(IntList (reverse_list(current_int)) :: acc )
        else if current_string <> [] then 
          reverse_list(StringList (reverse_list(current_string)) :: acc) 
          
        else 
          reverse_list(acc)
      | h :: t -> (*Checking if the head is an Int or String*)
          match h with 
            | Int i -> 
              if current_int = [] && current_string <> [] then (*Different cases that act accordingly *)
                  loop t (i :: current_int) [] (StringList (reverse_list(current_string)) :: acc)
              else 
                loop t (i :: current_int) [] acc
            | String s -> 
              if current_string = [] && current_int <> [] then
                loop t [] (s :: current_string) (IntList (reverse_list(current_int)) :: acc)
              else 
                loop t [] (s :: current_string) acc
    in loop l [] [] [];; (*The input to the recursive function*)
              

                 


  
