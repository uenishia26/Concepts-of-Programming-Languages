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

  let reverse_list revList =
    let rec reverseFormList rev acc =
      match rev with
      | [] -> acc
      | h :: t -> reverseFormList t (h :: acc)
    in reverseFormList revList [];;
  




let convert (l : int_or_string list) : int_list_or_string_list list =
  let rec loop int_or_string current_int current_string acc = 
    match int_or_string with
      | [] ->  (*If the int_or_string list empty *)
        if current_int <> [] then (*Append any remaining items either in current_int or current_string list*)
          reverse_list (IntList (current_int) :: acc )

        else if current_string <> [] then 
          reverse_list (StringList (current_string) :: acc )

        else 
          reverse_list (acc)
      | h :: t -> 
          match h with 
            | Int i -> 
              if current_int = [] && current_string <> [] then
                  loop t (reverse_list(i :: current_int)) [] (StringList (current_string) :: acc)
              else 
                loop t (reverse_list(i :: current_int)) [] acc
            | String s -> 
              if current_string = [] && current_int <> [] then
                loop t [] (reverse_list(s :: current_string)) (IntList (current_int) :: acc)
              else 
                loop t [] (reverse_list(s :: current_string)) acc
    in loop l [] [] [];;
              

                 


  
