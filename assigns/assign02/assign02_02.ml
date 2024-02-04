(* Recipes by Ingredients

   Implement a function `recs_by_ingrs` which given

     recs : a list of recipes
     ingrs : a list of ingredients (i.e., strings)

   returns the list of those recipes in `recs` (in the same order)
   whose ingredients are included in `ingrs`.

   You may assume that `ingrs` and `r.ingrs` for every `r` in `recs`
   do not contain duplicates.

   Hint: The function List.mem may be useful.

   Example:
   let r1 = { name = "1" ; ingrs = ["a"; "b"; "d"] }
   let r2 = { name = "2" ; ingrs = ["a"; "c"; "e"] }
   let r3 = { name = "3" ; ingrs = ["b"; "c"] }
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"d"] = [r1;r3])
   let _ = assert (recs_by_ingrs [r1;r2;r3] ["a";"b";"c";"e"] = [r2;r3])

*)

type ingr = string

type recipe = {
  name : string ;
  ingrs : ingr list;
}
(*Basically using filtering techniques*)

let recs_by_ingrs (l : recipe list) (s : ingr list) : recipe list =
  let rec recipe_filter recipes = 
    match recipes with
    | [] -> [] (*Base case*)
    | r :: rs -> (*r = current recipe, rs = rest of the recipes*)
      let rec iterateIngredients list = 
        match list with
        | [] -> true (*If zero elements left and its still true means every item existed in the required ingredient list*)
        | head :: tail ->  (*If doesn't exist just move onto next recipe*)
          if List.mem head s = false then false else iterateIngredients tail
      in 
      if iterateIngredients r.ingrs = true then r :: recipe_filter rs (*If true, we want to add the recipe else don't*)
      else recipe_filter rs 
  in recipe_filter l;;

