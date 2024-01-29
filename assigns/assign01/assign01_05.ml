(* Block text

   Please implement the function `block_text` of type `string ->
   string` which, given

   - a string `s` consisting only of capital English characters A-Z
   - a nonnegative integer `min_width`
   - a positive integer `max_width`

   returns a string with the same characters as `s` but separated into
   lines with the following properties:

   - each line has length at most `max_width`
   - each line has the same length except possibly the last line
   - the last line has length at least `min_width`

   If the above three properties are not possible to satisfy, then
   every line except for the last line of the returned string should
   have length `max_width` (in other words, ignore the last
   condition).

   If there are multiple ways to satisfy the above three properties,
   the choice with the longest lines (besides the last line) should be
   used.

   Hint: Take a look at the following functions in the OCaml
   documentation:

   - `String.sub`
   - `String.length`

   Examples:
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")  DONE
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")   DONE
   let _ = assert (block_text "ABDFEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")   

   ABC, def, GHI, J 1 < 2
 *)

let block_text (s : string) (min_width : int) (max_width : int) : string =
    (*If divisible by max_width, we don't care about the min_width requirment *)
  let rec subStringloop x y index currentStr : string = (*Parameters here are start position for substring, max_width counter and newStr*)
    
    
    (*When counter equals length of s we reached base case *)
    if index = String.length s then
      if String.length currentStr > 0 && currentStr.[String.length currentStr - 1] = '\n' then
        String.sub currentStr 0 (String.length currentStr - 1)
      else 
        currentStr
    
    else         
         
      if (String.length s) mod (max_width) = 0 then (*If divisible by max width, every line is maxwidth long*)
        subStringloop (x+y) y (index+max_width) (currentStr ^ (String.sub s x y) ^ "\n") (*We add \n*)

      else if ((String.length s) mod (max_width)) >= min_width || (min_width > max_width) then (*If the remainder is greater or equal to the minimum width required*)
        if(index + y) > String.length s then (*If adding max width produce out of bounds, we just wanna concatenate the remaining end BASE CASE*)
          subStringloop (x+y) y (index+((String.length s) - index)) (currentStr ^ (String.sub s x ((String.length s) - index)))
         else
          subStringloop (x+y) y (index+max_width) (currentStr ^ (String.sub s x y) ^ "\n") (*We add \n*) 

      else  (*Case where the remainder after divsion is less than the minimum width*)
        if (String.length s) mod min_width = 0 then 
          subStringloop (x+min_width) y (index+min_width) (currentStr ^ (String.sub s x min_width) ^ "\n")
        else 
          (*case where remainder after division *)
          if(index + y) > String.length s then (*If adding max width produce out of bounds, we just wanna concatenate the remaining end BASE CASE*)
            subStringloop (x+y) y (index+((String.length s) - index)) (currentStr ^ (String.sub s x ((String.length s) - index)))
           else
            subStringloop (x+y) y (index+max_width) (currentStr ^ (String.sub s x y) ^ "\n") (*We add \n*) 
          
  in subStringloop 0 max_width 0 "";; (*X = starting point, Y = maxWidth value, index = 0, currentStr = ""*)


