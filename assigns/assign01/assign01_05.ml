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
   let _ = assert (block_text "ABCDEFGHIJ" 0 3 = "ABC\nDEF\nGHI\nJ")
   let _ = assert (block_text "ABCDEFGHIJ" 2 3 = "AB\nCD\nEF\nGH\nIJ")
   let _ = assert (block_text "ABCDEFGHIJ" 0 4 = "ABCD\nEFGH\nIJ")
   let _ = assert (block_text "ABDFEFGHIJ" 3 4 = "ABCD\nEFGH\nIJ")

 *)

let block_text (s : string) (min_width : int) (max_width : int) : string =
    (*If divisible by max_width, we don't care about the min_width requirment *)
  if (String.length s) mod (max_width) = 0 then
    let rec subStringloop x y counter currentStr : string = (*Parameters here are start position for substring, max_width counter and newStr*)
      if counter = (String.length s) / max_width then (*Base case we return the string*)
        currentStr
      else (*Two cases for concatenation. Looking out for last case so we don't add \n in the last line vs all the other cases*)
        if (counter+1) = (String.length s) / max_width then 
          subStringloop (x+y) y (counter+1) (currentStr ^ (String.sub s x y)) (*We don't add \n to the last line *)
        else subStringloop (x+y) y (counter+1) (currentStr ^ (String.sub s x y) ^ "\n") (*We add \n to the last line *)
    in subStringloop 0 max_width 0 ""

  (*When string not divisble by max_width we have two case
     1) remainder after mod with max > min width
     2) remainder after mod with max < min width*)
  else
    if (String.length s) mod (max_width) > min_width then
      let rec subStringlooptwo x y counter current:Str : String = 
        
        
      in subStringlooptwo 



      

