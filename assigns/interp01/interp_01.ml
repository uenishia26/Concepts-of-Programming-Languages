(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'

type 'a parser = char list -> ('a * char list) option

let satisfy f = function
  | c :: cs when f c -> Some (c, cs)
  | _ -> None

let char c = satisfy ((=) c)

let str s cs =
  let rec go = function
    | [], ds -> Some (s, ds)
    | c :: cs, d :: ds when c = d -> go (cs, ds)
    | _ -> None
  in go (explode s, cs)

let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None

let (>|=) p f = map f p
let (>|) p x = map (fun _ -> x) p

let seq p1 p2 cs =
  match p1 cs with
  | Some (x, cs) -> (
      match p2 cs with
      | Some (y, cs) -> Some ((x, y), cs)
      | None -> None
    )
  | None -> None

let (<<) p1 p2 = map fst (seq p1 p2)
let (>>) p1 p2 = map snd (seq p1 p2)

let map2 f p1 p2 =
  seq p1 p2 >|= fun (x, y) -> f x y

let optional p cs =
  match p cs with
  | Some (x, cs) -> Some (Some x, cs)
  | None -> Some (None, cs)

let rec many p cs =
  match p cs with
  | Some (x, cs) -> (
      match (many p cs) with
      | Some (xs, cs) -> Some (x :: xs, cs)
      | None -> Some ([x], cs)
    )
  | None -> Some ([], cs)

let many1 p = map2 cons p (many p)

let alt p1 p2 cs =
  match p1 cs with
  | Some x -> Some x
  | None ->
    match p2 cs with
    | Some x -> Some x
    | None -> None

let (<|>) = alt

let pure x cs = Some (x, cs)
let fail _ = None

let bind p f cs =
  match p cs with
  | Some (x, cs) -> f x cs
  | None -> None

let (>>=) = bind

let choice ps =
  List.fold_left (<|>) fail ps

let ws = many (satisfy is_blank)
let keyword w = str w << ws

let rec_parser p =
  pure () >>= p

let parse p s =
  match p (explode s) with
  | Some (x, []) -> Some x
  | _ -> None

(* END OF UTILITIES *)

(* ============================================================ *)

(* BEGINNING OF PROJECT CODE *)
type ident = string
type command
  = Drop                   (* drop *)
  | Swap                   (* swap *)
  | Dup                    (* dup *)
  | Trace                  (* . *)
  | Add                    (* + *)
  | Sub                    (* - *)
  | Mul                    (* * *)
  | Div                    (* / *)
  | Lt                     (* < *)
  | Eq                     (* = *)
  | Bind of ident          (* |> ID *)
  | Call of ident          (* # ID *)
  | If of program          (* ? prog ; *)
  | Def of ident * program (* def prog ; *)
  | Ident of ident         (* ID *)
  | Num of int             (* num *)
and program = command list

let parse_ident = 
 let ident_char = many1 (satisfy is_upper_case) in 
  ident_char >|= fun c -> implode c
(* You are not required to used this but it may be useful in
   understanding how to use `rec_parser` *)
(* let map f p cs =
  match p cs with
  | Some (x, cs) -> Some (f x, cs)
  | None -> None*)
let parse_drop = 
 map (fun a -> Drop) (keyword "drop" << ws)

let parse_swap = 
 map (fun a-> Swap) (keyword "swap" << ws)

let parse_dup = 
 map (fun a -> Dup) (keyword "dup" << ws)

let parse_trace = 
 map (fun a -> Trace) (keyword "." << ws)

let parse_add = 
 map (fun a -> Add) (keyword "+" << ws)

let parse_sub = 
 map (fun a -> Sub) (keyword "-" << ws)

let parse_mul = 
 map (fun a -> Mul) (keyword "*" << ws)

let parse_div = 
 map (fun a -> Div) (keyword "/" << ws)

let parse_lt = 
 map (fun a -> Lt) (keyword "<" << ws)

let parse_eq = 
 map (fun a -> Eq) (keyword "=" << ws)

let parse_bind = 
 map (fun id -> Bind (id)) (keyword "|>" >> parse_ident << ws)

let parse_call =
 map (fun id -> Call (id)) (keyword "#" >> parse_ident << ws)

(* For number cases *)
let parse_num = 
 let num_char = many1 (satisfy is_digit ) in 
   num_char >|= fun c -> Num (int_of_string (implode c))

(*For non terminal ident / above handles recursive ident case*)
let parse_identifier = 
 map (fun id -> Ident id) (parse_ident << ws)


let rec parse_com () =
  let parse_def =
    map2
      (fun id p -> Def (id, p))
      (keyword "def" >> parse_ident << ws)
      (parse_prog_rec () << char ';')
  and parse_if = 
    map2 
      (fun id p -> (If p))
      (keyword "?" << ws)
      (parse_prog_rec () << char ';')
  in parse_def <|> parse_if <|> parse_drop  <|> parse_swap  <|> parse_dup  <|> parse_trace  <|> parse_add <|> parse_sub <|> parse_mul <|> parse_div <|> parse_eq <|> parse_lt <|> parse_bind <|> parse_call <|> parse_div <|> parse_num <|> parse_identifier
and parse_prog_rec () =
  many ((rec_parser parse_com) << ws)

let parse_prog = 
  fun x -> 
  match ws (explode x) with 
  |Some (_,input) -> 
   ( match parse_prog_rec () input with 
     | Some (p, []) -> Some p
     | _ -> None)
  | _ -> None 


(* A VERY SMALL TEST SET *)
(*
let test = parse_prog "drop"
let out = Some [Drop]
let _ = assert (test = out)

let test = parse_prog "     .       "
let out = Some [Trace]
let _ = assert (test = out)

let test = parse_prog "  |> TEST   "
let out = Some [Bind "TEST"]
let _ = assert (test = out)

let test = parse_prog "  23 00345 + |> OK "
let out = Some [Num 23; Num 345; Add; Bind "OK"]
let _ = assert (test = out)

let test = parse_prog "  def NEG 0 - ; 2 #NEG 2 =    \n\n   "
let out = Some [Def ("NEG", [Num 0; Sub]); Num 2; Call "NEG"; Num 2; Eq]
let _ = assert (test = out)

let test = parse_prog "
  def ABS
    dup 0 swap < ?
      0 -
    ;
  ;

  30 0 -
  #ABS
  |> X
"
let out = Some
    [ Def ("ABS", [Dup; Num 0; Swap; Lt; If [Num 0; Sub]])
    ; Num 30; Num 0; Sub
    ;  Call "ABS"
    ;  Bind "X"
    ]
let _ = assert (test = out)
*)

(* EVALUATION *)
(*This is the (Stack, Enviroment, Trace, Program)*)
type stack = int list
type value
  = Num of int
  | Prog of program
type env = (ident * value) list
type trace = string list

let update_env =
  fun (e: env) (var: ident) (values: value) -> (var, values) :: e (* TODO *)
let fetch_env =
  fun (e: env) (var: ident) ->
  let rec fetch (e: env) (var: ident) = (
    match e with
    | [] -> None
    | (vars, values) :: tails when var = vars -> Some values
    | head :: tail -> fetch tail var
  )
  in fetch e var (* TODO *)
let rec eval_prog =
  fun (s: stack) (e: env) (p: program) (t: trace) ->
    match p with
    | command :: rest -> (
      match command with
      | Drop -> (
        match s with
        | [] -> eval_prog [] e [] ("panic drop error"::t)
        | head :: tail -> eval_prog tail e rest t
      )
      | Swap -> (
        match s with
        | first :: second :: tail -> eval_prog (second :: first :: tail) e rest t
        | _ -> eval_prog s e rest ("panic swap error"::t)
      )
      | Dup -> (
        match s with
        | [] -> eval_prog [] e [] ("panic dup error"::t)
        | first :: tail -> eval_prog (first :: s) e rest t
      )
      | Trace -> (
        match s with
        | [] -> eval_prog [] e [] ("panic trace error"::t)
        | n :: tail -> eval_prog tail e rest ((string_of_int n) :: t)
      )
      | Num n -> eval_prog (n :: s) e rest t
      | Add -> (
        match s with
        | first :: second :: tail -> eval_prog ((first+second)::tail) e rest t
        | _ -> eval_prog s e [] ("panic add error"::t)
      )
      | Sub -> (
        match s with
        | first :: second :: tail -> eval_prog ((first-second)::tail) e rest t
        | _ -> eval_prog s e [] ("panic sub error"::t)
      )
      | Mul -> (
        match s with
        | first :: second :: tail -> eval_prog ((first*second)::tail) e rest t
        | _ -> eval_prog s e [] ("panic mul error"::t)
      )
      | Div -> (
        match s with
        | first :: second :: tail -> (
          match second with
          | 0 -> eval_prog s e [] ("panic divide by zero"::t)
          | _ -> eval_prog ((first/second)::tail) e rest t
        )
        | _ -> eval_prog s e [] ("panic divde error"::t)
      )
      | Lt -> (
        match s with
        | first :: second :: tail -> (
          if (first < second) then eval_prog (1::tail) e rest t
          else eval_prog (0::tail) e rest t
        )
        | _ -> eval_prog s e [] ("panic lt error"::t)
      )
      | Eq -> (
        match s with
        | first :: second :: tail -> (
          if (first = second) then eval_prog (1::tail) e rest t
          else eval_prog (0::tail) e rest t
        )
        | _ -> eval_prog s e [] ("panic eq error"::t)
      )
      | Bind id -> (
        match s with
        | [] -> eval_prog [] e [] ("panic var error"::t)
        | n :: tail -> eval_prog tail (update_env e id (Num n)) rest t
      )
      | Ident id -> (
        match (fetch_env e id) with
        | Some (Num n) -> eval_prog (n::s) e rest t
        | _ -> eval_prog s e [] ("panic fetch error" :: t)
      )
      | Def (id, q) -> eval_prog s (update_env e id (Prog q)) rest t
      | Call id -> (
        match (fetch_env e id) with
        | Some (Prog prog) -> eval_prog s e (prog @ rest) t
        | _ -> eval_prog s e [] ("panic call error"::t)
      )
      | If prog -> (
        match s with
        | [] -> eval_prog [] e [] ("panic if error"::t)
        | 0 :: tail -> eval_prog tail e rest t
        | _ :: tail -> eval_prog tail e (prog @ rest) t
      )
    )
    | _ -> t (* TODO *)
let interp =
  fun (code: string) ->
    match parse_prog code with
    | Some prog -> (
      match eval_prog [] [] prog [] with
      | t -> Some t
    )
    | None -> None (* TODO *)

(* END OF PROJECT CODE *)

(* ============================================================ *)

(* UNCOMMENT TO RUN INTERPRETER *)

let print_trace t =
  let rec go t =
    match t with
    | [] -> ()
    | x :: t ->
      print_endline x;
      go t
  in go (List.rev t)


let main () =
  let input =
    let rec get_input s =
      try
        get_input (s ^ "\n" ^ read_line ())
      with End_of_file ->
        s
    in get_input ""
  in
  match interp input with
  | None -> print_endline "Parse Error"
  | Some t -> print_trace t

let _ = main ()


(*Collaborate Moryan Tchumoi, Nick Galis*)


    