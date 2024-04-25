(* UTILITIES *)
let cons x xs = x :: xs
let explode s = List.of_seq (String.to_seq s)
let implode cs = String.of_seq (List.to_seq cs)
let is_digit c = '0' <= c && c <= '9'
let is_blank c = String.contains " \012\n\r\t" c
let is_upper_case c = 'A' <= c && c <= 'Z'
let is_lower_case c = 'a' <= c && c <= 'z'

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
let ( let* ) = bind

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

(* REQUIRED TYPES *)

type ident = string

type const
  = Num of int
  | Bool of bool

type command
  = Push of const | Trace
  | Add | Mul | Div
  | And | Or | Not | Lt | Eq
  | If of program * program
  | While of program * program
  | Bind of ident | Fetch of ident
  | Fun of program | Call | Return
  | Debug of string

and program = command list

and bindings = (ident * value) list

and value
  = Const of const
  | Clos of
      { def_id : int
      ; captured : bindings
      ; prog : program
      }

type record =
  { id : int
  ; local : bindings
  ; called_def_id : int
  ; return_prog : program
  }

type stack = value list
type trace = string list
type env
  = Global of bindings
  | Local of record * env

(* get the id of the topmost record *)
let local_id = function
  | Global _ -> 0
  | Local (r, _) -> r.id

(* convert a value to a string *)
let to_string v =
  match v with
  | Const (Bool true) -> "True"
  | Const (Bool false) -> "False"
  | Const (Num n) -> string_of_int n
  | Clos _ -> "<Closure>"

(* PARSING *)

let parse_ident =
  map2
    (fun c cs -> implode (c :: cs))
    (satisfy is_lower_case)
    (many (satisfy (fun c -> is_lower_case c || is_digit c || c = '_')))

let parse_int =
  let mk_int sign cs =
    let abs = int_of_string (implode cs) in
    if Option.is_none sign
    then abs
    else -abs
  in
  map2
    mk_int
    (optional (char '-'))
    (many1 (satisfy is_digit))

let parse_bool =
  (str "True" >| true) <|> (str "False" >| false)

let parse_comment =
  char '(' >> many (satisfy ((<>) ')')) >> char ')' >| ()

let parse_debug =
  char '"' >> many (satisfy ((<>) '"')) << char '"' >|= implode

  let parse_trace =
    map (fun x -> Trace) (keyword "." << ws)
  
  
  let parse_and =
    map (fun x -> And) (keyword "&&" << ws)
  let parse_or =
    map (fun x -> Or) (keyword "||" << ws)
  let parse_not =
    map (fun x -> Not) (keyword "~" << ws)
  let parse_add =
    map (fun x -> Add) (keyword "+" << ws)
  let parse_mul =
    map (fun x -> Mul) (keyword "*" << ws)
  let parse_div =
    map (fun x -> Div) (keyword "/" << ws)
  let parse_eq =
    map (fun x -> Eq) (keyword "=" << ws)
  let parse_lt =
    map (fun x -> Lt) (keyword "<" << ws)
  let parse_bind =
    map (fun id -> Bind (id)) (keyword "|>" >> parse_ident << ws)
  let parse_call =
    map (fun id -> Call) (keyword "#" << ws)
  let parse_return =
    map (fun id -> Return) (keyword "Return" << ws)
  let parse_const =
    (parse_bool >|= (fun b -> Push (Bool b))) <|> (parse_int >|= (fun b -> Push (Num b)))
  

let ws = many (ws >> parse_comment) >> ws
let keyword w = str w << ws

let rec parse_com () =
  let parse_fun = 
    let* _ = keyword ":" in
    let* funBody = parse_prog_rec () in
    let* _ = char ';' in 
    pure (Fun funBody);
  in
  let parse_if =
    let* _ = keyword "?" in
    let* ifc = parse_prog_rec () in
    let* _ = keyword ";" in
    let* elsec = parse_prog_rec () in
    let* _ = char ';' in
    pure (If (ifc, elsec))
  in
  let parse_while =
    let* _ = keyword "While" in
    let* check = parse_prog_rec () in
    let* _ = keyword ";" in
    let* body = parse_prog_rec () in
    let* _ = char ';' in
    pure (While (check, body))
  in
  choice
    (* TODO: Add more alternatives *)
    [ parse_fun
    ; parse_while
    ; parse_if
    ; parse_ident >|= (fun s -> Fetch s)
    ; parse_debug >|= (fun s -> Debug s)
    ; parse_bind
    ; parse_not
    ; parse_lt
    ; parse_eq
    ; parse_call
    ; parse_return
    ; parse_const
    ; parse_trace
    ; parse_add
    ; parse_mul
    ; parse_div
    ; parse_and
    ; parse_or
    ]
and parse_prog_rec () =
  many (rec_parser parse_com << ws)

let parse_prog = parse (ws >> parse_prog_rec ())

(* FETCHING AND UPDATING *)

(* fetchicng the value of x in Enviroment*)
let rec fetch_env envir x = 
  match envir with
    | Global glob -> (
      let rec fetch_global glob =
        match glob with
        | [] -> None
        | (vars, values) :: tails when x = vars -> Some values
        |  _ :: tails  -> fetch_global tails in fetch_global glob
      )
    | Local (lr,le) -> (
      if local_id envir <> lr.id then fetch_env le x else
        let rec fetch_local lr =  
          match lr with
          | [] -> fetch_env le x
          | (vars, values) :: tails when x = vars -> Some values
          | _ :: tails -> fetch_local tails in fetch_local lr.local
      )

let rec update_env envir x v = 
  match envir with 
  | Local (recrd, envi) -> Local ({ recrd with local = (x, v) :: recrd.local }, envi)
  | Global binds -> Global ((x, v) :: binds)

(* EVALUTION *)

(* panic configuration given a configuration *)
let panic (_, _, t, _) msg = [], Global [], ("panic: " ^ msg) :: t, []

let rec merge b l = 
  match l with
  | [] -> b
  | hd :: tl -> if List.mem hd tl then merge b tl else merge (hd::b) tl

let rec eval_step (c : stack * env * trace * program) =
  match c with 
  (* This is for Push *)
  | s, e, t, Push c :: p -> Const c :: s, e, t, p

  (* This is for Trace *)
  | v :: s, e, t, Trace :: p -> s, e, to_string v :: t, p
  | [], _, _, Trace :: _ -> panic c "stack underflow"

  (* This is for Add *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Add :: p -> Const (Num (m + n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Add :: _ -> panic c "type error"
  | _ :: [], _, _, Add :: _ -> panic c "stack underflow"
  | [], _, _, Add :: _ -> panic c "stack underflow"

  (* This is for Multiply *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Mul :: p -> Const (Num (m * n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Mul :: _ -> panic c "type error"
  | _ :: [], _, _, Mul :: _ -> panic c "stack underflow"
  | [], _, _, Mul :: _ -> panic c "stack underflow"
  (* This is for Divide *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Div :: p -> Const (Num (m / n)) :: s, e, t, p
  | _ :: Const (Num 0) :: _, _, _, Div :: _ -> panic c "div by zero is not allowed"
  | _ :: _ :: _, _, _, Div :: _ -> panic c "type error"
  | _ :: [], _, _, Div :: _ -> panic c "stack underflow"
  | [], _, _, Div :: _ -> panic c "stack underflow"

  (* This is for And *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, And :: p -> Const (Bool (m && n)) :: s, e, t, p
  | _ :: _ :: _, _, _, And :: _ -> panic c "type error"
  | _ :: [], _, _, And :: _ -> panic c "stack underflow"
  | [], _, _, And :: _ -> panic c "stack underflow"

  (* This is for Or *)
  | Const (Bool m) :: Const (Bool n) :: s, e, t, Or :: p -> Const (Bool (m || n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Or :: _ -> panic c "type error"
  | _ :: [], _, _, Or :: _ -> panic c "stack underflow"
  | [], _, _, Or :: _ -> panic c "stack underflow"

  (* This is for Not *)
  | Const (Bool n) :: s, e, t, Not :: p -> Const (Bool (not n)) :: s, e, t, p
  | _ :: _ , _, _, Not :: _ -> panic c "type error"
  | [], _, _, Not :: _ -> panic c "stack underflow"

  (*This is for Less Than *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Lt :: p -> Const (Bool (m < n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Lt :: _ -> panic c "type error"
  | _ :: [], _, _, Lt :: _ -> panic c "stack underflow"
  | [], _, _, Lt :: _ -> panic c "stack underflow"

  (* This is for Equal *)
  | Const (Num m) :: Const (Num n) :: s, e, t, Eq :: p -> Const (Bool (m = n)) :: s, e, t, p
  | _ :: _ :: _, _, _, Eq :: _ -> panic c "type error"
  | _ :: [], _, _, Eq :: _ -> panic c "stack underflow"
  | [], _, _, Eq :: _ -> panic c "stack underflow"

  (*This is for If-Else *)
  | Const (Bool true) :: s, e, t, If(x,y) :: p -> s, e, t, x @ p
  | Const (Bool false) :: s, e, t, If(x,y) :: p -> s, e, t, y @ p
  | _ :: _ , _, _, If(x,y) :: _ -> panic c "type error"
  | [], _, _, If(x,y) :: _ -> panic c "stack underflow"

  (*This is for While *)
  | s, e, t, While(x,y) :: p -> s, e, t, x @ (If (y @ [While (x,y)], []) :: p)

  (* This is for Fetch *)
  | s, e, t, Fetch x :: p ->
    (match fetch_env e x with
    | None -> panic c "varible does not exist in the environment"
    | Some v -> v :: s, e, t, p)

  (* This is for Bind *)
  | v :: s, e, t, Bind x :: p -> s, update_env e x v, t, p
  | [], e, t, Bind x :: p -> panic c "stack underflow"

  (* This is for Fun *)
  | s, e, t, Fun func :: p -> Clos ({def_id = local_id e;captured=[];prog=func;}) :: s, e, t, p

  (* This is for Call *)
  | Clos ({def_id=i;captured=b;prog=q;}) :: s, e, t, Call :: p -> s, Local ({id=(local_id e) + 1;local=b;called_def_id=i;return_prog=p}, e), t, q
  | [], e, t, Call :: p -> panic c "stack underflow"

  (* This is for Return *)
  | [Clos ({def_id=i;captured=b;prog=q;})], Local (r, le), t, Return :: p when i = r.id -> [Clos ({def_id=r.called_def_id;captured= merge b r.local;prog=q;})], le, t, r.return_prog
  | [Clos ({def_id=i;captured=b;prog=q;})], Local (r, le), t, Return :: p when i <> r.id -> [Clos ({def_id=i;captured= b;prog=q;})], le, t, r.return_prog
  | [x], Local (r, le), t, Return :: p -> [x], le, t, r.return_prog
  | [], Local (r, le), t, Return :: p -> [], le, t, r.return_prog
  | [], Local (r, le), t, _ -> [], le, t, r.return_prog
  | x :: y :: s, Local (r, le), t, Return :: p -> panic c "stack underflow"
  | x :: s, Local (r, le), t, _ -> panic c "stack underflow"
  | s, Global g, t, Return :: p -> panic c "stack underflow"

  (*Debug*)
  | s, e, t, Debug m :: p -> s, e, m::t, p
  | _ -> assert false (* TODO *)

let rec eval c =
  match c with
  | (_, Global _, t, []) -> t
  | _ -> eval (eval_step c)

let rec eval_prog p = eval ([], Global [], [], p)
let interp s = Option.map eval_prog (parse_prog s)

(* MAIN *)

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

(* END OF FILE *)
