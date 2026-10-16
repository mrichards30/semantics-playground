type tok =
  | INT of int
  | IDENT of string
  | OP of string
  | LPAREN
  | RPAREN
  | LBRACK
  | RBRACK
  | LBRACE
  | RBRACE
  | COMMA
  | BACKTICK
  | EOF

type token = { tok : tok; line : int; col : int; start : int; stop : int }

exception Error of int * int * string

let tok_to_string = function
  | INT n -> string_of_int n
  | IDENT x -> x
  | OP o -> o
  | LPAREN -> "("
  | RPAREN -> ")"
  | LBRACK -> "["
  | RBRACK -> "]"
  | LBRACE -> "{"
  | RBRACE -> "}"
  | COMMA -> ","
  | BACKTICK -> "`"
  | EOF -> "end of input"

(* Multi-character operators, longest first. Any other run of operator
   characters is split into single characters. *)
let multi_ops =
  [ "|->"; "->*"; "::="; "==>"; "-->"; "<=>";
    "->"; ":="; "::"; ">="; "<="; "=="; "!="; "<>"; "&&"; "||"; "=>"; "<-";
    "|>"; "<|"; "++"; "**"; "~>" ]

let is_op_char c = String.contains "+-*/<>=!:;|&^~.?@#$%\\" c
let is_digit c = '0' <= c && c <= '9'
let is_ident_start c = c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_ident_char c = is_ident_start c || is_digit c || c = '\''

let tokenize (src : string) : token array =
  let n = String.length src in
  let toks = ref [] in
  let line = ref 1 and line_start = ref 0 in
  let i = ref 0 in
  let emit tok start stop =
    toks := { tok; line = !line; col = start - !line_start + 1; start; stop } :: !toks
  in
  let starts_with s at =
    let l = String.length s in
    at + l <= n && String.sub src at l = s
  in
  while !i < n do
    let c = src.[!i] in
    if c = '\n' then (
      incr i;
      incr line;
      line_start := !i)
    else if c = ' ' || c = '\t' || c = '\r' then incr i
    else if starts_with "--" !i && not (starts_with "-->" !i) then
      while !i < n && src.[!i] <> '\n' do incr i done
    else if is_digit c then (
      let s = !i in
      while !i < n && is_digit src.[!i] do incr i done;
      emit (INT (int_of_string (String.sub src s (!i - s)))) s !i)
    else if is_ident_start c then (
      let s = !i in
      while !i < n && is_ident_char src.[!i] do incr i done;
      emit (IDENT (String.sub src s (!i - s))) s !i)
    else if is_op_char c then (
      let s = !i in
      let op =
        match List.find_opt (fun o -> starts_with o s) multi_ops with
        | Some o -> o
        | None -> String.make 1 c
      in
      i := s + String.length op;
      emit (OP op) s !i)
    else
      let s = !i in
      incr i;
      let tok =
        match c with
        | '(' -> LPAREN
        | ')' -> RPAREN
        | '[' -> LBRACK
        | ']' -> RBRACK
        | '{' -> LBRACE
        | '}' -> RBRACE
        | ',' -> COMMA
        | '`' -> BACKTICK
        | _ ->
            raise
              (Error (!line, s - !line_start + 1, Printf.sprintf "unexpected character '%c'" c))
      in
      emit tok s !i
  done;
  emit EOF n n;
  Array.of_list (List.rev !toks)

(* A cursor over the token array, shared by all the parsers. *)
type stream = { src : string; toks : token array; mutable pos : int }

let stream_of_string src = { src; toks = tokenize src; pos = 0 }
let peek s = s.toks.(s.pos)
let peek2 s = s.toks.(min (s.pos + 1) (Array.length s.toks - 1))
let advance s = if (peek s).tok <> EOF then s.pos <- s.pos + 1
let error_at tk msg = raise (Error (tk.line, tk.col, msg))

let expect s tok =
  let tk = peek s in
  if tk.tok = tok then advance s
  else
    error_at tk
      (Printf.sprintf "expected '%s' but found '%s'" (tok_to_string tok) (tok_to_string tk.tok))

(* Is [tk] directly after the previous token, with no whitespace? *)
let adjacent s tk = s.pos > 0 && s.toks.(s.pos - 1).stop = tk.start

(* The source text of tokens [from, s.pos), with whitespace collapsed. *)
let source_since s from =
  if s.pos <= from then ""
  else
    let a = s.toks.(from).start and b = s.toks.(s.pos - 1).stop in
    String.sub s.src a (b - a)
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")
    |> String.concat " "
