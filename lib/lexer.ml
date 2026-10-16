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

(* Every error points at the token (and so the source range) it is about. *)
exception Error of token * string


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

(* The runtime's own multi-character symbols: rule arrows, grammar
   definitions, store maplets, and the metalogic's operators. Object-language
   operators are never listed here; they are learned from the grammar. *)
let own_ops = [ "|->"; "->*"; "::="; "->"; "<="; ">="; "<>"; "!="; "&&"; "||" ]

let is_op_char c = String.contains "+-*/<>=!:;|&^~.?@#$%\\" c
let is_digit c = '0' <= c && c <= '9'
let is_ident_start c = c = '_' || ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z')
let is_ident_char c = is_ident_start c || is_digit c || c = '\''

(* Tokens are produced on demand, because what counts as one operator
   depends on the grammar read so far.

   - In [munch] mode (inside grammar and fixity declarations) a run of
     operator characters is a single symbol: that is how a grammar declares
     its operators, e.g. e ::= e <+> e.
   - Otherwise a run is split by longest match against the known symbols,
     falling back to single characters, so x:=!x+1 lexes once := is known. *)
type stream = {
  src : string;
  mutable buf : token array;
  mutable len : int;          (* tokens lexed so far *)
  mutable off : int;          (* where lexing resumes *)
  mutable line : int;
  mutable line_start : int;
  mutable ops : string list;  (* known multi-character symbols, longest first *)
  mutable munch : bool;
  mutable pos : int;          (* the parser's cursor into buf *)
}

let stream_of_string src =
  { src; buf = [||]; len = 0; off = 0; line = 1; line_start = 0;
    ops = List.sort (fun a b -> compare (String.length b) (String.length a)) own_ops;
    munch = false; pos = 0 }

let lex_one s =
  let src = s.src and n = String.length s.src in
  let starts_with str at =
    let l = String.length str in
    at + l <= n && String.sub src at l = str
  in
  let rec skip () =
    if s.off < n then
      let c = src.[s.off] in
      if c = '\n' then (
        s.off <- s.off + 1;
        s.line <- s.line + 1;
        s.line_start <- s.off;
        skip ())
      else if c = ' ' || c = '\t' || c = '\r' then (
        s.off <- s.off + 1;
        skip ())
      else if starts_with "--" s.off && not (s.off + 2 < n && is_op_char src.[s.off + 2]) then (
        while s.off < n && src.[s.off] <> '\n' do s.off <- s.off + 1 done;
        skip ())
  in
  skip ();
  let start = s.off in
  let mk tok = { tok; line = s.line; col = start - s.line_start + 1; start; stop = s.off } in
  let scan p = while s.off < n && p src.[s.off] do s.off <- s.off + 1 done in
  if start >= n then mk EOF
  else
    let c = src.[start] in
    if is_digit c then (
      scan is_digit;
      mk (INT (int_of_string (String.sub src start (s.off - start)))))
    else if is_ident_start c then (
      scan is_ident_char;
      mk (IDENT (String.sub src start (s.off - start))))
    else if is_op_char c then (
      if s.munch then scan is_op_char
      else
        s.off <-
          start
          + String.length
              (Option.value ~default:(String.make 1 c)
                 (List.find_opt (fun o -> starts_with o start) s.ops));
      mk (OP (String.sub src start (s.off - start))))
    else (
      s.off <- start + 1;
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
              (Error
                 ( { tok = EOF; line = s.line; col = start - s.line_start + 1; start; stop = start + 1 },
                   Printf.sprintf "unexpected character '%c'" c ))
      in
      mk tok)

let fill s i =
  while s.len <= i && (s.len = 0 || s.buf.(s.len - 1).tok <> EOF) do
    let tk = lex_one s in
    if s.len = Array.length s.buf then
      s.buf <- Array.append s.buf (Array.make (max 64 s.len) tk);
    s.buf.(s.len) <- tk;
    s.len <- s.len + 1
  done;
  s.buf.(min i (s.len - 1))

(* Throw away tokens lexed past the cursor, so they are lexed again under
   new rules. *)
let relex s =
  if s.len > s.pos then (
    let tk = s.buf.(s.pos) in
    s.off <- tk.start;
    s.line <- tk.line;
    s.line_start <- tk.start - tk.col + 1;
    s.len <- s.pos)

let set_munch s b =
  if s.munch <> b then (
    s.munch <- b;
    relex s)

let add_ops s ops =
  let ops = List.filter (fun o -> String.length o > 1 && not (List.mem o s.ops)) ops in
  if ops <> [] then (
    s.ops <- List.sort (fun a b -> compare (String.length b) (String.length a)) (ops @ s.ops);
    relex s)

let peek s = fill s s.pos
let peek2 s = fill s (s.pos + 1)
let advance s = if (peek s).tok <> EOF then s.pos <- s.pos + 1
let error_at (tk : token) msg = raise (Error (tk, msg))

let expect s tok =
  let tk = peek s in
  if tk.tok = tok then advance s
  else
    error_at tk
      (Printf.sprintf "expected '%s' but found '%s'" (tok_to_string tok) (tok_to_string tk.tok))

(* Is [tk] directly after the previous token, with no whitespace? *)
let adjacent s (tk : token) = s.pos > 0 && s.buf.(s.pos - 1).stop = tk.start

(* The source text of tokens [from, s.pos), with whitespace collapsed. *)
let source_since s from =
  if s.pos <= from then ""
  else
    let a = s.buf.(from).start and b = s.buf.(s.pos - 1).stop in
    String.sub s.src a (b - a)
    |> String.split_on_char '\n'
    |> List.map String.trim
    |> List.filter (fun l -> l <> "")
    |> String.concat " "
