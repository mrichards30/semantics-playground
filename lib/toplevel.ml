(* The text front end: grammar, fixity, rule declarations and commands.

     grammar  e ::= v | e + e | ...     nonterminals and productions
     fixity   infixl 4 +  prefix 5 !    operator precedence
     rule     name lhs -> rhs if p1, p2, ...
     step     config                    print every successor
     run      config                    print every final configuration
     trace    config                    print one path, noting branches
     show     meta-expr                 evaluate a metalogic expression *)

open Lexer
open Term
module G = Grammar
module O = Objparse
module M = Meta
module R = Rules

let commands = [ "grammar"; "fixity"; "rule"; "step"; "run"; "trace"; "show" ]

let reserved =
  commands @ [ "if"; "then"; "else"; "in"; "where"; "and"; "not"; "true"; "false"; "arb" ]

let is_command tk = match tk.tok with IDENT x -> List.mem x commands | _ -> false
let fail tk fmt = Printf.ksprintf (error_at tk) fmt

(* Metalogic expressions *)

let rec m_expr lang s = m_where lang s

and m_where lang s =
  let body = m_or lang s in
  if (peek s).tok = IDENT "where" then (
    advance s;
    let binds = ref [] in
    let bind () =
      match (peek s).tok with
      | IDENT x when not (List.mem x reserved) ->
          advance s;
          expect s (OP "=");
          binds := (x, m_or lang s) :: !binds
      | _ -> fail (peek s) "expected a binding after 'where'"
    in
    bind ();
    while (peek s).tok = IDENT "and" do
      advance s;
      bind ()
    done;
    M.Where (body, List.rev !binds))
  else body

and m_left lang s ops next =
  let l = ref (next lang s) in
  let rec loop () =
    match (peek s).tok with
    | OP o when List.mem o ops ->
        advance s;
        l := M.Bin (o, !l, next lang s);
        loop ()
    | _ -> ()
  in
  loop ();
  !l

and m_or lang s = m_left lang s [ "||" ] m_and
and m_and lang s = m_left lang s [ "&&" ] m_not

and m_not lang s =
  if (peek s).tok = IDENT "not" then (
    advance s;
    M.Not (m_not lang s))
  else m_cmp lang s

and m_cmp lang s =
  let first = m_add lang s in
  let rest = ref [] in
  let rec loop () =
    match (peek s).tok with
    | OP (("=" | "<>" | "!=" | "<" | "<=" | ">" | ">=") as o) ->
        advance s;
        rest := (o, m_add lang s) :: !rest;
        loop ()
    | IDENT "in" ->
        advance s;
        rest := ("in", m_add lang s) :: !rest;
        loop ()
    | _ -> ()
  in
  loop ();
  if !rest = [] then first else M.Chain (first, List.rev !rest)

and m_add lang s = m_left lang s [ "+"; "-" ] m_mul
and m_mul lang s = m_left lang s [ "*"; "/" ] m_unary

and m_unary lang s =
  if (peek s).tok = OP "-" then (
    advance s;
    M.Neg (m_unary lang s))
  else m_app lang s

and atom_start tk =
  match tk.tok with
  | INT _ | LPAREN | LBRACE | BACKTICK -> true
  | IDENT x -> not (List.mem x reserved) || List.mem x [ "true"; "false"; "arb" ]
  | _ -> false

and m_app lang s =
  let f = ref (m_postfix lang s) in
  while atom_start (peek s) do
    f := M.App (!f, m_postfix lang s)
  done;
  !f

and m_postfix lang s =
  let a = ref (m_atom lang s) in
  while (peek s).tok = LBRACK do
    advance s;
    let k = m_expr lang s in
    expect s (OP "|->");
    let v = m_expr lang s in
    expect s RBRACK;
    a := M.Update (!a, k, v)
  done;
  !a

and m_atom lang s =
  let tk = peek s in
  match tk.tok with
  | INT n ->
      advance s;
      M.Const (Int n)
  | IDENT "true" -> advance s; M.Const tt
  | IDENT "false" -> advance s; M.Const ff
  | IDENT "arb" -> advance s; M.Const Arb
  | IDENT "if" ->
      advance s;
      let c = m_expr lang s in
      expect s (IDENT "then");
      let a = m_expr lang s in
      expect s (IDENT "else");
      M.If (c, a, m_expr lang s)
  | IDENT x when not (List.mem x reserved) ->
      advance s;
      M.Var x
  | BACKTICK ->
      advance s;
      let p = O.parse_expr lang.R.grammar O.Pattern s 0 in
      expect s BACKTICK;
      M.Quote p
  | LPAREN ->
      advance s;
      if (peek s).tok = RPAREN then (advance s; M.TupleLit [])
      else
        let first = m_expr lang s in
        if (peek s).tok = COMMA then (
          let items = ref [ first ] in
          while (peek s).tok = COMMA do
            advance s;
            items := m_expr lang s :: !items
          done;
          expect s RPAREN;
          M.TupleLit (List.rev !items))
        else (
          expect s RPAREN;
          first)
  | LBRACE ->
      advance s;
      if (peek s).tok = RBRACE then (advance s; M.SetLit [])
      else
        let first = m_expr lang s in
        if (peek s).tok = OP "|->" then (
          advance s;
          let kvs = ref [ (first, m_expr lang s) ] in
          while (peek s).tok = COMMA do
            advance s;
            let k = m_expr lang s in
            expect s (OP "|->");
            kvs := (k, m_expr lang s) :: !kvs
          done;
          expect s RBRACE;
          M.StoreLit (List.rev !kvs))
        else
          let items = ref [ first ] in
          while (peek s).tok = COMMA do
            advance s;
            items := m_expr lang s :: !items
          done;
          expect s RBRACE;
          M.SetLit (List.rev !items)
  | _ -> fail tk "expected an expression but found '%s'" (tok_to_string tk.tok)

(* Grammar blocks *)

let parse_grammar lang s =
  let g = lang.R.grammar in
  let is_nt_start () =
    match ((peek s).tok, (peek2 s).tok) with IDENT _, OP "::=" -> true | _ -> false
  in
  if not (is_nt_start ()) then fail (peek s) "expected 'nonterminal ::= ...'";
  let raw = ref [] in
  while is_nt_start () do
    let x = match (peek s).tok with IDENT x -> x | _ -> assert false in
    let tk_x = peek s in
    if List.mem x G.builtins then fail tk_x "'%s' is a built-in set" x;
    advance s;
    advance s;
    let prods = ref [] and cur = ref [] in
    let finish () =
      if !cur = [] then fail (peek s) "empty production for '%s'" x;
      prods := List.rev !cur :: !prods;
      cur := []
    in
    let rec loop () =
      let tk = peek s in
      if tk.tok = EOF || is_command tk || is_nt_start () then finish ()
      else (
        (match tk.tok with
        | OP "|" -> finish ()
        | IDENT _ | OP _ -> cur := tk :: !cur
        | LBRACK when (peek2 s).tok = RBRACK ->
            cur := tk :: !cur;
            advance s
        | _ -> fail tk "unexpected '%s' in a production" (tok_to_string tk.tok));
        advance s;
        loop ())
    in
    loop ();
    raw := (x, List.rev !prods) :: !raw
  done;
  let raw = List.rev !raw in
  (* Resolve names only once the whole block is read, so nonterminals can be
     used before they are defined. *)
  let known y = G.is_nt g y || List.mem_assoc y raw in
  List.iter
    (fun (x, prods) ->
      let resolve toks =
        match toks with
        | [ { tok = LBRACK; _ } ] -> G.PHole
        | [ { tok = IDENT y; _ } ] when known y -> G.PAlias y
        | [ { tok = IDENT y; _ } ] when List.mem y G.builtins -> G.PBuiltin y
        | [ { tok = IDENT y | OP y; _ } ] -> G.PLit y
        | _ ->
            G.PSeq
              (List.map
                 (fun tk ->
                   match tk.tok with
                   | IDENT y when known y -> G.NT y
                   | IDENT y | OP y -> G.Lit y
                   | _ -> fail tk "'[]' must be a production on its own")
                 toks)
      in
      G.add_prods g x (List.map resolve prods))
    raw

let parse_fixity lang s =
  let g = lang.R.grammar in
  let assoc_of = function
    | IDENT "infixl" -> Some G.Left
    | IDENT "infixr" -> Some G.Right
    | IDENT "infix" -> Some G.Non
    | IDENT "prefix" -> Some G.Prefix
    | _ -> None
  in
  if assoc_of (peek s).tok = None then fail (peek s) "expected infixl, infixr, infix or prefix";
  while assoc_of (peek s).tok <> None do
    let a = Option.get (assoc_of (peek s).tok) in
    advance s;
    let p = match (peek s).tok with INT p -> advance s; p | _ -> fail (peek s) "expected a precedence" in
    let any = ref false in
    let rec ops () =
      let tk = peek s in
      match tk.tok with
      | (OP o | IDENT o) when assoc_of tk.tok = None && not (is_command tk) ->
          Hashtbl.replace g.G.fixity o (a, p);
          any := true;
          advance s;
          ops ()
      | _ -> ()
    in
    ops ();
    if not !any then fail (peek s) "expected operators after the precedence"
  done

(* Rules *)

let parse_premise lang s bound =
  let g = lang.R.grammar in
  let save = s.pos in
  let obj = try Some (O.parse_expr g O.Pattern s 0) with Error _ -> None in
  match (obj, (peek s).tok) with
  | Some a, OP (("->" | "->*") as arrow) ->
      advance s;
      let b = O.parse_expr g O.Pattern s 0 in
      if arrow = "->" then R.Step (a, b) else R.Steps (a, b)
  | _ -> (
      s.pos <- save;
      let tk = peek s in
      match m_expr lang s with
      | M.Chain (M.Var x, [ ("=", e) ]) when not (List.mem x !bound) -> (
          match G.base_nt g x with
          | Some nt -> R.Bind (x, Some nt, e)
          | None ->
              fail tk
                "'%s' is not named after a nonterminal, so it can't be a pattern variable" x)
      | e -> R.Cond e)

let parse_rule lang s =
  let g = lang.R.grammar in
  let name_tk = peek s in
  let name =
    match name_tk.tok with
    | IDENT x ->
        advance s;
        let buf = Buffer.create 8 in
        Buffer.add_string buf x;
        (* rule names may contain operators: op+, op>= *)
        let rec glue () =
          let tk = peek s in
          match tk.tok with
          | (OP o | IDENT o) when adjacent s tk ->
              Buffer.add_string buf o;
              advance s;
              glue ()
          | INT n when adjacent s tk ->
              Buffer.add_string buf (string_of_int n);
              advance s;
              glue ()
          | _ -> ()
        in
        glue ();
        Buffer.contents buf
    | _ -> fail name_tk "expected a rule name"
  in
  let lhs = O.parse_expr g O.Pattern s 0 in
  expect s (OP "->");
  let rhs = O.parse_expr g O.Pattern s 0 in
  let bound = ref (pat_vars lhs) in
  let premises = ref [] in
  if (peek s).tok = IDENT "if" then (
    advance s;
    let one () =
      let tk = peek s in
      let p = parse_premise lang s bound in
      (match p with
      | R.Step (a, b) | R.Steps (a, b) ->
          List.iter
            (fun x -> if not (List.mem x !bound) then fail tk "'%s' is used before it is bound" x)
            (pat_vars a);
          bound := pat_vars b @ !bound
      | R.Bind (x, _, _) -> bound := x :: !bound
      | R.Cond _ -> ());
      premises := p :: !premises
    in
    one ();
    while (peek s).tok = COMMA do
      advance s;
      one ()
    done);
  List.iter
    (fun x ->
      if not (List.mem x !bound) then
        fail name_tk "rule %s: '%s' in the conclusion is never bound" name x)
    (pat_vars rhs);
  { R.name; lhs; rhs; premises = List.rev !premises }

(* Commands *)

let show lang t = G.to_string lang.R.grammar t

let run_command lang out cmd s =
  let g = lang.R.grammar in
  let config () = O.to_term (O.parse_expr g O.Concrete s 0) in
  match cmd with
  | "step" -> (
      let t = config () in
      match R.successors lang t with
      | [] -> out "  no rule applies"
      | succs -> List.iter (fun (name, _, t') -> out (Printf.sprintf "  %s: %s" name (show lang t'))) succs)
  | "run" ->
      let r = R.run lang (config ()) in
      if r.finals = [] then out "  no final configuration (every path loops)";
      List.iter (fun t -> out ("  " ^ show lang t)) r.finals;
      if r.truncated then out (Printf.sprintf "  (stopped after %d states)" r.states)
  | "trace" ->
      let t = config () in
      out ("     " ^ show lang t);
      let rec go t n =
        if n >= 1000 then out "  (stopped after 1000 steps)"
        else
          match R.successors lang t with
          | [] -> ()
          | ((name, _, t') :: _) as succs ->
              let note =
                match List.length succs with 1 -> "" | k -> Printf.sprintf "  [1 of %d]" k
              in
              out (Printf.sprintf "  -> %s: %s%s" name (show lang t') note);
              go t' (n + 1)
      in
      go t 0
  | "show" -> out ("  " ^ show lang (M.eval g [] (m_expr lang s)))
  | _ -> assert false

(* Inside declarations, a run of operator characters is one symbol. *)
let declaring s f =
  set_munch s true;
  Fun.protect ~finally:(fun () -> set_munch s false) f

(* The grammar's operators become symbols for the rest of the file. *)
let learn_operators lang s =
  let g = lang.R.grammar in
  let is_op o = o <> "" && String.for_all is_op_char o in
  add_ops s (List.filter is_op (G.literals g @ List.of_seq (Hashtbl.to_seq_keys g.G.fixity)))

(* A stream that already knows the operators of the grammar loaded so far. *)
let stream_for lang src =
  let s = stream_of_string src in
  learn_operators lang s;
  s

(* Parse a single concrete configuration, e.g. (x := 1, {x |-> 0}). *)
let parse_config lang src =
  let s = stream_for lang src in
  let t = O.to_term (O.parse_expr lang.R.grammar O.Concrete s 0) in
  if (peek s).tok <> EOF then fail (peek s) "unexpected '%s'" (tok_to_string (peek s).tok);
  t

let run_string ?(out = print_endline) lang src =
  let s = stream_for lang src in
  let rec loop () =
    let tk = peek s in
    match tk.tok with
    | EOF -> ()
    | IDENT "grammar" ->
        advance s;
        declaring s (fun () -> parse_grammar lang s);
        learn_operators lang s;
        loop ()
    | IDENT "fixity" ->
        advance s;
        declaring s (fun () -> parse_fixity lang s);
        learn_operators lang s;
        loop ()
    | IDENT "rule" ->
        advance s;
        R.add_rule lang (parse_rule lang s);
        loop ()
    | IDENT (("step" | "run" | "trace" | "show") as cmd) ->
        let start = s.pos in
        advance s;
        (* run first, so the echoed text is exactly what was parsed *)
        let buf = ref [] in
        let out' l = buf := l :: !buf in
        run_command lang out' cmd s;
        out (source_since s start);
        List.iter out (List.rev !buf);
        if not (is_command (peek s) || (peek s).tok = EOF) then
          fail (peek s) "unexpected '%s' after %s" (tok_to_string (peek s).tok) cmd;
        loop ()
    | _ -> fail tk "expected grammar, fixity, rule, step, run, trace or show"
  in
  loop ()
