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

(* Application is only dom s and s(l): general juxtaposition would swallow
   the next premise when a comma is missing. *)
and m_app lang s =
  let f = ref (m_postfix lang s) in
  let more () =
    match !f with
    | M.Var "dom" -> atom_start (peek s)
    | _ -> (peek s).tok = LPAREN && adjacent s (peek s)
  in
  while more () do
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

(* Diagnostics. Errors abandon the item (grammar row, rule, command) they
   are in; warnings never stop anything. Both are collected by [check]. *)

type severity = Err | Warning

type diag = {
  severity : severity;
  start : int;   (* source offsets, [start, stop) *)
  stop : int;
  line : int;
  col : int;
  message : string;
}

let diags : diag list ref = ref []

let report severity (tk : token) message =
  diags := { severity; start = tk.start; stop = tk.stop; line = tk.line; col = tk.col; message } :: !diags

let warn tk fmt = Printf.ksprintf (report Warning tk) fmt

(* Spans of tokens [i, j), so the UI can recover the text of each part. *)
type span = int * int

(* The first token in [from, s.pos) that is the identifier [x]. *)
let find_ident s from x =
  let rec go i =
    if i >= s.pos then None else if s.buf.(i).tok = IDENT x then Some s.buf.(i) else go (i + 1)
  in
  go from

let ident_tk s from x ~default = Option.value ~default (find_ident s from x)

(* Identifiers in a pattern that are neither nonterminals nor symbols of the
   grammar: almost always a typo for a nonterminal. *)
let rec stray_syms g = function
  | PSym x when String.length x > 0 && is_ident_start x.[0] ->
      if List.mem x (G.literals g) || G.is_nt g x then [] else [ x ]
  | PSym _ | PInt _ | PHole | PVar _ -> []
  | PCtx (_, _, p) -> stray_syms g p
  | PNode ps | PTuple ps -> List.concat_map (stray_syms g) ps
  | PStore kvs -> List.concat_map (fun (k, v) -> stray_syms g k @ stray_syms g v) kvs

let warn_strays lang s from p =
  List.iter
    (fun x ->
      match find_ident s from x with
      | Some tk ->
          warn tk "'%s' is not a nonterminal or a symbol of the grammar, so it only matches the symbol %s" x x
      | None -> ())
    (List.sort_uniq compare (stray_syms lang.R.grammar p))

(* Variables a metalogic expression refers to. *)
let rec meta_vars = function
  | M.Const _ -> []
  | M.Var x -> [ x ]
  | M.Quote p -> pat_vars p
  | M.Bin (_, a, b) | M.App (a, b) -> meta_vars a @ meta_vars b
  | M.Chain (a, rest) -> meta_vars a @ List.concat_map (fun (_, b) -> meta_vars b) rest
  | M.Not a | M.Neg a -> meta_vars a
  | M.If (a, b, c) | M.Update (a, b, c) -> meta_vars a @ meta_vars b @ meta_vars c
  | M.SetLit es | M.TupleLit es -> List.concat_map meta_vars es
  | M.StoreLit kvs -> List.concat_map (fun (k, v) -> meta_vars k @ meta_vars v) kvs
  | M.Where (body, binds) ->
      let local = List.map fst binds in
      List.filter
        (fun x -> not (List.mem x local))
        (meta_vars body @ List.concat_map (fun (_, e) -> meta_vars e) binds)

let warn_unbound_meta lang s from bound e =
  let g = lang.R.grammar in
  List.iter
    (fun x ->
      if not (List.mem x bound || G.is_nt g x || List.mem x G.builtins || x = "dom") then
        match find_ident s from x with
        | Some tk -> warn tk "'%s' is not bound here, so it is arb" x
        | None -> ())
    (List.sort_uniq compare (meta_vars e))

(* Grammar blocks *)

let is_nt_start s =
  match ((peek s).tok, (peek2 s).tok) with IDENT _, OP "::=" -> true | _ -> false

(* Returns the span of each nonterminal definition. Bad tokens inside a
   production are reported and skipped, so one typo doesn't lose the rest of
   the grammar. *)
let parse_grammar lang s : span list =
  let g = lang.R.grammar in
  if not (is_nt_start s) then fail (peek s) "expected 'nonterminal ::= ...'";
  let raw = ref [] and rows = ref [] in
  while is_nt_start s do
    let row_start = s.pos in
    let tk_x = peek s in
    let x = match tk_x.tok with IDENT x -> x | _ -> assert false in
    advance s;
    advance s;
    let prods = ref [] and cur = ref [] in
    let finish () =
      if !cur = [] then report Err (peek s) (Printf.sprintf "empty production for '%s'" x)
      else prods := List.rev !cur :: !prods;
      cur := []
    in
    let rec loop () =
      let tk = peek s in
      if tk.tok = EOF || is_command tk || is_nt_start s then finish ()
      else (
        (match tk.tok with
        | OP "|" -> finish ()
        | IDENT _ | OP _ -> cur := tk :: !cur
        | LBRACK when (peek2 s).tok = RBRACK ->
            cur := tk :: !cur;
            advance s
        | _ ->
            report Err tk (Printf.sprintf "unexpected '%s' in a production" (tok_to_string tk.tok)));
        advance s;
        loop ())
    in
    loop ();
    rows := (row_start, s.pos) :: !rows;
    if List.mem x G.builtins then report Err tk_x (Printf.sprintf "'%s' is a built-in set" x)
    else raw := (x, List.rev !prods) :: !raw
  done;
  let raw = List.rev !raw in
  (* Resolve names only once the whole block is read, so nonterminals can be
     used before they are defined. *)
  let known y = G.is_nt g y || List.mem_assoc y raw in
  let resolved =
    List.map
      (fun (x, prods) ->
        let resolve toks =
          match toks with
          | [ { tok = LBRACK; _ } ] -> Some G.PHole
          | [ { tok = IDENT y; _ } ] when known y -> Some (G.PAlias y)
          | [ { tok = IDENT y; _ } ] when List.mem y G.builtins -> Some (G.PBuiltin y)
          | [ { tok = IDENT y | OP y; _ } ] -> Some (G.PLit y)
          | _ ->
              let items =
                List.map
                  (fun tk ->
                    match tk.tok with
                    | IDENT y when known y -> Some (G.NT y)
                    | IDENT y | OP y -> Some (G.Lit y)
                    | _ ->
                        report Err tk "'[]' must be a production on its own";
                        None)
                  toks
              in
              if List.mem None items then None else Some (G.PSeq (List.filter_map Fun.id items))
        in
        let ps = List.filter_map (fun toks -> Option.map (fun p -> (List.hd toks, p)) (resolve toks)) prods in
        G.add_prods g x (List.map snd ps);
        ps)
      raw
  in
  (* Productions the parser can't use. *)
  let rec adjacent_nts = function
    | G.NT _ :: (G.NT _ :: _ as rest) -> true || adjacent_nts rest
    | _ :: rest -> adjacent_nts rest
    | [] -> false
  in
  List.iter
    (List.iter (fun (tk, p) ->
         match p with
         | G.PSeq (G.NT _ :: G.NT o :: rest) when G.op_lits g o <> None ->
             if adjacent_nts rest then
               warn tk "two subterms in a row can't be parsed; put a symbol between them"
         | G.PSeq items when adjacent_nts items ->
             warn tk "two subterms in a row can't be parsed; put a symbol between them"
         | G.PSeq (G.Lit k :: rest) when G.nud_form g k <> Some (G.skel_of rest) ->
             warn tk "another production already starts with '%s'; only that one is used for parsing" k
         | G.PSeq (G.NT _ :: G.Lit k :: rest) when G.led_form g k <> Some (G.skel_of rest) ->
             warn tk "another production already uses '%s' after a subterm; only that one is used for parsing" k
         | _ -> ()))
    resolved;
  List.rev !rows

let parse_fixity lang s : span =
  let g = lang.R.grammar in
  let start = s.pos in
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
          if g.G.nts <> [] && not (List.mem o (G.literals g)) then
            warn tk "'%s' is not a symbol of the grammar" o;
          Hashtbl.replace g.G.fixity o (a, p);
          any := true;
          advance s;
          ops ()
      | _ -> ()
    in
    ops ();
    if not !any then fail (peek s) "expected operators after the precedence"
  done;
  (start, s.pos)

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

type rule_info = { rule : R.rule; name_tk : token; concl : span; prems : span list }

let parse_rule lang s : rule_info =
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
  let concl_start = s.pos in
  let lhs = O.parse_expr g O.Pattern s 0 in
  expect s (OP "->");
  let rhs_start = s.pos in
  let rhs = O.parse_expr g O.Pattern s 0 in
  let concl = (concl_start, s.pos) in
  let at_end () = is_command (peek s) || (peek s).tok = EOF in
  if (peek s).tok <> IDENT "if" && not (at_end ()) then
    fail (peek s) "unexpected '%s' after the conclusion; premises start with 'if'"
      (tok_to_string (peek s).tok);
  warn_strays lang s concl_start lhs;
  warn_strays lang s rhs_start rhs;
  let bound = ref (pat_vars lhs) in
  let premises = ref [] and prems = ref [] in
  if (peek s).tok = IDENT "if" then (
    advance s;
    let one () =
      let from = s.pos in
      let tk = peek s in
      let p = parse_premise lang s bound in
      (match p with
      | R.Step (a, b) | R.Steps (a, b) ->
          List.iter
            (fun x ->
              if not (List.mem x !bound) then
                fail (ident_tk s from x ~default:tk) "'%s' is used before it is bound" x)
            (pat_vars a);
          warn_strays lang s from a;
          warn_strays lang s from b;
          bound := pat_vars b @ !bound
      | R.Bind (x, _, e) ->
          warn_unbound_meta lang s from !bound e;
          bound := x :: !bound
      | R.Cond e -> warn_unbound_meta lang s from !bound e);
      premises := p :: !premises;
      prems := (from, s.pos) :: !prems
    in
    one ();
    while (peek s).tok = COMMA do
      advance s;
      one ()
    done;
    if not (at_end ()) then
      fail (peek s) "unexpected '%s'; premises are separated by commas"
        (tok_to_string (peek s).tok));
  List.iter
    (fun x ->
      if not (List.mem x !bound) then
        fail (ident_tk s rhs_start x ~default:name_tk) "'%s' in the conclusion is never bound" x)
    (pat_vars rhs);
  { rule = { R.name; lhs; rhs; premises = List.rev !premises };
    name_tk; concl; prems = List.rev !prems }

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

(* Commands are only parsed, not run, when checking. *)
let parse_command lang cmd s =
  if cmd = "show" then ignore (m_expr lang s)
  else ignore (O.parse_expr lang.R.grammar O.Concrete s 0)

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

(* One top-level item. *)

type item =
  | IGrammar of span list
  | IFixity of span
  | IRule of rule_info
  | ICommand

let parse_item lang s ~on_command =
  let tk = peek s in
  match tk.tok with
  | EOF -> None
  | IDENT "grammar" ->
      advance s;
      let rows = declaring s (fun () -> parse_grammar lang s) in
      learn_operators lang s;
      Some (IGrammar rows)
  | IDENT "fixity" ->
      advance s;
      let span = declaring s (fun () -> parse_fixity lang s) in
      learn_operators lang s;
      Some (IFixity span)
  | IDENT "rule" ->
      advance s;
      let info = parse_rule lang s in
      R.add_rule lang info.rule;
      Some (IRule info)
  | IDENT (("step" | "run" | "trace" | "show") as cmd) ->
      let start = s.pos in
      advance s;
      on_command cmd start;
      if not (is_command (peek s) || (peek s).tok = EOF) then
        fail (peek s) "unexpected '%s' after %s" (tok_to_string (peek s).tok) cmd;
      Some ICommand
  | _ -> fail tk "expected grammar, fixity, rule, step, run, trace or show"

(* Run a file: declarations take effect, commands print their results. The
   first error stops everything. *)
let run_string ?(out = print_endline) lang src =
  let s = stream_for lang src in
  let on_command cmd start =
    (* run first, so the echoed text is exactly what was parsed *)
    let buf = ref [] in
    run_command lang (fun l -> buf := l :: !buf) cmd s;
    out (source_since s start);
    List.iter out (List.rev !buf)
  in
  while parse_item lang s ~on_command <> None do () done

(* Check a file without running it: every error and warning, in source
   order. After an error, checking resumes at the next top-level item. *)
let check src : diag list * R.lang =
  diags := [];
  let lang = R.create () in
  let s = stream_for lang src in
  let names = Hashtbl.create 16 in
  let rec loop () =
    let save = s.pos in
    match parse_item lang s ~on_command:(fun cmd _ -> parse_command lang cmd s) with
    | None -> ()
    | Some (IRule { rule; name_tk; _ }) ->
        if Hashtbl.mem names rule.R.name then
          warn name_tk "there is already a rule called %s" rule.R.name;
        Hashtbl.replace names rule.R.name ();
        loop ()
    | Some _ -> loop ()
    | exception Error (tk, msg) ->
        report Err tk msg;
        if s.pos = save then advance s;
        while not (is_command (peek s) || (peek s).tok = EOF) do advance s done;
        loop ()
  in
  loop ();
  let ds = List.sort_uniq (fun a b -> compare (a.start, a.stop, a.message) (b.start, b.stop, b.message)) !diags in
  diags := [];
  (ds, lang)

(* The parts of a file, as text: grammar rows, fixity declarations, and each
   rule's name, conclusion and premises. Used to show a file as rule cards.
   Comments are dropped. *)

type rule_parts = { name : string; conclusion : string; premises : string list }
type outline = { grammar : string list; fixity : string list; rules : rule_parts list }

let slice s ((i, j) : span) =
  if j <= i then ""
  else
    let a = s.buf.(i).start and b = s.buf.(j - 1).stop in
    let strip_comment l =
      let n = String.length l in
      let rec go k =
        if k + 1 >= n then l
        else if l.[k] = '-' && l.[k + 1] = '-' && not (k + 2 < n && is_op_char l.[k + 2]) then
          String.sub l 0 k
        else go (k + 1)
      in
      go 0
    in
    String.sub s.src a (b - a)
    |> String.split_on_char '\n'
    |> List.map (fun l -> String.trim (strip_comment l))
    |> List.filter (fun l -> l <> "")
    |> String.concat " "

let outline src =
  let lang = R.create () in
  let s = stream_for lang src in
  let grammar = ref [] and fixity = ref [] and rules = ref [] in
  let rec loop () =
    match parse_item lang s ~on_command:(fun cmd _ -> parse_command lang cmd s) with
    | None -> ()
    | Some (IGrammar rows) -> grammar := List.rev_append (List.map (slice s) rows) !grammar; loop ()
    | Some (IFixity sp) -> fixity := slice s sp :: !fixity; loop ()
    | Some (IRule { rule; concl; prems; _ }) ->
        rules :=
          { name = rule.R.name; conclusion = slice s concl; premises = List.map (slice s) prems }
          :: !rules;
        loop ()
    | Some ICommand -> loop ()
  in
  match loop () with
  | () -> Ok { grammar = List.rev !grammar; fixity = List.rev !fixity; rules = List.rev !rules }
  | exception Error (tk, msg) -> Error (tk, msg)
