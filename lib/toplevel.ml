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

let commands = [ "grammar"; "fixity"; "relation"; "rule"; "step"; "run"; "trace"; "query"; "show" ]

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
   grammar but are one edit away from one: almost always a typo. (Other
   unknown words are fine: they are names, like locations or process
   constants.) *)
let rec pat_syms = function
  | PSym x when String.length x > 0 && is_ident_start x.[0] -> [ x ]
  | PSym _ | PInt _ | PHole | PVar _ -> []
  | PCtx (_, _, p) -> pat_syms p
  | PNode ps | PTuple ps -> List.concat_map pat_syms ps
  | PStore kvs -> List.concat_map (fun (k, v) -> pat_syms k @ pat_syms v) kvs

let near a b =
  let la = String.length a and lb = String.length b in
  if a = b || abs (la - lb) > 1 || min la lb < 2 then false
  else
    (* edit distance at most 1 *)
    let rec go i j edits =
      if edits > 1 then false
      else if i = la || j = lb then edits + (la - i) + (lb - j) <= 1
      else if a.[i] = b.[j] then go (i + 1) (j + 1) edits
      else go (i + 1) (j + 1) (edits + 1) || go (i + 1) j (edits + 1) || go i (j + 1) (edits + 1)
    in
    go 0 0 0

let warn_strays lang s from p =
  let g = lang.R.grammar in
  let known = List.filter (fun l -> is_ident_start l.[0]) (G.literals g) @ List.map fst g.G.nts in
  List.iter
    (fun x ->
      if not (List.mem x known) then
        match (List.find_opt (near x) known, find_ident s from x) with
        | Some k, Some tk -> warn tk "'%s' is not a symbol of the grammar; did you mean '%s'?" x k
        | _ -> ())
    (List.sort_uniq compare (pat_syms p))

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
    let prods = ref [] and cur = ref [] and shown = ref None in
    let finish () =
      if !cur = [] then report Err (peek s) (Printf.sprintf "empty production for '%s'" x)
      else prods := (List.rev !cur, !shown) :: !prods;
      cur := [];
      shown := None
    in
    let rec loop () =
      let tk = peek s in
      if tk.tok = EOF || is_command tk || is_nt_start s then finish ()
      else (
        (match tk.tok with
        | OP "|" -> finish ()
        | IDENT "shown" when (match (peek2 s).tok with STRING _ -> true | _ -> false) ->
            advance s;
            shown := (match (peek s).tok with STRING t -> Some t | _ -> None)
        | IDENT _ | OP _ | STRING _ | INT _ -> cur := tk :: !cur
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
        let lit tk =
          match tk.tok with
          | IDENT y | OP y | STRING y -> Some y
          | INT n -> Some (string_of_int n)
          | _ -> None
        in
        let resolve toks =
          match toks with
          | [ { tok = LBRACK; _ } ] -> Some G.PHole
          | [ { tok = IDENT y; _ } ] when known y -> Some (G.PAlias y)
          | [ { tok = IDENT y; _ } ] when List.mem y G.builtins -> Some (G.PBuiltin y)
          | [ tk ] -> Option.map (fun l -> G.PLit l) (lit tk)
          | _ ->
              let items =
                List.map
                  (fun tk ->
                    match tk.tok with
                    | IDENT y when known y -> Some (G.NT y)
                    | _ -> (
                        match lit tk with
                        | Some l -> Some (G.Lit l)
                        | None ->
                            report Err tk "'[]' must be a production on its own";
                            None))
                  toks
              in
              if List.mem None items then None else Some (G.PSeq (List.filter_map Fun.id items))
        in
        let ps =
          List.filter_map
            (fun (toks, shown) ->
              Option.map
                (fun p ->
                  (match (shown, p) with
                  | Some t, G.PSeq items -> Hashtbl.replace g.G.templates (G.skel_of items) t
                  | Some t, G.PLit l -> Hashtbl.replace g.G.lit_templates l t
                  | _ -> ());
                  (List.hd toks, p))
                (resolve toks))
            prods
        in
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

(* Relations *)

(* A relation is declared by its notation: argument slots (nonterminal
   names, or tuples of them) between symbols.

     relation (e, s) -> (e, s)
     relation (e, s) ==> (v, s)  shown "#1 \Downarrow #2"
     relation P -alpha-> P       mode in out out  shown "#1 \xrightarrow{#2} #3"
     relation v value

   By default the last argument is the output and the rest are inputs; a
   relation with one argument is a predicate (its argument is an input). *)
let parse_relation lang s : span =
  let g = lang.R.grammar in
  let start = s.pos in
  let notation = ref [] and sorts = ref [] in
  let is_arg tk =
    match tk.tok with
    | LPAREN | LBRACE -> true
    | IDENT x -> G.base_nt g x <> None
    | _ -> false
  in
  let stop tk = tk.tok = EOF || is_command tk || tk.tok = IDENT "mode" || tk.tok = IDENT "shown" in
  while not (stop (peek s)) do
    let tk = peek s in
    if is_arg tk then (
      sorts := O.parse_nud g O.Pattern s :: !sorts;
      notation := R.NArg :: !notation)
    else
      match tk.tok with
      | OP o | IDENT o ->
          advance s;
          notation := R.NSym o :: !notation
      | _ -> fail tk "unexpected '%s' in a relation's notation" (tok_to_string tk.tok)
  done;
  let notation = List.rev !notation and sorts = List.rev !sorts in
  if sorts = [] then fail (peek s) "a relation needs at least one argument, named after a nonterminal";
  if not (List.exists (function R.NSym _ -> true | R.NArg -> false) notation) then
    fail (peek s) "a relation needs a symbol, e.g. -> or ==>";
  let n = List.length sorts in
  let modes =
    if (peek s).tok = IDENT "mode" then (
      let tk = peek s in
      advance s;
      let ms = ref [] in
      let rec loop () =
        match (peek s).tok with
        | IDENT "in" -> advance s; ms := R.In :: !ms; loop ()
        | IDENT "out" -> advance s; ms := R.Out :: !ms; loop ()
        | _ -> ()
      in
      loop ();
      if List.length !ms <> n then fail tk "this relation has %d arguments, so its mode needs %d of in/out" n n;
      List.rev !ms)
    else if n = 1 then [ R.In ]
    else List.init n (fun i -> if i = n - 1 then R.Out else R.In)
  in
  let shown =
    if (peek s).tok = IDENT "shown" then (
      advance s;
      match (peek s).tok with
      | STRING t -> advance s; Some t
      | _ -> fail (peek s) "expected a LaTeX template in quotes after 'shown'")
    else None
  in
  let key = String.concat " " (List.map (function R.NArg -> "_" | R.NSym x -> x) notation) in
  List.iter
    (function R.NSym w when is_ident_start w.[0] -> g.G.words <- w :: g.G.words | _ -> ())
    notation;
  R.add_relation lang { R.key; notation; sorts; modes; shown };
  (start, s.pos)

(* Judgments *)

let at_boundary s =
  match (peek s).tok with COMMA | EOF | IDENT "if" -> true | _ -> is_command (peek s)

(* An argument is an object term; in a query, ? stands for an output to find. *)
let parse_arg lang s ~holes =
  if holes && (peek s).tok = OP "?" then (
    advance s;
    PVar ("?", None))
  else O.parse_expr lang.R.grammar O.Pattern s 0

(* Read a judgment by trying each declared relation's notation. The reading
   that ends at a boundary (a comma, 'if', the next item) wins, the longest
   if several do; otherwise the error from the attempt that got furthest is
   reported. *)
let parse_judgment ?(holes = false) lang s : R.judgment =
  let start = s.pos in
  if lang.R.relations = [] then
    fail (peek s) "no relation is declared yet; declare one first, e.g. relation (e, s) -> (e, s)";
  let attempt rel =
    s.pos <- start;
    try
      let args = ref [] in
      List.iter
        (function
          | R.NArg -> args := parse_arg lang s ~holes :: !args
          | R.NSym sym ->
              let tk = peek s in
              if O.key_of tk = Some sym then advance s
              else fail tk "expected '%s' but found '%s'" sym (tok_to_string tk.tok))
        rel.R.notation;
      if at_boundary s then Stdlib.Ok ({ R.rel; args = List.rev !args }, s.pos)
      else
        fail (peek s) "unexpected '%s' after the judgment; premises start with 'if' and are separated by commas"
          (tok_to_string (peek s).tok)
    with Error (tk, msg) -> Stdlib.Error (tk, msg)
  in
  let results = List.map attempt lang.R.relations in
  let oks = List.filter_map (function Stdlib.Ok r -> Some r | Stdlib.Error _ -> None) results in
  match List.sort (fun (_, a) (_, b) -> compare b a) oks with
  | (j, p) :: _ ->
      s.pos <- p;
      j
  | [] ->
      let errs = List.filter_map (function Stdlib.Error e -> Some e | Stdlib.Ok _ -> None) results in
      let tk, msg =
        List.fold_left
          (fun (((t : token), _) as best) (((t' : token), _) as e) -> if t'.start > t.start then e else best)
          (List.hd errs) errs
      in
      s.pos <- start;
      raise (Error (tk, msg))

let judgment_vars (j : R.judgment) m = List.concat_map pat_vars (R.pick j.rel.modes m j.args)

(* Rules *)

let parse_premise lang s bound =
  let g = lang.R.grammar in
  let save = s.pos in
  match parse_judgment lang s with
  | j -> R.Judge j
  | exception Error _ -> (
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

(* rule name judgment [if premise, premise, ...]

   Mode checking: the conclusion's inputs bind variables; each premise's
   inputs must already be bound and its outputs bind more; the conclusion's
   outputs must all be bound by the end. *)
let parse_rule lang s : rule_info =
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
  let concl = parse_judgment lang s in
  let concl_span = (concl_start, s.pos) in
  let at_end () = is_command (peek s) || (peek s).tok = EOF in
  if (peek s).tok <> IDENT "if" && not (at_end ()) then
    fail (peek s) "unexpected '%s' after the conclusion; premises start with 'if'"
      (tok_to_string (peek s).tok);
  List.iter (warn_strays lang s concl_start) concl.R.args;
  let bound = ref (judgment_vars concl R.In) in
  let premises = ref [] and prems = ref [] in
  if (peek s).tok = IDENT "if" then (
    advance s;
    let one () =
      let from = s.pos in
      let tk = peek s in
      let p = parse_premise lang s bound in
      (match p with
      | R.Judge j ->
          List.iter
            (fun x ->
              if not (List.mem x !bound) then
                fail (ident_tk s from x ~default:tk) "'%s' is used before it is bound" x)
            (judgment_vars j R.In);
          List.iter (warn_strays lang s from) j.R.args;
          bound := judgment_vars j R.Out @ !bound
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
    if not (at_end ()) then (
      let tk = peek s in
      let rel_syms =
        List.concat_map (fun r -> List.filter_map (function R.NSym x -> Some x | R.NArg -> None) r.R.notation)
          lang.R.relations
      in
      match tk.tok with
      | OP o when not (List.mem o rel_syms) && (String.contains o '>' || String.contains o '-') && String.length o > 1 ->
          fail tk "'%s' isn't the symbol of any declared relation" o
      | _ -> fail tk "unexpected '%s'; premises are separated by commas" (tok_to_string tk.tok)));
  List.iter
    (fun x ->
      if not (List.mem x !bound) then
        fail (ident_tk s concl_start x ~default:name_tk) "'%s' in the conclusion is never bound" x)
    (judgment_vars concl R.Out);
  { rule = { R.name; concl; premises = List.rev !premises };
    name_tk; concl = concl_span; prems = List.rev !prems }

(* Commands *)

let show lang t = G.to_string lang.R.grammar t

let judgment_to_string lang (rel : R.relation) (args : Term.t list) =
  let rec go items args =
    match (items, args) with
    | R.NArg :: rest, a :: args -> show lang a :: go rest args
    | R.NSym x :: rest, args -> x :: go rest args
    | _ -> []
  in
  String.concat " " (go rel.R.notation args)

let rec derivation_lines lang indent (d : R.derivation) =
  let rel = List.find (fun r -> r.R.key = d.R.rel_key) lang.R.relations in
  (indent ^ "by " ^ d.R.by ^ ": " ^ judgment_to_string lang rel d.R.args)
  :: List.concat_map (derivation_lines lang (indent ^ "  ")) d.R.premises

let stepping_relation lang tk =
  match R.default_stepping lang with
  | Some r -> r
  | None -> fail tk "no declared relation can be stepped (it needs one input and an output of the same sort)"

let run_command lang out cmd s =
  let g = lang.R.grammar in
  let cmd_tk = s.buf.(s.pos - 1) in
  let config () = O.to_term (O.parse_expr g O.Concrete s 0) in
  let step_text (st : R.step) =
    let labels = match st.R.labels with [] -> "" | ls -> " " ^ String.concat " " (List.map (show lang) ls) in
    Printf.sprintf "%s%s: %s" st.R.rule_name labels (show lang st.R.target)
  in
  match cmd with
  | "step" -> (
      let rel = stepping_relation lang cmd_tk in
      let t = config () in
      match R.successors lang rel t with
      | [] -> out "  no rule applies"
      | succs -> List.iter (fun st -> out ("  " ^ step_text st)) succs)
  | "run" ->
      let rel = stepping_relation lang cmd_tk in
      let r = R.run lang rel (config ()) in
      if r.finals = [] then out "  no final configuration (every path loops)";
      List.iter (fun t -> out ("  " ^ show lang t)) r.finals;
      if r.truncated then out (Printf.sprintf "  (stopped after %d states)" r.states)
  | "trace" ->
      let rel = stepping_relation lang cmd_tk in
      let t = config () in
      out ("     " ^ show lang t);
      let rec go t n =
        if n >= 1000 then out "  (stopped after 1000 steps)"
        else
          match R.successors lang rel t with
          | [] -> ()
          | (st :: _) as succs ->
              let note = match List.length succs with 1 -> "" | k -> Printf.sprintf "  [1 of %d]" k in
              out (Printf.sprintf "  -> %s%s" (step_text st) note);
              go st.R.target (n + 1)
      in
      go t 0
  | "query" ->
      let j = parse_judgment ~holes:true lang s in
      let ins = List.map O.to_term (R.inputs j.R.rel j.R.args) in
      let wanted = R.outputs j.R.rel j.R.args in
      let sols =
        List.filter
          (fun (outs, _) ->
            List.for_all2
              (fun p o -> match p with PVar ("?", None) -> true | _ -> Term.equal (O.to_term p) o)
              wanted outs)
          (R.query lang j.R.rel ins)
      in
      if sols = [] then out "  no solution";
      List.iter
        (fun (outs, d) ->
          out ("  " ^ String.concat ", " (List.map (show lang) outs));
          List.iter out (derivation_lines lang "    " d))
        sols;
      if lang.R.out_of_fuel then out "  (search ran out of fuel)"
  | "show" -> out ("  " ^ show lang (M.eval g [] (m_expr lang s)))
  | _ -> assert false

(* Commands are only parsed, not run, when checking. *)
let parse_command lang cmd s =
  match cmd with
  | "show" -> ignore (m_expr lang s)
  | "query" -> ignore (parse_judgment ~holes:true lang s)
  | _ -> ignore (O.parse_expr lang.R.grammar O.Concrete s 0)

(* Inside declarations, a run of operator characters is one symbol. *)
let declaring s f =
  set_munch s true;
  Fun.protect ~finally:(fun () -> set_munch s false) f

(* The grammar's operators become symbols for the rest of the file. *)
let learn_operators lang s =
  let g = lang.R.grammar in
  let is_op o = o <> "" && String.for_all is_op_char o in
  let rel_syms =
    List.concat_map (fun r -> List.filter_map (function R.NSym x -> Some x | R.NArg -> None) r.R.notation)
      lang.R.relations
  in
  add_ops s (List.filter is_op (G.literals g @ rel_syms @ List.of_seq (Hashtbl.to_seq_keys g.G.fixity)))

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
  | IRelation of span
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
  | IDENT "relation" ->
      advance s;
      let span = declaring s (fun () -> parse_relation lang s) in
      learn_operators lang s;
      Some (IRelation span)
  | IDENT "rule" ->
      advance s;
      let info = parse_rule lang s in
      R.add_rule lang info.rule;
      Some (IRule info)
  | IDENT (("step" | "run" | "trace" | "query" | "show") as cmd) ->
      let start = s.pos in
      advance s;
      on_command cmd start;
      if not (is_command (peek s) || (peek s).tok = EOF) then
        fail (peek s) "unexpected '%s' after %s" (tok_to_string (peek s).tok) cmd;
      Some ICommand
  | _ -> fail tk "expected grammar, fixity, relation, rule, step, run, trace, query or show"

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
(* Also returns each rule that parsed, with the source range of its item. *)
let check src : diag list * R.lang * (R.rule * int * int) list =
  diags := [];
  let lang = R.create () in
  let s = stream_for lang src in
  let names = Hashtbl.create 16 in
  let rules = ref [] in
  let rec loop () =
    let save = s.pos in
    match parse_item lang s ~on_command:(fun cmd _ -> parse_command lang cmd s) with
    | None -> ()
    | Some (IRule { rule; name_tk; _ }) ->
        if Hashtbl.mem names rule.R.name then
          warn name_tk "there is already a rule called %s" rule.R.name;
        Hashtbl.replace names rule.R.name ();
        rules := (rule, s.buf.(save).start, s.buf.(s.pos - 1).stop) :: !rules;
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
  (ds, lang, List.rev !rules)

(* The parts of a file, as text: grammar rows, fixity declarations, and each
   rule's name, conclusion and premises. Used to show a file as rule cards.
   Comments are dropped. *)

type rule_parts = { name : string; conclusion : string; premises : string list }
type outline = {
  grammar : string list;
  fixity : string list;
  relations : string list;
  rules : rule_parts list;
}

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
  let grammar = ref [] and fixity = ref [] and relations = ref [] and rules = ref [] in
  let rec loop () =
    match parse_item lang s ~on_command:(fun cmd _ -> parse_command lang cmd s) with
    | None -> ()
    | Some (IGrammar rows) -> grammar := List.rev_append (List.map (slice s) rows) !grammar; loop ()
    | Some (IFixity sp) -> fixity := slice s sp :: !fixity; loop ()
    | Some (IRelation sp) -> relations := slice s sp :: !relations; loop ()
    | Some (IRule { rule; concl; prems; _ }) ->
        rules :=
          { name = rule.R.name; conclusion = slice s concl; premises = List.map (slice s) prems }
          :: !rules;
        loop ()
    | Some ICommand -> loop ()
  in
  match loop () with
  | () ->
      Ok
        { grammar = List.rev !grammar; fixity = List.rev !fixity;
          relations = List.rev !relations; rules = List.rev !rules }
  | exception Error (tk, msg) -> Error (tk, msg)
