(* A Pratt parser for object-language terms, driven by the grammar's
   productions and fixity declarations. *)

open Lexer
open Term

type mode =
  | Pattern   (* nonterminal-named identifiers are variables *)
  | Concrete  (* every identifier is a symbol *)

let key_of tk = match tk.tok with OP o -> Some o | IDENT x -> Some x | _ -> None

(* An infix operator position: a literal, or (in patterns) a variable ranging
   over an operator nonterminal, as in e1 op e2. *)
let find_led g mode s =
  match key_of (peek s) with
  | None -> None
  | Some k -> (
      match Grammar.led_form g k with
      | Some sk -> Some (PSym k, sk, Grammar.led_fixity g k)
      | None -> (
          match (mode, (peek s).tok) with
          | Pattern, IDENT x -> (
              match Grammar.base_nt g x with
              | Some o -> (
                  match Grammar.op_lits g o with
                  | Some (l :: ls) ->
                      let sk = Option.get (Grammar.led_form g l) in
                      let p =
                        List.fold_left
                          (fun p l -> min p (snd (Grammar.led_fixity g l)))
                          (snd (Grammar.led_fixity g l))
                          ls
                      in
                      Some (PVar (x, Some o), sk, (Grammar.Left, p))
                  | _ -> None)
              | None -> None)
          | _ -> None))

let rec parse_expr g mode s min_prec =
  let left = parse_nud g mode s in
  parse_led g mode s min_prec left

and parse_led g mode s min_prec left =
  match find_led g mode s with
  | Some (op, sk, (assoc, p)) when p >= min_prec ->
      advance s;
      let last = match assoc with Grammar.Right -> p | _ -> p + 1 in
      let rest = parse_items g mode s sk last in
      parse_led g mode s min_prec (PNode ((left :: op :: rest)))
  | _ -> left

(* The rest of a form: expect literals, parse subterms. The last subterm is
   parsed at [last_prec]; inner ones are delimited by literals. *)
and parse_items g mode s sk last_prec =
  let n = List.length sk in
  List.mapi
    (fun i lit ->
      match lit with
      | Some l ->
          let tk = peek s in
          if key_of tk = Some l then (
            advance s;
            PSym l)
          else
            error_at tk
              (Printf.sprintf "expected '%s' but found '%s'" l (tok_to_string tk.tok))
      | None -> parse_expr g mode s (if i = n - 1 then last_prec else 0))
    sk

and parse_nud g mode s =
  let tk = peek s in
  let form k =
    match Grammar.nud_form g k with
    | Some sk ->
        advance s;
        Some (PNode (PSym k :: parse_items g mode s sk (Grammar.nud_prec g k)))
    | None -> None
  in
  match tk.tok with
  | INT n ->
      advance s;
      PInt n
  | OP "-" when (match (peek2 s).tok with INT _ -> (peek2 s).start = tk.stop | _ -> false)
                && Grammar.nud_form g "-" = None -> (
      advance s;
      match (peek s).tok with
      | INT n ->
          advance s;
          PInt (-n)
      | _ -> assert false)
  | OP o -> (
      match form o with
      | Some p -> p
      | None -> error_at tk (Printf.sprintf "unexpected '%s'" o))
  | IDENT x -> (
      match form x with
      | Some p -> p
      | None -> (
          advance s;
          match (mode, Grammar.base_nt g x) with
          | Pattern, Some nt ->
              let nx = peek s in
              if nx.tok = LBRACK && adjacent s nx && Grammar.is_ctx_nt g nt then (
                advance s;
                let inner = parse_expr g mode s 0 in
                expect s RBRACK;
                PCtx (x, nt, inner))
              else PVar (x, Some nt)
          | _ -> PSym x))
  | LPAREN ->
      advance s;
      if (peek s).tok = RPAREN then (
        advance s;
        PTuple [])
      else
        let first = parse_expr g mode s 0 in
        if (peek s).tok = COMMA then (
          let items = ref [ first ] in
          while (peek s).tok = COMMA do
            advance s;
            items := parse_expr g mode s 0 :: !items
          done;
          expect s RPAREN;
          PTuple (List.rev !items))
        else (
          expect s RPAREN;
          first)
  | LBRACE ->
      advance s;
      let kvs = ref [] in
      if (peek s).tok <> RBRACE then (
        let entry () =
          let k = parse_expr g mode s 0 in
          expect s (OP "|->");
          let v = parse_expr g mode s 0 in
          kvs := (k, v) :: !kvs
        in
        entry ();
        while (peek s).tok = COMMA do
          advance s;
          entry ()
        done);
      expect s RBRACE;
      PStore (List.rev !kvs)
  | LBRACK when (peek2 s).tok = RBRACK ->
      advance s;
      advance s;
      PHole
  | _ -> error_at tk (Printf.sprintf "expected a term but found '%s'" (tok_to_string tk.tok))

(* Concrete terms (e.g. the program given to `run`) contain no variables. *)
let rec to_term = function
  | PInt n -> Int n
  | PSym x -> Sym x
  | PHole -> Hole
  | PNode ps -> Node (List.map to_term ps)
  | PTuple ps -> Tuple (List.map to_term ps)
  | PStore kvs -> mk_store (List.map (fun (k, v) -> (to_term k, to_term v)) kvs)
  | PVar (x, _) | PCtx (x, _, _) -> Sym x
