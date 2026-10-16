(* The metalogic: side conditions like n = n1 + n2 and l in dom s.
   Total: anything ill-defined evaluates to arb rather than failing. *)

open Term

type expr =
  | Const of Term.t
  | Var of string
  | Quote of Term.pat                      (* `object term` *)
  | Bin of string * expr * expr            (* + - * / && || *)
  | Chain of expr * (string * expr) list   (* a < b <= c, x in S *)
  | Not of expr
  | Neg of expr
  | If of expr * expr * expr
  | App of expr * expr                     (* dom s, s(l) *)
  | Update of expr * expr * expr           (* s[k |-> v] *)
  | SetLit of expr list
  | StoreLit of (expr * expr) list
  | TupleLit of expr list
  | Where of expr * (string * expr) list   (* lazy, bindings in any order *)

let as_bool = function Sym "true" -> Some true | Sym "false" -> Some false | _ -> None

let arith op a b =
  match (op, a, b) with
  | "+", Int x, Int y -> Int (x + y)
  | "-", Int x, Int y -> Int (x - y)
  | "*", Int x, Int y -> Int (x * y)
  | "/", Int _, Int 0 -> Arb
  | "/", Int x, Int y -> Int (x / y)
  | _ -> Arb

let compare_op op a b =
  match (op, a, b) with
  | "=", _, _ -> Term.equal a b
  | ("<>" | "!="), _, _ -> not (Term.equal a b)
  | "<", Int x, Int y -> x < y
  | "<=", Int x, Int y -> x <= y
  | ">", Int x, Int y -> x > y
  | ">=", Int x, Int y -> x >= y
  | _ -> false

let rec eval g (env : env) e =
  match e with
  | Const t -> t
  | Var x -> ( match lookup env x with Some v -> v | None -> Arb)
  | Quote p -> instantiate env p
  | Bin ("&&", a, b) -> (
      match as_bool (eval g env a) with
      | Some true -> ( match as_bool (eval g env b) with Some v -> of_bool v | None -> Arb)
      | Some false -> ff
      | None -> Arb)
  | Bin ("||", a, b) -> (
      match as_bool (eval g env a) with
      | Some false -> ( match as_bool (eval g env b) with Some v -> of_bool v | None -> Arb)
      | Some true -> tt
      | None -> Arb)
  | Bin (op, a, b) -> arith op (eval g env a) (eval g env b)
  | Chain (a, rest) ->
      let rec go l = function
        | [] -> true
        | ("in", b) :: rest -> member g env l b && go (eval g env b) rest
        | (op, b) :: rest ->
            let r = eval g env b in
            compare_op op l r && go r rest
      in
      of_bool (go (eval g env a) rest)
  | Not a -> ( match as_bool (eval g env a) with Some v -> of_bool (not v) | None -> Arb)
  | Neg a -> ( match eval g env a with Int n -> Int (-n) | _ -> Arb)
  | If (c, a, b) -> (
      match as_bool (eval g env c) with
      | Some true -> eval g env a
      | Some false -> eval g env b
      | None -> Arb)
  | App (Var "dom", a) when lookup env "dom" = None -> (
      match eval g env a with Store kvs -> mk_set (List.map fst kvs) | _ -> Arb)
  | App (f, a) -> (
      match (eval g env f, eval g env a) with
      | Store kvs, k -> (
          match List.find_opt (fun (k', _) -> Term.equal k k') kvs with
          | Some (_, v) -> v
          | None -> Arb)
      | _ -> Arb)
  | Update (s, k, v) -> (
      match (eval g env s, eval g env k, eval g env v) with
      | Store kvs, k, v when not (has_arb k) -> mk_store (kvs @ [ (k, v) ])
      | _ -> Arb)
  | SetLit es -> mk_set (List.map (eval g env) es)
  | StoreLit kvs -> mk_store (List.map (fun (k, v) -> (eval g env k, eval g env v)) kvs)
  | TupleLit es -> Tuple (List.map (eval g env) es)
  | Where (body, binds) ->
      let rec env' =
        lazy (List.map (fun (x, e) -> (x, lazy (eval g (Lazy.force env') e))) binds @ env)
      in
      eval g (Lazy.force env') body

(* x in S, where S is a nonterminal, a builtin set (int, sym, name, store),
   or any expression evaluating to a set. arb is in no set. *)
and member g env x s =
  (not (x = Arb))
  &&
  match s with
  | Var name when Grammar.is_nt g name || List.mem name Grammar.builtins ->
      Grammar.in_set g name x
  | _ -> (
      match eval g env s with Set ts -> List.exists (Term.equal x) ts | _ -> false)
