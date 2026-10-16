(* Terms are the runtime's only kind of value: object-language syntax trees,
   configurations, stores, and the metalogic's sets all share this type. *)

type t =
  | Int of int
  | Sym of string
  | Node of t list            (* an instance of a grammar production *)
  | Tuple of t list           (* configurations, e.g. (e, s) *)
  | Store of (t * t) list     (* finite map, sorted by key *)
  | Set of t list             (* finite set, sorted, no duplicates *)
  | Hole                      (* the [] of an evaluation context *)
  | Arb                       (* the result of anything ill-defined *)

let tt = Sym "true"
let ff = Sym "false"
let of_bool b = if b then tt else ff

let rec has_arb = function
  | Arb -> true
  | Int _ | Sym _ | Hole -> false
  | Node ts | Tuple ts | Set ts -> List.exists has_arb ts
  | Store kvs -> List.exists (fun (k, v) -> has_arb k || has_arb v) kvs

(* arb is equal to nothing, including itself. *)
let equal a b = (not (has_arb a)) && (not (has_arb b)) && compare a b = 0

(* Later bindings win. *)
let mk_store kvs =
  let kvs =
    List.fold_left (fun acc (k, v) -> (k, v) :: List.remove_assoc k acc) [] kvs
  in
  Store (List.sort (fun (a, _) (b, _) -> compare a b) kvs)

let mk_set ts = Set (List.sort_uniq compare ts)

(* Fill the hole of a context. *)
let rec plug ctx t =
  match ctx with
  | Hole -> t
  | Node cs -> Node (List.map (fun c -> plug c t) cs)
  | Tuple cs -> Tuple (List.map (fun c -> plug c t) cs)
  | c -> c

(* Patterns: object terms whose nonterminal-named identifiers are variables. *)
type pat =
  | PVar of string * string option   (* variable, its nonterminal category *)
  | PCtx of string * string * pat    (* E[p]: context variable, its nonterminal, hole contents *)
  | PInt of int
  | PSym of string
  | PNode of pat list
  | PTuple of pat list
  | PStore of (pat * pat) list
  | PHole

(* Environments are lazy so that metalogic `where` bindings can refer to each
   other in any order. Pattern bindings are already-evaluated values. *)
type env = (string * t Lazy.t) list

let lookup (env : env) x =
  match List.assoc_opt x env with
  | Some v -> ( try Some (Lazy.force v) with Lazy.Undefined -> Some Arb)
  | None -> None

let bind (env : env) x v : env = (x, Lazy.from_val v) :: env

let rec pat_vars = function
  | PVar (x, _) -> [ x ]
  | PCtx (x, _, p) -> x :: pat_vars p
  | PInt _ | PSym _ | PHole -> []
  | PNode ps | PTuple ps -> List.concat_map pat_vars ps
  | PStore kvs -> List.concat_map (fun (k, v) -> pat_vars k @ pat_vars v) kvs

let rec instantiate env = function
  | PVar (x, _) -> ( match lookup env x with Some v -> v | None -> Arb)
  | PCtx (x, _, p) -> (
      match lookup env x with Some c -> plug c (instantiate env p) | None -> Arb)
  | PInt n -> Int n
  | PSym s -> Sym s
  | PHole -> Hole
  | PNode ps -> Node (List.map (instantiate env) ps)
  | PTuple ps -> Tuple (List.map (instantiate env) ps)
  | PStore kvs ->
      mk_store (List.map (fun (k, v) -> (instantiate env k, instantiate env v)) kvs)
