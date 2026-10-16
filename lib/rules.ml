(* Relations, rules, pattern matching, and the solver.

   Nothing here is specific to small-step semantics: users declare
   relations (->, ==>, |- … : …) and rules concluding them. Solving a
   relation is mode-directed proof search: given its input arguments, find
   every combination of output arguments, each with a derivation. *)

open Term

type mode = In | Out

(* A relation's notation alternates argument slots and symbols, e.g.
   [Arg; Sym "-"; Arg; Sym "->"; Arg] for P -alpha-> P'. *)
type notation_item = NArg | NSym of string

type relation = {
  key : string;                  (* the notation with slots as "_": identifies it *)
  notation : notation_item list;
  sorts : pat list;              (* each argument as declared, e.g. (e, s) *)
  modes : mode list;
  shown : string option;         (* LaTeX template: #1, #2, #1.2 … *)
}

type judgment = { rel : relation; args : pat list }

type premise =
  | Judge of judgment                       (* (e1, s) -> (e1', s') *)
  | Bind of string * string option * Meta.expr   (* n = n1 + n2 *)
  | Cond of Meta.expr                       (* l in dom s *)

type rule = { name : string; concl : judgment; premises : premise list }

(* Why a judgment holds: the rule, the judgment's arguments, and a
   derivation for each judgment premise. *)
type derivation = { by : string; rel_key : string; args : Term.t list; premises : derivation list }

type lang = {
  grammar : Grammar.t;
  mutable relations : relation list;
  mutable rules : rule list;
  memo : (string * Term.t list, (Term.t list * derivation) list) Hashtbl.t;
  active : (string * Term.t list, unit) Hashtbl.t;
  mutable cycle : bool;          (* a goal recurred during the current search *)
  mutable out_of_fuel : bool;    (* the depth limit was hit *)
  mutable max_depth : int;
  mutable max_states : int;
}

let create () =
  { grammar = Grammar.create (); relations = []; rules = []; memo = Hashtbl.create 256;
    active = Hashtbl.create 64; cycle = false; out_of_fuel = false;
    max_depth = 2_000; max_states = 100_000 }

let add_rule lang r =
  lang.rules <- lang.rules @ [ r ];
  Hashtbl.reset lang.memo

let add_relation lang r =
  lang.relations <- List.filter (fun r' -> r'.key <> r.key) lang.relations @ [ r ];
  Hashtbl.reset lang.memo

let pick modes m xs = List.filteri (fun i _ -> List.nth modes i = m) xs
let inputs rel xs = pick rel.modes In xs
let outputs rel xs = pick rel.modes Out xs

(* The shape of a sort, ignoring variable names: (e, s) and (e1, s') agree. *)
let rec sort_shape = function
  | PVar (_, Some nt) -> nt
  | PVar (x, None) -> x
  | PTuple ps -> "(" ^ String.concat "," (List.map sort_shape ps) ^ ")"
  | _ -> "?"

(* A relation can be stepped when it has one input and an output of the same
   sort: that output is the next state, and any other outputs label the step
   (the action of a labelled transition system). *)
let stepping rel =
  match List.filteri (fun i _ -> List.nth rel.modes i = In) rel.sorts with
  | [ input ] ->
      let outs = outputs rel rel.sorts in
      let rec find i = function
        | [] -> None
        | o :: rest -> if sort_shape o = sort_shape input then Some i else find (i + 1) rest
      in
      find 0 outs
  | _ -> None

(* Bind a variable, checking its category, or check consistency with an
   existing binding (repeated names must match equal terms). *)
let bind_var g env x cat v =
  match lookup env x with
  | Some u -> if Term.equal u v then [ env ] else []
  | None ->
      let ok = match cat with None -> v <> Arb | Some nt -> Grammar.mem g nt v in
      if ok then [ bind env x v ] else []

(* All the ways [p] matches [t], each extending [env]. *)
let rec matches g p t env : env list =
  match (p, t) with
  | PVar (x, cat), _ -> bind_var g env x cat t
  | PInt n, Int m -> if n = m then [ env ] else []
  | PSym a, Sym b -> if a = b then [ env ] else []
  | PHole, Hole -> [ env ]
  | PNode ps, Node ts | PTuple ps, Tuple ts ->
      if List.length ps = List.length ts then match_list g ps ts env else []
  | PStore pkvs, Store kvs ->
      if List.length pkvs = List.length kvs then
        match_list g
          (List.concat_map (fun (k, v) -> [ k; v ]) pkvs)
          (List.concat_map (fun (k, v) -> [ k; v ]) kvs)
          env
      else []
  | PCtx (x, nt, inner), _ ->
      List.concat_map
        (fun (ctx, env') -> bind_var g env' x None ctx)
        (decompose g nt inner t env)
  | _ -> []

and match_list g ps ts env =
  List.fold_left2 (fun envs p t -> List.concat_map (matches g p t) envs) [ env ] ps ts

(* All ways to split [t] into a context from nonterminal [nt] with something
   matching [inner] in its hole. *)
and decompose g nt inner t env : (Term.t * env) list =
  List.concat_map
    (fun p ->
      match (p, t) with
      | Grammar.PHole, _ -> List.map (fun env' -> (Hole, env')) (matches g inner t env)
      | Grammar.PAlias y, _ when Grammar.is_ctx_nt g y -> decompose g y inner t env
      | Grammar.PSeq items, Node ts when List.length items = List.length ts -> (
          let slots =
            List.concat
              (List.mapi
                 (fun i it ->
                   match it with Grammar.NT y when Grammar.is_ctx_nt g y -> [ (i, y) ] | _ -> [])
                 items)
          in
          match slots with
          | [ (i, y) ] ->
              let others_ok =
                List.for_all2 (fun it (j, tj) -> j = i || Grammar.mem_item g it tj) items
                  (List.mapi (fun j tj -> (j, tj)) ts)
              in
              if not others_ok then []
              else
                List.map
                  (fun (c, env') -> (Node (List.mapi (fun j tj -> if j = i then c else tj) ts), env'))
                  (decompose g y inner (List.nth ts i) env)
          | _ -> [])
      | _ -> [])
    (Grammar.prods g nt)

(* The solver *)

let dedup results =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (outs, d) ->
      let k = (d.by, outs) in
      if Hashtbl.mem seen k then false
      else (
        Hashtbl.add seen k ();
        true))
    results

(* Every solution of [rel] for these inputs: output arguments and why.
   A goal that recurs while it is being solved contributes nothing to the
   inner call (least fixed point), and searches deeper than [max_depth] are
   cut off; neither kind of incomplete result is memoised. *)
let rec solve lang rel ins depth : (Term.t list * derivation) list =
  let key = (rel.key, ins) in
  match Hashtbl.find_opt lang.memo key with
  | Some r -> r
  | None ->
      if Hashtbl.mem lang.active key then (
        lang.cycle <- true;
        [])
      else if depth > lang.max_depth then (
        lang.out_of_fuel <- true;
        [])
      else (
        Hashtbl.add lang.active key ();
        let cycle = lang.cycle and fuel = lang.out_of_fuel in
        lang.cycle <- false;
        lang.out_of_fuel <- false;
        let r =
          List.concat_map
            (fun rule -> if rule.concl.rel.key = rel.key then apply lang rule ins depth else [])
            lang.rules
          |> dedup
        in
        Hashtbl.remove lang.active key;
        if not (lang.cycle || lang.out_of_fuel) then Hashtbl.replace lang.memo key r;
        lang.cycle <- cycle || lang.cycle;
        lang.out_of_fuel <- fuel || lang.out_of_fuel;
        r)

and apply lang rule ins depth =
  let g = lang.grammar in
  let j = rule.concl in
  let envs = if List.length ins = List.length (inputs j.rel j.args) then match_list g (inputs j.rel j.args) ins [] else [] in
  let states =
    List.fold_left
      (fun sts prem -> List.concat_map (solve_premise lang prem depth) sts)
      (List.map (fun e -> (e, [])) envs)
      rule.premises
  in
  List.map
    (fun (env, ds) ->
      let args = List.map (instantiate env) j.args in
      (outputs j.rel args, { by = rule.name; rel_key = j.rel.key; args; premises = List.rev ds }))
    states

and solve_premise lang prem depth (env, ds) =
  let g = lang.grammar in
  match prem with
  | Judge j ->
      let ins = List.map (instantiate env) (inputs j.rel j.args) in
      List.concat_map
        (fun (outs, d) -> List.map (fun e -> (e, d :: ds)) (match_list g (outputs j.rel j.args) outs env))
        (solve lang j.rel ins (depth + 1))
  | Bind (x, cat, e) -> List.map (fun e -> (e, ds)) (bind_var g env x cat (Meta.eval g env e))
  | Cond e -> if Meta.eval g env e = tt then [ (env, ds) ] else []

(* Solve from the top: clears the flags so they describe this search. *)
let query lang rel ins =
  lang.cycle <- false;
  lang.out_of_fuel <- false;
  solve lang rel ins 0

(* Stepping *)

type step = { rule_name : string; labels : Term.t list; target : Term.t; why : derivation }

let successors lang rel t : step list =
  match stepping rel with
  | None -> []
  | Some j ->
      List.map
        (fun (outs, d) ->
          { rule_name = d.by; target = List.nth outs j; labels = List.filteri (fun i _ -> i <> j) outs; why = d })
        (query lang rel [ t ])

let succ_terms lang rel t = List.sort_uniq compare (List.map (fun s -> s.target) (successors lang rel t))

type run_result = { finals : Term.t list; states : int; truncated : bool }

(* Explore every reachable state; the finals are those with no successor.
   States reachable only through cycles never finish, so a program that can
   only loop has no finals. *)
let run lang rel t : run_result =
  let seen = Hashtbl.create 64 and q = Queue.create () in
  Hashtbl.replace seen t ();
  Queue.add t q;
  let finals = ref [] and truncated = ref false in
  while not (Queue.is_empty q) do
    let c = Queue.pop q in
    match succ_terms lang rel c with
    | [] -> finals := c :: !finals
    | ts ->
        List.iter
          (fun t' ->
            if not (Hashtbl.mem seen t') then
              if Hashtbl.length seen >= lang.max_states then truncated := true
              else (
                Hashtbl.replace seen t' ();
                Queue.add t' q))
          ts
  done;
  { finals = List.sort_uniq compare !finals; states = Hashtbl.length seen; truncated = !truncated }

(* The first relation that can be stepped, if any: what step/run/trace use. *)
let default_stepping lang = List.find_opt (fun r -> stepping r <> None) lang.relations
