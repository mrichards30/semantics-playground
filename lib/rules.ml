(* Rules, pattern matching, and the runtime: step and run. *)

open Term

type premise =
  | Step of pat * pat                       (* (e1, s) -> (e1', s') *)
  | Steps of pat * pat                      (* (e, s) ->* (v, s') *)
  | Bind of string * string option * Meta.expr   (* n = n1 + n2 *)
  | Cond of Meta.expr                       (* l in dom s *)

type rule = { name : string; lhs : pat; rhs : pat; premises : premise list }

type lang = {
  grammar : Grammar.t;
  mutable rules : rule list;
  memo : (Term.t, (string * env * Term.t) list) Hashtbl.t;
  mutable max_states : int;
}

let create () =
  { grammar = Grammar.create (); rules = []; memo = Hashtbl.create 256; max_states = 100_000 }

let add_rule lang r =
  lang.rules <- lang.rules @ [ r ];
  Hashtbl.reset lang.memo

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

let dedup_succs succs =
  let seen = Hashtbl.create 16 in
  List.filter
    (fun (name, _, t) ->
      if Hashtbl.mem seen (name, t) then false
      else (
        Hashtbl.add seen (name, t) ();
        true))
    succs

type run_result = { finals : Term.t list; states : int; truncated : bool }

(* Every successor of a configuration, with the rule that produced it and
   the rule's bindings. *)
let rec successors lang t : (string * env * Term.t) list =
  match Hashtbl.find_opt lang.memo t with
  | Some r -> r
  | None ->
      let r =
        List.concat_map
          (fun rule -> List.map (fun (env, t') -> (rule.name, env, t')) (apply_rule lang rule t))
          lang.rules
        |> dedup_succs
      in
      Hashtbl.replace lang.memo t r;
      r

and succ_terms lang t = List.sort_uniq compare (List.map (fun (_, _, t) -> t) (successors lang t))

and apply_rule lang rule t =
  let g = lang.grammar in
  let envs = matches g rule.lhs t [] in
  let envs =
    List.fold_left (fun envs prem -> List.concat_map (solve lang prem) envs) envs rule.premises
  in
  List.map (fun env -> (env, instantiate env rule.rhs)) envs

and solve lang prem env =
  let g = lang.grammar in
  match prem with
  | Step (a, b) ->
      List.concat_map (fun t' -> matches g b t' env) (succ_terms lang (instantiate env a))
  | Steps (a, b) ->
      List.concat_map (fun t' -> matches g b t' env) (run lang (instantiate env a)).finals
  | Bind (x, cat, e) -> bind_var g env x cat (Meta.eval g env e)
  | Cond e -> if Meta.eval g env e = tt then [ env ] else []

(* Explore every reachable configuration; the finals are those with no
   successor. Configurations reachable only through cycles never finish, so
   a program that can only loop has no finals. *)
and run lang t : run_result =
  let seen = Hashtbl.create 64 and q = Queue.create () in
  Hashtbl.replace seen t ();
  Queue.add t q;
  let finals = ref [] and truncated = ref false in
  while not (Queue.is_empty q) do
    let c = Queue.pop q in
    match succ_terms lang c with
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
