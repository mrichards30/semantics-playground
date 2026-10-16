(* Grammars: nonterminals, their productions, and fixity declarations.

   A grammar plays four roles:
   - each nonterminal is a set of terms (membership: [mem]);
   - pattern variables named after a nonterminal only match its members;
   - a nonterminal with a [] production is an evaluation context;
   - productions plus fixities tell the object parser how to read terms. *)

type item = Lit of string | NT of string

type prod =
  | PBuiltin of string   (* int, sym, name, store *)
  | PAlias of string     (* another nonterminal *)
  | PLit of string       (* a single literal symbol, e.g. skip *)
  | PHole                (* [] *)
  | PSeq of item list    (* e.g. if e then e else e *)

type assoc = Left | Right | Non | Prefix

type t = {
  mutable nts : (string * prod list) list;
  fixity : (string, assoc * int) Hashtbl.t;
}

let builtins = [ "int"; "sym"; "name"; "store" ]
let create () = { nts = []; fixity = Hashtbl.create 16 }
let is_nt g x = List.mem_assoc x g.nts
let prods g x = Option.value ~default:[] (List.assoc_opt x g.nts)

(* Redefining a nonterminal adds productions, so extensions can add syntax. *)
let add_prods g x ps =
  if is_nt g x then
    g.nts <- List.map (fun (y, qs) -> if y = x then (y, qs @ ps) else (y, qs)) g.nts
  else g.nts <- g.nts @ [ (x, ps) ]

(* The nonterminal a variable name refers to: e, e1, e', e1', e_body all
   refer to e. *)
let base_nt g x =
  let s = ref x in
  while String.length !s > 1 && !s.[String.length !s - 1] = '\'' do
    s := String.sub !s 0 (String.length !s - 1)
  done;
  let s = !s in
  if is_nt g s then Some s
  else
    match String.index_opt s '_' with
    | Some i when i > 0 && is_nt g (String.sub s 0 i) -> Some (String.sub s 0 i)
    | _ ->
        let j = ref (String.length s) in
        while !j > 0 && Lexer.is_digit s.[!j - 1] do decr j done;
        if !j > 0 && !j < String.length s && is_nt g (String.sub s 0 !j) then
          Some (String.sub s 0 !j)
        else None

let literals g =
  List.concat_map
    (fun (_, ps) ->
      List.concat_map
        (function
          | PLit l -> [ l ]
          | PSeq items -> List.filter_map (function Lit l -> Some l | NT _ -> None) items
          | _ -> [])
        ps)
    g.nts

(* `name`: any symbol the grammar doesn't use as a literal or nonterminal. *)
let is_name g s = (not (List.mem s (literals g))) && base_nt g s = None

(* A nonterminal whose productions are all literals, like op ::= + | >=. *)
let op_lits g x =
  let ps = prods g x in
  if ps <> [] && List.for_all (function PLit _ -> true | _ -> false) ps then
    Some (List.map (function PLit l -> l | _ -> assert false) ps)
  else None

let rec is_ctx_nt_seen seen g x =
  (not (List.mem x seen))
  && List.exists
       (function
         | PHole -> true
         | PAlias y -> is_ctx_nt_seen (x :: seen) g y
         | _ -> false)
       (prods g x)

let is_ctx_nt g x = is_ctx_nt_seen [] g x

(* Membership *)

let rec mem g nt (t : Term.t) = List.exists (fun p -> mem_prod g p t) (prods g nt)

and mem_prod g p (t : Term.t) =
  match (p, t) with
  | PBuiltin "int", Int _ -> true
  | PBuiltin "sym", Sym _ -> true
  | PBuiltin "name", Sym s -> is_name g s
  | PBuiltin "store", Store _ -> true
  | PBuiltin _, _ -> false
  | PAlias y, _ -> mem g y t
  | PLit l, Sym s -> l = s
  | PHole, Hole -> true
  | PSeq items, Node ts ->
      List.length items = List.length ts && List.for_all2 (mem_item g) items ts
  | _ -> false

and mem_item g it (t : Term.t) =
  match (it, t) with Lit l, Sym s -> l = s | NT y, _ -> mem g y t | _ -> false

(* `t in name` for a nonterminal or builtin set. *)
let in_set g name t =
  if is_nt g name then mem g name t
  else if List.mem name builtins then mem_prod g (PBuiltin name) t
  else false

(* Syntax forms, derived from productions for the parser and printer.
   A skeleton lists the production's items with nonterminals erased. *)

type skel = string option list   (* Some literal | None for a subterm slot *)

let skel_of items = List.map (function Lit l -> Some l | NT _ -> None) items

(* Prefix forms start with a literal (if, while, !); infix forms start with a
   subterm followed by a literal (+, :=, ;). Returned as (key, rest-of-skeleton). *)
let forms g =
  let nuds = ref [] and leds = ref [] in
  let add r k sk = if not (List.mem (k, sk) !r) then r := !r @ [ (k, sk) ] in
  List.iter
    (fun (_, ps) ->
      List.iter
        (function
          | PSeq (Lit k :: rest) -> add nuds k (skel_of rest)
          | PSeq (NT _ :: Lit k :: rest) -> add leds k (skel_of rest)
          | PSeq (NT _ :: NT o :: rest) -> (
              match op_lits g o with
              | Some ls -> List.iter (fun k -> add leds k (skel_of rest)) ls
              | None -> ())
          | _ -> ())
        ps)
    g.nts;
  (!nuds, !leds)

let nud_form g k = List.assoc_opt k (fst (forms g))
let led_form g k = List.assoc_opt k (snd (forms g))

let led_fixity g k =
  match Hashtbl.find_opt g.fixity k with
  | Some (Prefix, p) | Some (Left, p) -> (Left, p)
  | Some (a, p) -> (a, p)
  | None -> (Left, 5)

(* The precedence of a prefix form's last subterm. *)
let nud_prec g k =
  match Hashtbl.find_opt g.fixity k with Some (_, p) -> p | None -> 0

(* Printing, with only the parentheses the fixities require. *)

(* [req] is the precedence the context requires; [tail] says nothing follows
   the term, so a prefix form like while ... do e needs no parentheses even
   though its body would otherwise swallow what comes after. *)
let rec to_string g (t : Term.t) = print g 0 true t

and print g req tail (t : Term.t) =
  match t with
  | Int n -> string_of_int n
  | Sym s -> s
  | Hole -> "[]"
  | Arb -> "arb"
  | Tuple ts -> "(" ^ String.concat ", " (List.map (print g 0 true) ts) ^ ")"
  | Set ts -> "{" ^ String.concat ", " (List.map (print g 0 true) ts) ^ "}"
  | Store kvs ->
      "{"
      ^ String.concat ", "
          (List.map (fun (k, v) -> print g 0 true k ^ " |-> " ^ print g 0 true v) kvs)
      ^ "}"
  | Node ts -> (
      let print_items sk ts last_req last_tail =
        let n = List.length ts in
        List.mapi
          (fun i (lit, t) ->
            match lit with
            | Some l -> l
            | None ->
                if i = n - 1 then print g last_req last_tail t else print g 0 true t)
          (List.combine sk ts)
        |> String.concat " "
      in
      let nud =
        match ts with
        | Sym k :: rest -> (
            match nud_form g k with
            | Some sk when List.length sk = List.length rest -> Some (k, sk, rest)
            | _ -> None)
        | _ -> None
      in
      let led =
        match ts with
        | l :: Sym k :: rest -> (
            match led_form g k with
            | Some sk when List.length sk = List.length rest -> Some (l, k, sk, rest)
            | _ -> None)
        | _ -> None
      in
      match (nud, led) with
      | Some (k, sk, rest), _ ->
          let p = nud_prec g k in
          let wrap = (not tail) && p < req in
          let body = print_items sk rest p (tail || wrap) in
          let glue = if String.length k = 1 && not (Lexer.is_ident_start k.[0]) then "" else " " in
          let s = k ^ glue ^ body in
          if wrap then "(" ^ s ^ ")" else s
      | None, Some (l, k, sk, rest) ->
          let a, p = led_fixity g k in
          let lreq, rreq =
            match a with Right -> (p + 1, p) | Left | Prefix -> (p, p + 1) | Non -> (p + 1, p + 1)
          in
          let wrap = p < req in
          let body = print_items sk rest rreq (tail || wrap) in
          let s = print g lreq false l ^ " " ^ k ^ " " ^ body in
          if wrap then "(" ^ s ^ ")" else s
      | None, None ->
          "(" ^ String.concat " " (List.map (print g 1000 false) ts) ^ ")")
