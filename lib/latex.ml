(* Typesetting: terms, patterns, judgments, rules and grammars as LaTeX.

   Structure comes from the grammar (the same forms and fixities the parser
   uses), so brackets appear only where needed. How things are drawn can be
   set in the source: a production or relation may carry a template, e.g.
   ~a shown "\bar{#1}", where #1, #2 … are its subterms (and #1.2 the second
   component of a tuple argument). Everything else gets textbook defaults:
   keywords in bold, variables in italic with subscripts and primes, Greek
   names as Greek letters, tuples as angle brackets. *)

open Term
module G = Grammar
module R = Rules

let escape s =
  String.concat ""
    (List.map
       (function
         | '\\' -> "\\backslash{}"
         | ('#' | '$' | '%' | '&' | '_' | '{' | '}') as c -> "\\" ^ String.make 1 c
         | '~' -> "\\sim{}"
         | '^' -> "\\wedge{}"
         | c -> String.make 1 c)
       (List.init (String.length s) (String.get s)))

let greek =
  [ "alpha"; "beta"; "gamma"; "delta"; "epsilon"; "zeta"; "eta"; "theta"; "iota"; "kappa";
    "lambda"; "mu"; "nu"; "xi"; "pi"; "rho"; "sigma"; "tau"; "upsilon"; "phi"; "chi"; "psi";
    "omega"; "Gamma"; "Delta"; "Theta"; "Lambda"; "Xi"; "Pi"; "Sigma"; "Upsilon"; "Phi"; "Psi";
    "Omega" ]

(* A name in math italic: one letter as is, Greek names as Greek letters. *)
let name x =
  if List.mem x greek then "\\" ^ x
  else if String.length x = 1 then x
  else "\\textit{" ^ escape x ^ "}"   (* text italic keeps a word's spacing *)

(* A pattern variable: e1' is e_{1}', e_body is e_{\mathit{body}}. *)
let variable g x =
  let n = String.length x in
  let p = ref n in
  while !p > 0 && x.[!p - 1] = '\'' do decr p done;
  let primes = String.sub x !p (n - !p) and s = String.sub x 0 !p in
  match G.base_nt g x with
  | None -> name s ^ primes
  | Some base ->
      let rest = String.sub s (String.length base) (String.length s - String.length base) in
      let sub =
        if rest = "" then ""
        else if rest.[0] = '_' then "_{" ^ name (String.sub rest 1 (String.length rest - 1)) ^ "}"
        else "_{" ^ rest ^ "}"
      in
      name base ^ sub ^ primes

let op_tex = function
  | "+" -> "+" | "-" -> "-" | "*" -> "\\times" | "/" -> "/"
  | ">=" -> "\\geq" | "<=" -> "\\leq" | "<>" | "!=" -> "\\neq" | "=" -> "=" | "<" -> "<" | ">" -> ">"
  | ":=" -> "\\mathrel{:=}" | ";" -> "\\mathbin{;}" | "!" -> "{!}" | "||" -> "\\parallel"
  | "|" -> "\\mid" | "." -> "." | "\\" -> "\\backslash" | "->" -> "\\to" | "|->" -> "\\mapsto"
  | "::" -> "::" | "~" -> "\\sim" | "==>" -> "\\Downarrow" | "|-" -> "\\vdash" | "-->" -> "\\longrightarrow"
  | ":" -> ":" | "," -> ","
  | o -> "\\mathbin{\\texttt{" ^ escape o ^ "}}"

let is_word l = l <> "" && Lexer.is_ident_start l.[0]

(* Words inside a production (if, then, while) are keywords and get space
   around them; a word that is a whole production (skip, true) is a value. *)
let keyword_words g =
  List.concat_map
    (fun (_, ps) ->
      List.concat_map
        (function G.PSeq items -> List.filter_map (function G.Lit l when is_word l -> Some l | _ -> None) items | _ -> [])
        ps)
    g.G.nts

let literal g l =
  match Hashtbl.find_opt g.G.lit_templates l with
  | Some t -> "{" ^ t ^ "}"
  | None ->
      if is_word l then
        if List.mem l (keyword_words g) then "\\mathrel{\\mathbf{" ^ escape l ^ "}}"
        else "\\mathbf{" ^ escape l ^ "}"
      else if l <> "" && Lexer.is_digit l.[0] then l
      else op_tex l

(* Fill a template: #1 … #9, and #1.2 for a component of a tuple argument. *)
let fill template (args : string list) (parts : string list list) =
  let b = Buffer.create 64 and n = String.length template in
  let rec go i =
    if i < n then
      if template.[i] = '#' && i + 1 < n && Lexer.is_digit template.[i + 1] then (
        let k = Char.code template.[i + 1] - Char.code '1' in
        if i + 3 < n && template.[i + 2] = '.' && Lexer.is_digit template.[i + 3] then (
          let c = Char.code template.[i + 3] - Char.code '1' in
          (match List.nth_opt parts k with
          | Some comps -> Buffer.add_string b ("{" ^ Option.value ~default:"" (List.nth_opt comps c) ^ "}")
          | None -> ());
          go (i + 4))
        else (
          Buffer.add_string b ("{" ^ Option.value ~default:"" (List.nth_opt args k) ^ "}");
          go (i + 2)))
      else (
        Buffer.add_char b template.[i];
        go (i + 1))
  in
  go 0;
  Buffer.contents b

(* Terms and patterns share one printer. *)

type v =
  | VInt of int
  | VSym of string
  | VVar of string
  | VCtx of string * v
  | VNode of v list
  | VTuple of v list
  | VMap of (v * v) list
  | VSet of v list
  | VHole
  | VArb

let rec of_term : Term.t -> v = function
  | Int n -> VInt n
  | Sym x -> VSym x
  | Node ts -> VNode (List.map of_term ts)
  | Tuple ts -> VTuple (List.map of_term ts)
  | Store kvs -> VMap (List.map (fun (k, v) -> (of_term k, of_term v)) kvs)
  | Set ts -> VSet (List.map of_term ts)
  | Hole -> VHole
  | Arb -> VArb

let rec of_pat : Term.pat -> v = function
  | PVar ("?", None) -> VSym "?"
  | PVar (x, _) -> VVar x
  | PCtx (x, _, p) -> VCtx (x, of_pat p)
  | PInt n -> VInt n
  | PSym x -> VSym x
  | PNode ps -> VNode (List.map of_pat ps)
  | PTuple ps -> VTuple (List.map of_pat ps)
  | PStore kvs -> VMap (List.map (fun (k, v) -> (of_pat k, of_pat v)) kvs)
  | PHole -> VHole

let rec tex g ?(req = 0) ?(tail = true) (t : v) =
  match t with
  | VInt n -> (
      match Hashtbl.find_opt g.G.lit_templates (string_of_int n) with
      | Some l -> "{" ^ l ^ "}"
      | None -> string_of_int n)
  | VSym "?" -> "\\mathord{?}"
  | VSym x -> if List.mem x (G.literals g) then literal g x else name x
  | VVar x -> variable g x
  | VCtx (x, p) -> variable g x ^ "[" ^ tex g p ^ "]"
  | VHole -> "[\\,]"
  | VArb -> "\\mathrm{arb}"
  | VTuple ts -> "\\langle " ^ String.concat ",\\, " (List.map (tex g) ts) ^ " \\rangle"
  | VSet ts -> "\\{" ^ String.concat ",\\, " (List.map (tex g) ts) ^ "\\}"
  | VMap kvs ->
      "\\{" ^ String.concat ",\\, " (List.map (fun (k, v) -> tex g k ^ " \\mapsto " ^ tex g v) kvs) ^ "\\}"
  | VNode ts -> node g req tail ts

and node g req tail ts =
  let subterms sk ts = List.filteri (fun i _ -> List.nth sk i = None) ts in
  (* operator positions may hold a variable (e1 op e2) *)
  let lit_of = function VSym l -> Some l | _ -> None in
  (* a template's skeleton fits when its literals are where ours are; a
     subterm slot can hold anything, including a symbol (the a in ~a) *)
  let fits sk =
    List.length sk = List.length ts
    && List.for_all2 (fun l t -> match l with Some l -> t = VSym l | None -> true) sk ts
  in
  let template ts =
    Hashtbl.fold
      (fun sk t found -> match found with Some _ -> found | None -> if fits sk then Some (sk, t) else None)
      g.G.templates None
    |> Option.map (fun (sk, t) -> fill t (List.map (tex g) (subterms sk ts)) [])
  in
  match template ts with
  | Some s -> s
  | None -> (
      let items sk ts last_req last_tail =
        let n = List.length ts in
        List.mapi
          (fun i (lit, t) ->
            match lit with
            | Some l -> literal g l
            | None -> if i = n - 1 then tex g ~req:last_req ~tail:last_tail t else tex g t)
          (List.combine sk ts)
        |> String.concat " "
      in
      let nud =
        match ts with
        | VSym k :: rest -> (
            match G.nud_form g k with
            | Some sk when List.length sk = List.length rest -> Some (k, sk, rest)
            | _ -> None)
        | _ -> None
      in
      let led =
        match ts with
        | l :: o :: rest -> (
            let k = lit_of o in
            let form = match k with Some k -> G.led_form g k | None -> None in
            match (form, o) with
            | Some sk, _ when List.length sk = List.length rest -> Some (l, o, Option.get k, sk, rest)
            | None, VVar x -> (
                (* an operator variable: use the shape of any of its operators *)
                match Option.bind (G.base_nt g x) (G.op_lits g) with
                | Some (k :: _) -> (
                    match G.led_form g k with
                    | Some sk when List.length sk = List.length rest -> Some (l, o, k, sk, rest)
                    | _ -> None)
                | _ -> None)
            | _ -> None)
        | _ -> None
      in
      match (nud, led) with
      | Some (k, sk, rest), _ ->
          let p = G.nud_prec g k in
          let wrap = (not tail) && p < req in
          let s = literal g k ^ " " ^ items sk rest p (tail || wrap) in
          if wrap then "(" ^ s ^ ")" else s
      | None, Some (l, o, k, sk, rest) ->
          let a, p = G.led_fixity g k in
          let lreq, rreq =
            match a with G.Right -> (p + 1, p) | G.Left | G.Prefix -> (p, p + 1) | G.Non -> (p + 1, p + 1)
          in
          let wrap = p < req in
          let op = match o with VVar x -> "\\mathbin{" ^ variable g x ^ "}" | _ -> literal g k in
          let s = tex g ~req:lreq ~tail:false l ^ " " ^ op ^ " " ^ items sk rest rreq (tail || wrap) in
          if wrap then "(" ^ s ^ ")" else s
      | None, None -> "(" ^ String.concat "\\ " (List.map (tex g ~req:1000 ~tail:false) ts) ^ ")")

let term g t = tex g (of_term t)
let pat g p = tex g (of_pat p)

(* Judgments: the relation's template, or its notation with symbols drawn. *)
let judgment_of g (rel : R.relation) (args : v list) =
  let arg_tex = List.map (tex g) args in
  match rel.R.shown with
  | Some t ->
      let parts = List.map (function VTuple ts -> List.map (tex g) ts | a -> [ tex g a ]) args in
      fill t arg_tex parts
  | None ->
      let rec go items args =
        match (items, args) with
        | R.NArg :: rest, a :: args -> a :: go rest args
        | R.NSym x :: rest, args ->
            (if is_word x then "\\mathrel{\\mathrm{" ^ escape x ^ "}}"
             else if x = "->" then "\\longrightarrow"
             else "\\mathrel{" ^ op_tex x ^ "}")
            :: go rest args
        | _ -> []
      in
      String.concat " " (go rel.R.notation arg_tex)

let judgment g (j : R.judgment) = judgment_of g j.R.rel (List.map of_pat j.R.args)
let judgment_terms g rel args = judgment_of g rel (List.map of_term args)

(* The metalogic, for side conditions. *)
let rec meta g (e : Meta.expr) =
  let open Meta in
  (* comparisons and connectives inside another expression keep their brackets *)
  let operand = function
    | (Chain _ | Bin (("&&" | "||"), _, _)) as x -> "(" ^ meta g x ^ ")"
    | x -> meta g x
  in
  (* the right of `in`: a nonterminal or built-in set by name, or an expression *)
  let set_or_meta = function
    | Meta.Var x when x = "int" -> "\\mathbb{Z}"
    | Meta.Var x when List.mem x G.builtins -> "\\mathsf{" ^ x ^ "}"
    | Meta.Var x when G.is_nt g x -> name x
    | b -> meta g b
  in
  match e with
  | Const t -> term g t
  | Var x -> variable g x
  | Quote p -> pat g p
  | Bin (op, a, b) ->
      let o = match op with "&&" -> "\\land" | "||" -> "\\lor" | "*" -> "\\times" | o -> o in
      operand a ^ " " ^ o ^ " " ^ operand b
  | Chain (a, rest) ->
      operand a
      ^ String.concat ""
          (List.map
             (fun (op, b) ->
               let o = match op with "in" -> "\\in" | "<=" -> "\\leq" | ">=" -> "\\geq" | "<>" | "!=" -> "\\neq" | o -> o in
               " " ^ o ^ " " ^ (match b with Chain _ | Bin (("&&" | "||"), _, _) -> operand b | _ -> set_or_meta b))
             rest)
  | Not (Chain (a, [ ("in", b) ])) -> meta g a ^ " \\notin " ^ set_or_meta b
  | Not a -> "\\neg(" ^ meta g a ^ ")"
  | Neg a -> "-" ^ meta g a
  | If (c, a, b) -> "\\text{if } " ^ meta g c ^ " \\text{ then } " ^ meta g a ^ " \\text{ else } " ^ meta g b
  | App (Var "dom", a) -> "\\mathrm{dom}(" ^ meta g a ^ ")"
  | App (f, a) -> meta g f ^ "(" ^ meta g a ^ ")"
  | Update (s, k, v) -> meta g s ^ "[" ^ meta g k ^ " \\mapsto " ^ meta g v ^ "]"
  | SetLit es -> "\\{" ^ String.concat ",\\, " (List.map (meta g) es) ^ "\\}"
  | StoreLit kvs -> "\\{" ^ String.concat ",\\, " (List.map (fun (k, v) -> meta g k ^ " \\mapsto " ^ meta g v) kvs) ^ "\\}"
  | TupleLit es -> "(" ^ String.concat ",\\, " (List.map (meta g) es) ^ ")"
  | Where (body, binds) ->
      meta g body ^ " \\text{ where } "
      ^ String.concat ",\\; " (List.map (fun (x, e) -> variable g x ^ " = " ^ meta g e) binds)

(* A rule: judgment premises above the bar, side conditions to the right,
   as in Sewell's notes. *)
let rule_name n =
  (* runs of operator characters are drawn as operators: op>= is op≥ *)
  let b = Buffer.create 16 and len = String.length n in
  let rec go i =
    if i < len then
      if Lexer.is_ident_char n.[i] then (
        Buffer.add_char b n.[i];
        go (i + 1))
      else
        let j = ref i in
        while !j < len && not (Lexer.is_ident_char n.[!j]) do incr j done;
        Buffer.add_string b ("}{" ^ op_tex (String.sub n i (!j - i)) ^ "}\\textrm{");
        go !j
  in
  go 0;
  "\\textrm{(" ^ Buffer.contents b ^ ")}"

let rule g (r : R.rule) =
  let above = List.filter_map (function R.Judge j -> Some (judgment g j) | _ -> None) r.R.premises in
  let side =
    List.filter_map
      (function
        | R.Bind (x, _, e) ->
            let rhs = match e with Meta.Chain _ | Meta.Bin (("&&" | "||"), _, _) -> "(" ^ meta g e ^ ")" | _ -> meta g e in
            Some (variable g x ^ " = " ^ rhs)
        | R.Cond e -> Some (meta g e)
        | R.Judge _ -> None)
      r.R.premises
  in
  let concl = judgment g r.R.concl in
  let body = if above = [] then concl else "\\dfrac{" ^ String.concat " \\qquad " above ^ "}{" ^ concl ^ "}" in
  let cond = if side = [] then "" else " \\qquad \\text{if } " ^ String.concat ",\\; " side in
  rule_name r.R.name ^ " \\quad " ^ body ^ cond

(* A grammar as a BNF array, with one row per nonterminal. *)
let production g = function
  | G.PBuiltin b -> if b = "int" then "\\mathbb{Z}" else "\\mathsf{" ^ b ^ "}"
  | G.PAlias y -> name y
  | G.PLit l -> literal g l
  | G.PHole -> "[\\,]"
  | G.PSeq items -> (
      match Hashtbl.find_opt g.G.templates (G.skel_of items) with
      | Some t -> fill t (List.filter_map (function G.NT y -> Some (name y) | _ -> None) items) []
      | None ->
          String.concat " "
            (List.map
               (function
                 | G.NT y -> (
                     match G.op_lits g y with Some _ -> "\\mathbin{" ^ name y ^ "}" | None -> name y)
                 | G.Lit l -> literal g l)
               items))

let grammar g =
  "\\begin{array}{rcl} "
  ^ String.concat " \\\\ "
      (List.map
         (fun (x, ps) -> name x ^ " & ::= & " ^ String.concat " \\;\\mid\\; " (List.map (production g) ps))
         g.G.nts)
  ^ " \\end{array}"

let relation g (r : R.relation) =
  judgment_of g r (List.map of_pat r.R.sorts)
