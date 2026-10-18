(* The interface the web UI drives. Terms stay on the OCaml side; the UI
   refers to them by integer ids, which are stable for the lifetime of a
   session (equal terms always get the same id), so the UI can merge paths
   and draw cycles. Relations are referred to by their index in
   [relations]. *)

type session = {
  mutable lang : Rules.lang;
  ids : (Term.t, int) Hashtbl.t;
  terms : (int, Term.t) Hashtbl.t;
}

type error = { line : int; col : int; message : string }

let create () = { lang = Rules.create (); ids = Hashtbl.create 256; terms = Hashtbl.create 256 }

let id s t =
  match Hashtbl.find_opt s.ids t with
  | Some i -> i
  | None ->
      let i = Hashtbl.length s.ids in
      Hashtbl.add s.ids t i;
      Hashtbl.add s.terms i t;
      i

let term s i = Hashtbl.find s.terms i
let show s i = Grammar.to_string s.lang.grammar (term s i)
let latex s i = Latex.term s.lang.grammar (term s i)

(* The term [i] typeset with what differs from [j] wrapped in
   \htmlClass{cls}{…}: the redex before a step, the result after it. *)
let latex_diff s i j cls = Latex.term_diff s.lang.grammar cls (term s i) (term s j)

let catch f =
  try Ok (f ()) with Lexer.Error (tk, message) -> Error { line = tk.line; col = tk.col; message }

(* Replace the language with a new definition. Any commands in the source
   run too; their output is returned. *)
let load s src =
  catch (fun () ->
      let lang = Rules.create () in
      let out = ref [] in
      Toplevel.run_string ~out:(fun l -> out := l :: !out) lang src;
      s.lang <- lang;
      Hashtbl.reset s.ids;
      Hashtbl.reset s.terms;
      List.rev !out)

let config s src = catch (fun () -> id s (Toplevel.parse_config s.lang src))

(* Relations *)

type relation_info = {
  index : int;
  notation : string;   (* with slots, e.g. "_ -> _": identifies the relation *)
  form : string;       (* as declared, e.g. "(e, s) -> (e, s)" *)
  shown : string;      (* LaTeX, with the declared sorts as arguments *)
  steps : bool;        (* can be stepped: one input, an output of its sort *)
  inputs : int;
}

let relation s i = List.nth s.lang.relations i

let relations s =
  List.mapi
    (fun index (r : Rules.relation) ->
      let rec sort = function
        | Term.PVar (x, _) -> x
        | Term.PTuple ps -> "(" ^ String.concat ", " (List.map sort ps) ^ ")"
        | _ -> "?"
      in
      let rec form items sorts =
        match (items, sorts) with
        | Rules.NArg :: rest, x :: sorts -> sort x :: form rest sorts
        | Rules.NSym y :: rest, sorts -> y :: form rest sorts
        | _ -> []
      in
      { index; notation = r.key; form = String.concat " " (form r.notation r.sorts); shown = Latex.relation s.lang.grammar r;
        steps = Rules.stepping r <> None; inputs = List.length (Rules.inputs r r.sorts) })
    s.lang.relations

(* Derivations, typeset: each node is a judgment and the rule that proves it. *)
type derivation = { rule : string; judgment : string; premises : derivation list }

let rec derivation s (d : Rules.derivation) =
  let rel = List.find (fun (r : Rules.relation) -> r.key = d.rel_key) s.lang.relations in
  { rule = d.by; judgment = Latex.judgment_terms s.lang.grammar rel d.args;
    premises = List.map (derivation s) d.premises }

type successor = { rule_name : string; target : int; labels : string list; why : derivation }

let successors s rel i =
  List.map
    (fun (st : Rules.step) ->
      { rule_name = st.rule_name; target = id s st.target;
        labels = List.map (Latex.term s.lang.grammar) st.labels; why = derivation s st.why })
    (Rules.successors s.lang (relation s rel) (term s i))

type solution = { outputs : string list; output_ids : int list; proof : derivation }
type solved = { solutions : solution list; out_of_fuel : bool }

(* All solutions of a relation with one input. *)
let solve s rel i =
  let r = relation s rel in
  let sols = Rules.query s.lang r [ term s i ] in
  { solutions =
      List.map
        (fun (outs, d) ->
          { outputs = List.map (Latex.term s.lang.grammar) outs; output_ids = List.map (id s) outs;
            proof = derivation s d })
        sols;
    out_of_fuel = s.lang.out_of_fuel }

type run = { finals : int list; states : int; truncated : bool }

let run s rel i =
  let r = Rules.run s.lang (relation s rel) (term s i) in
  { finals = List.map (id s) r.finals; states = r.states; truncated = r.truncated }

let rule_names s = List.map (fun (r : Rules.rule) -> r.name) s.lang.rules

(* Editor support *)

type diagnostic = { severity : string; start : int; stop : int; message : string }
type typeset_rule = { from : int; until : int; tex : string }

type checked = {
  diagnostics : diagnostic list;
  nonterminals : string list;
  literals : string list;        (* symbols the grammar uses: if, skip, :=, ... *)
  keywords : string list;        (* literal words inside productions: if, then, while *)
  operator_nts : string list;    (* nonterminals that stand for an operator: op *)
  grammar_tex : string;
  relations_tex : string list;
  rules_tex : typeset_rule list; (* by the source range of each rule *)
}

let check src =
  let ds, lang, rules = Toplevel.check src in
  let g = lang.grammar in
  {
    diagnostics =
      List.map
        (fun (d : Toplevel.diag) ->
          { severity = (match d.severity with Err -> "error" | Warning -> "warning");
            start = d.start; stop = d.stop; message = d.message })
        ds;
    nonterminals = List.map fst g.nts;
    literals = List.sort_uniq compare (Grammar.literals g @ g.words);
    keywords = List.sort_uniq compare (Latex.keyword_words g);
    operator_nts = List.filter (fun x -> Grammar.op_lits g x <> None) (List.map fst g.nts);
    grammar_tex = Latex.grammar g;
    relations_tex = List.map (Latex.relation g) lang.relations;
    rules_tex = List.map (fun (r, from, until) -> { from; until; tex = Latex.rule g r }) rules;
  }

let outline src =
  match Toplevel.outline src with
  | Ok o -> Ok o
  | Error (tk, message) -> Error { line = tk.line; col = tk.col; message }

(* Problems in a program against the loaded language: syntax errors with
   their range, and subterms no production matches. *)
let check_program s src =
  let g = s.lang.grammar in
  let in_grammar t = List.exists (fun (nt, _) -> Grammar.mem g nt t) g.nts in
  let rec stray (t : Term.t) =
    match t with
    | Tuple ts -> List.find_map stray ts
    | Node ts when not (in_grammar t) -> (
        match List.find_map stray ts with Some u -> Some u | None -> Some t)
    | _ -> None
  in
  match Toplevel.parse_config s.lang src with
  | t -> (
      match stray t with
      | None -> []
      | Some u ->
          [ { severity = "warning"; start = 0; stop = String.length src;
              message =
                Printf.sprintf "%s doesn't match any production of the grammar" (Grammar.to_string g u) } ])
  | exception Lexer.Error (tk, message) ->
      [ { severity = "error"; start = tk.start; stop = max tk.stop (tk.start + 1); message } ]
