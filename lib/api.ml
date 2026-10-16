(* The interface the web UI drives. Terms stay on the OCaml side; the UI
   refers to configurations by integer ids, which are stable for the
   lifetime of a session (equal configurations always get the same id), so
   the UI can merge paths and draw cycles in the configuration graph. *)

type session = {
  mutable lang : Rules.lang;
  ids : (Term.t, int) Hashtbl.t;
  terms : (int, Term.t) Hashtbl.t;
}

type successor = { rule : string; target : int; bindings : (string * string) list }
type run = { finals : int list; states : int; truncated : bool }
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

let catch f =
  try Ok (f ()) with Lexer.Error (tk, message) -> Error { line = tk.line; col = tk.col; message }

(* Replace the language with a new definition (grammar, fixity, rules).
   Any commands in the source run too; their output is returned. *)
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

let successors s i =
  List.map
    (fun (rule, env, t) ->
      let bindings =
        List.rev env
        |> List.filter_map (fun (x, v) ->
               if Lazy.is_val v then Some (x, Grammar.to_string s.lang.grammar (Lazy.force v))
               else None)
      in
      { rule; target = id s t; bindings })
    (Rules.successors s.lang (term s i))

let run s i =
  let r = Rules.run s.lang (term s i) in
  { finals = List.map (id s) r.finals; states = r.states; truncated = r.truncated }

let rule_names s = List.map (fun (r : Rules.rule) -> r.name) s.lang.rules

(* Editor support. *)

type diagnostic = { severity : string; start : int; stop : int; message : string }

type checked = {
  diagnostics : diagnostic list;
  nonterminals : string list;
  literals : string list;   (* symbols the grammar uses: if, skip, :=, ... *)
  keywords : string list;   (* literal words inside productions: if, then, while *)
  operator_nts : string list;   (* nonterminals that stand for an operator: op *)
}

let check src =
  let ds, lang = Toplevel.check src in
  {
    diagnostics =
      List.map
        (fun (d : Toplevel.diag) ->
          { severity = (match d.severity with Err -> "error" | Warning -> "warning");
            start = d.start; stop = d.stop; message = d.message })
        ds;
    nonterminals = List.map fst lang.grammar.nts;
    literals = List.sort_uniq compare (Grammar.literals lang.grammar);
    keywords =
      List.sort_uniq compare
        (List.concat_map
           (fun (_, ps) ->
             List.concat_map
               (function
                 | Grammar.PSeq items ->
                     List.filter_map
                       (function
                         | Grammar.Lit l when Lexer.is_ident_start l.[0] -> Some l
                         | _ -> None)
                       items
                 | _ -> [])
               ps)
           lang.grammar.nts);
    operator_nts =
      List.filter (fun x -> Grammar.op_lits lang.grammar x <> None) (List.map fst lang.grammar.nts);
  }

let outline src =
  match Toplevel.outline src with
  | Ok o -> Ok o
  | Error (tk, message) -> Error { line = tk.line; col = tk.col; message }

(* Problems in a program (a configuration), against the loaded language:
   syntax errors with their range, and subterms no production matches. *)
let check_program s src =
  let g = s.lang.grammar in
  let in_grammar t = List.exists (fun (nt, _) -> Grammar.mem g nt t) g.nts in
  (* The smallest construct that isn't in the grammar, if any. *)
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
                Printf.sprintf "%s doesn't match any production of the grammar"
                  (Grammar.to_string g u) } ])
  | exception Lexer.Error (tk, message) ->
      [ { severity = "error"; start = tk.start; stop = max tk.stop (tk.start + 1); message } ]
