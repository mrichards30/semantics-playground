(* JavaScript bindings for the runtime: exports a `circ` object.

     circ.load(src)            -> {ok, output: [string]} | {ok: false, line, col, message}
     circ.config(src)          -> {ok, id} | error
     circ.show(id)             -> string        (text)
     circ.latex(id)            -> string        (LaTeX)
     circ.latexDiff(id, other, cls) -> LaTeX of id, parts differing from other in \htmlClass{cls}
     circ.relations()          -> [{index, notation, shown, steps, inputs}]
     circ.successors(rel, id)  -> [{rule, target, labels: [latex], why}]
     circ.solve(rel, id)       -> {solutions: [{outputs: [latex], outputIds, proof}], outOfFuel}
     circ.run(rel, id)         -> {finals: [id], states, truncated}
     circ.rules()              -> [string]
     circ.check(src)           -> {diagnostics: [{severity, start, stop, message}],
                                   nonterminals, literals, keywords, operatorNonterminals,
                                   grammarTex, relationsTex: [latex],
                                   rulesTex: [{from, until, tex}]}
     circ.outline(src)         -> {ok, grammar, fixity, relations: [string],
                                   rules: [{name, conclusion, premises}]} | error
     circ.checkProgram(src)    -> [{severity, start, stop, message}]

   A derivation (why, proof) is {rule, judgment: latex, premises: [derivation]}.
   Source offsets are UTF-8 byte offsets; the UI converts them to string
   indices. *)

open Js_of_ocaml
module Api = Circ.Api

let session = Api.create ()
let obj fields = Js.Unsafe.obj (Array.of_list fields)
let inj = Js.Unsafe.inject
let str s = inj (Js.string s)
let arr l = inj (Js.array (Array.of_list l))
let strs l = arr (List.map str l)
let fn f = inj (Js.wrap_callback f)

let result ok = function
  | Ok v -> obj (("ok", inj Js._true) :: ok v)
  | Error (e : Api.error) ->
      obj [ ("ok", inj Js._false); ("line", inj e.line); ("col", inj e.col); ("message", str e.message) ]

let rec derivation (d : Api.derivation) =
  obj [ ("rule", str d.rule); ("judgment", str d.judgment); ("premises", arr (List.map derivation d.premises)) ]

let diagnostics ds =
  arr
    (List.map
       (fun (d : Api.diagnostic) ->
         obj [ ("severity", str d.severity); ("start", inj d.start); ("stop", inj d.stop); ("message", str d.message) ])
       ds)

let () =
  Js.export "circ"
    (obj
       [ ( "load",
           fn (fun src -> result (fun lines -> [ ("output", strs lines) ]) (Api.load session (Js.to_string src))) );
         ("config", fn (fun src -> result (fun i -> [ ("id", inj i) ]) (Api.config session (Js.to_string src))));
         ("show", fn (fun i -> Js.string (Api.show session i)));
         ("latex", fn (fun i -> Js.string (Api.latex session i)));
         ( "latexDiff",
           fn (fun i j cls -> Js.string (Api.latex_diff session i j (Js.to_string cls))) );
         ( "relations",
           fn (fun () ->
               arr
                 (List.map
                    (fun (r : Api.relation_info) ->
                      obj
                        [ ("index", inj r.index); ("notation", str r.notation); ("form", str r.form);
                          ("shown", str r.shown);
                          ("steps", inj (Js.bool r.steps)); ("inputs", inj r.inputs) ])
                    (Api.relations session))) );
         ( "successors",
           fn (fun rel i ->
               arr
                 (List.map
                    (fun (s : Api.successor) ->
                      obj
                        [ ("rule", str s.rule_name); ("target", inj s.target); ("labels", strs s.labels);
                          ("why", derivation s.why) ])
                    (Api.successors session rel i))) );
         ( "solve",
           fn (fun rel i ->
               let r = Api.solve session rel i in
               obj
                 [ ( "solutions",
                     arr
                       (List.map
                          (fun (s : Api.solution) ->
                            obj
                              [ ("outputs", strs s.outputs); ("outputIds", arr (List.map inj s.output_ids));
                                ("proof", derivation s.proof) ])
                          r.solutions) );
                   ("outOfFuel", inj (Js.bool r.out_of_fuel)) ]) );
         ( "run",
           fn (fun rel i ->
               let r = Api.run session rel i in
               obj
                 [ ("finals", arr (List.map inj r.finals)); ("states", inj r.states);
                   ("truncated", inj (Js.bool r.truncated)) ]) );
         ("rules", fn (fun () -> strs (Api.rule_names session)));
         ( "check",
           fn (fun src ->
               let c = Api.check (Js.to_string src) in
               obj
                 [ ("diagnostics", diagnostics c.diagnostics);
                   ("nonterminals", strs c.nonterminals); ("literals", strs c.literals);
                   ("keywords", strs c.keywords); ("operatorNonterminals", strs c.operator_nts);
                   ("grammarTex", str c.grammar_tex); ("relationsTex", strs c.relations_tex);
                   ( "rulesTex",
                     arr
                       (List.map
                          (fun (r : Api.typeset_rule) ->
                            obj [ ("from", inj r.from); ("until", inj r.until); ("tex", str r.tex) ])
                          c.rules_tex) ) ]) );
         ( "outline",
           fn (fun src ->
               result
                 (fun (o : Circ.Toplevel.outline) ->
                   [ ("grammar", strs o.grammar); ("fixity", strs o.fixity); ("relations", strs o.relations);
                     ( "rules",
                       arr
                         (List.map
                            (fun (r : Circ.Toplevel.rule_parts) ->
                              obj
                                [ ("name", str r.name); ("conclusion", str r.conclusion);
                                  ("premises", strs r.premises) ])
                            o.rules) ) ])
                 (Api.outline (Js.to_string src))) );
         ("checkProgram", fn (fun src -> diagnostics (Api.check_program session (Js.to_string src)))) ])
