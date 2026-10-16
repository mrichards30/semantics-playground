(* JavaScript bindings for the runtime: exports a `circ` object.

     circ.load(src)       -> {ok, output: [string]} | {ok: false, line, col, message}
     circ.config(src)     -> {ok, id} | {ok: false, line, col, message}
     circ.show(id)        -> string
     circ.successors(id)  -> [{rule, target, bindings: [[name, value]]}]
     circ.run(id)         -> {finals: [id], states, truncated}
     circ.rules()         -> [string]
     circ.check(src)      -> {diagnostics: [{severity, start, stop, message}],
                              nonterminals: [string], literals: [string]}
     circ.outline(src)    -> {ok, grammar: [string], fixity: [string],
                              rules: [{name, conclusion, premises: [string]}]} | error

   Source offsets are UTF-8 byte offsets; the UI converts them to string
   indices. *)

open Js_of_ocaml
module Api = Circ.Api

let session = Api.create ()
let obj fields = Js.Unsafe.obj (Array.of_list fields)
let inj = Js.Unsafe.inject
let str s = inj (Js.string s)
let arr l = inj (Js.array (Array.of_list l))

let result ok = function
  | Ok v -> obj (("ok", inj Js._true) :: ok v)
  | Error (e : Api.error) ->
      obj
        [ ("ok", inj Js._false); ("line", inj e.line); ("col", inj e.col);
          ("message", str e.message) ]

let fn f = inj (Js.wrap_callback f)

let () =
  Js.export "circ"
    (obj
       [ ( "load",
           fn (fun src ->
               result
                 (fun lines -> [ ("output", arr (List.map str lines)) ])
                 (Api.load session (Js.to_string src))) );
         ( "config",
           fn (fun src ->
               result (fun i -> [ ("id", inj i) ]) (Api.config session (Js.to_string src))) );
         ("show", fn (fun i -> Js.string (Api.show session i)));
         ( "successors",
           fn (fun i ->
               Js.array
                 (Array.of_list
                    (List.map
                       (fun (s : Api.successor) ->
                         obj
                           [ ("rule", str s.rule); ("target", inj s.target);
                             ( "bindings",
                               arr (List.map (fun (x, v) -> arr [ str x; str v ]) s.bindings) ) ])
                       (Api.successors session i)))) );
         ( "check",
           fn (fun src ->
               let c = Api.check (Js.to_string src) in
               obj
                 [ ( "diagnostics",
                     arr
                       (List.map
                          (fun (d : Api.diagnostic) ->
                            obj
                              [ ("severity", str d.severity); ("start", inj d.start);
                                ("stop", inj d.stop); ("message", str d.message) ])
                          c.diagnostics) );
                   ("nonterminals", arr (List.map str c.nonterminals));
                   ("literals", arr (List.map str c.literals));
                   ("keywords", arr (List.map str c.keywords));
                   ("operatorNonterminals", arr (List.map str c.operator_nts)) ]) );
         ( "outline",
           fn (fun src ->
               result
                 (fun (o : Circ.Toplevel.outline) ->
                   [ ("grammar", arr (List.map str o.grammar));
                     ("fixity", arr (List.map str o.fixity));
                     ( "rules",
                       arr
                         (List.map
                            (fun (r : Circ.Toplevel.rule_parts) ->
                              obj
                                [ ("name", str r.name); ("conclusion", str r.conclusion);
                                  ("premises", arr (List.map str r.premises)) ])
                            o.rules) ) ])
                 (Api.outline (Js.to_string src))) );
         ("rules", fn (fun () -> arr (List.map str (Api.rule_names session))));
         ( "run",
           fn (fun i ->
               let r = Api.run session i in
               obj
                 [ ("finals", arr (List.map inj r.finals)); ("states", inj r.states);
                   ("truncated", inj (Js.bool r.truncated)) ]) ) ])
