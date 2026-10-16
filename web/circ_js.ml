(* JavaScript bindings for the runtime: exports a `circ` object.

     circ.load(src)       -> {ok, output: [string]} | {ok: false, line, col, message}
     circ.config(src)     -> {ok, id} | {ok: false, line, col, message}
     circ.show(id)        -> string
     circ.successors(id)  -> [{rule, target, bindings: [[name, value]]}]
     circ.run(id)         -> {finals: [id], states, truncated}
     circ.rules()         -> [string] *)

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
         ("rules", fn (fun () -> arr (List.map str (Api.rule_names session))));
         ( "run",
           fn (fun i ->
               let r = Api.run session i in
               obj
                 [ ("finals", arr (List.map inj r.finals)); ("states", inj r.states);
                   ("truncated", inj (Js.bool r.truncated)) ]) ) ])
