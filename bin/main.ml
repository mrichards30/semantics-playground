let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  match Sys.argv with
  | [| _; path |] -> (
      try Circ.Toplevel.run_string (Circ.Rules.create ()) (read path) with
      | Circ.Lexer.Error (tk, msg) ->
          Printf.eprintf "%s:%d:%d: %s\n" path tk.line tk.col msg;
          exit 1)
  | [| _; "--check"; path |] ->
      let ds, _, _ = Circ.Toplevel.check (read path) in
      List.iter
        (fun (d : Circ.Toplevel.diag) ->
          Printf.printf "%s:%d:%d: %s: %s\n" path d.line d.col
            (match d.severity with Err -> "error" | Warning -> "warning")
            d.message)
        ds;
      if List.exists (fun (d : Circ.Toplevel.diag) -> d.severity = Err) ds then exit 1
  | [| _; "--latex"; path |] ->
      (* how the web UI typesets the file's grammar, relations and rules *)
      let _, lang, rules = Circ.Toplevel.check (read path) in
      let g = lang.Circ.Rules.grammar in
      print_endline (Circ.Latex.grammar g);
      List.iter (fun r -> print_endline (Circ.Latex.relation g r)) lang.Circ.Rules.relations;
      List.iter (fun (r, _, _) -> print_endline (Circ.Latex.rule g r)) rules
  | _ ->
      prerr_endline "usage: circ FILE | circ --check FILE | circ --latex FILE";
      exit 2
