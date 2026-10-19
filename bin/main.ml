let read path = In_channel.with_open_bin path In_channel.input_all

let () =
  match Sys.argv with
  | [| _; path |] -> (
      try Semplay.Toplevel.run_string (Semplay.Rules.create ()) (read path) with
      | Semplay.Lexer.Error (tk, msg) ->
          Printf.eprintf "%s:%d:%d: %s\n" path tk.line tk.col msg;
          exit 1)
  | [| _; "--check"; path |] ->
      let ds, _, _ = Semplay.Toplevel.check (read path) in
      List.iter
        (fun (d : Semplay.Toplevel.diag) ->
          Printf.printf "%s:%d:%d: %s: %s\n" path d.line d.col
            (match d.severity with Err -> "error" | Warning -> "warning")
            d.message)
        ds;
      if List.exists (fun (d : Semplay.Toplevel.diag) -> d.severity = Err) ds then exit 1
  | [| _; "--latex"; path |] ->
      (* how the web UI typesets the file's grammar, relations and rules *)
      let _, lang, rules = Semplay.Toplevel.check (read path) in
      let g = lang.Semplay.Rules.grammar in
      print_endline (Semplay.Latex.grammar g);
      List.iter (fun r -> print_endline (Semplay.Latex.relation g r)) lang.Semplay.Rules.relations;
      List.iter (fun (r, _, _) -> print_endline (Semplay.Latex.rule g r)) rules
  | _ ->
      prerr_endline "usage: semplay FILE | semplay --check FILE | semplay --latex FILE";
      exit 2
