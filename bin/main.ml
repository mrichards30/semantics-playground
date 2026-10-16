let () =
  match Sys.argv with
  | [| _; path |] -> (
      let src = In_channel.with_open_bin path In_channel.input_all in
      try Circ.Toplevel.run_string (Circ.Rules.create ()) src with
      | Circ.Lexer.Error (line, col, msg) ->
          Printf.eprintf "%s:%d:%d: %s\n" path line col msg;
          exit 1)
  | _ ->
      prerr_endline "usage: circ FILE";
      exit 2
