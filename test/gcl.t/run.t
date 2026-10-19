  $ semplay gcl.sem
  step (do x > y -> if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi od, {x |-> 0, y |-> -1})
    loop2: (if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi ; do x > y -> if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi od, {x |-> 0, y |-> -1})
  step (if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi ; do x > y -> if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi od, {x |-> 0, y |-> -1})
    seq2: (y := y - 1 ; do x > y -> if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi od, {x |-> 0, y |-> -1})
    seq2: (x := x - 1 ; do x > y -> if x - y <= 5 -> y := y - 1 [] x <= 0 -> x := x - 1 fi od, {x |-> 0, y |-> -1})
  run (do x > y -> x := x - y [] y > x -> y := y - x od, {x |-> 12, y |-> 18})
    (skip, {x |-> 6, y |-> 6})
