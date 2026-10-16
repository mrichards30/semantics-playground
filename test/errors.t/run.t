Rules are checked when they are loaded.

  $ cat > unbound.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > rule bad (e1 + e2) -> e3
  > X
  $ circ unbound.sem
  unbound.sem:4:6: rule bad: 'e3' in the conclusion is never bound
  [1]

  $ cat > notnt.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > rule bad (n1 + n2) -> b if b = n1 + n2
  > X
  $ circ notnt.sem
  notnt.sem:4:28: 'b' is not named after a nonterminal, so it can't be a pattern variable
  [1]

  $ cat > syntax.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > step (1 + )
  > X
  $ circ syntax.sem
  syntax.sem:4:11: expected a term but found ')'
  [1]
