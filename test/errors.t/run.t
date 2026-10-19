Rules are checked when they are loaded.

  $ cat > unbound.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > relation e -> e
  > rule bad (e1 + e2) -> e3
  > X
  $ semplay unbound.sem
  unbound.sem:5:23: 'e3' in the conclusion is never bound
  [1]

  $ cat > notnt.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > relation e -> e
  > rule bad (n1 + n2) -> b if b = n1 + n2
  > X
  $ semplay notnt.sem
  notnt.sem:5:28: 'b' is not named after a nonterminal, so it can't be a pattern variable
  [1]

  $ cat > syntax.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > relation e -> e
  > step (1 + )
  > X
  $ semplay syntax.sem
  syntax.sem:5:11: expected a term but found ')'
  [1]
