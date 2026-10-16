The checker reports every problem, with positions, and keeps going after
errors. Good files are clean:

  $ circ --check ../l1.t/l1.sem
  $ circ --check ../par.t/par.sem
  $ circ --check ../ctx.t/ctx.sem

Several mistakes in one file:

  $ cat > bad.sem <<'X'
  > grammar
  >   v  ::= n | true | false
  >   e  ::= v | e + e | e e | if e then e else e | if e then e
  >   n  ::= int
  >   s  ::= store
  > fixity infixl 4 + infixl 5 ++
  > rule plus (n1 + n2, s) -> (n, s) if n = n1 + nn
  > rule ifx (if tru then e1 else e2, s) -> (e1, s)
  > rule bad1 (e1 + e2, s) -> (e3, s)
  > rule bad2 (e1 + , s) -> (e1, s)
  > rule seq (e1 + e2, s) -> (e1' + e2, s') if (e3, s) -> (e1', s')
  > rule plus (n1 + n2, s) -> (n1, s)
  > rule bind (n1 + n2, s) -> (n1, s) if b = 1
  > X
  $ circ --check bad.sem
  bad.sem:3:22: warning: two subterms in a row can't be parsed; put a symbol between them
  bad.sem:3:49: warning: another production already starts with 'if'; only that one is used for parsing
  bad.sem:6:28: warning: '++' is not a symbol of the grammar
  bad.sem:7:46: warning: 'nn' is not bound here, so it is arb
  bad.sem:8:14: warning: 'tru' is not a nonterminal or a symbol of the grammar, so it only matches the symbol tru
  bad.sem:9:28: error: 'e3' in the conclusion is never bound
  bad.sem:10:17: error: expected a term but found ','
  bad.sem:11:45: error: 'e3' is used before it is bound
  bad.sem:12:6: warning: there is already a rule called plus
  bad.sem:13:38: error: 'b' is not named after a nonterminal, so it can't be a pattern variable
  [1]

Stray text after a conclusion or between premises:

  $ cat > trailing.sem <<'X'
  > grammar
  >   e ::= n | e + e
  >   n ::= int
  > rule a (n1 + n2) -> n junk if n = n1 + n2
  > rule b (n1 + n2) -> n if n = n1 + n2 n = 3
  > X
  $ circ --check trailing.sem
  trailing.sem:4:23: error: unexpected 'junk' after the conclusion; premises start with 'if'
  trailing.sem:5:38: error: unexpected 'n'; premises are separated by commas
  [1]
