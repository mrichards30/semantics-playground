# circular-imp — plan

A runtime for small-step operational semantics. Users write a grammar and
textbook-style inference rules (LaTeX-style, in a web UI); the runtime
registers them and lets users step a program interactively, seeing every
possible continuation. Nondeterminism (e.g. parallel composition) falls out of
the rules with no special support.

## Architecture

- **Core** (OCaml, no I/O): terms, patterns, grammars, rules, matcher, `step`.
- **Text front end** (internal): a textual rule/grammar syntax used for tests
  and debugging. Users may see it, but don't normally write it.
- **LaTeX front end**: a fixed subset (`\frac`, `\langle\rangle`, `\to`,
  `\to^*`, `\in`, `\mathrm{dom}`, `\mapsto`, subscripts, primes) compiled to
  the same core. Rendered back with KaTeX.
- **Web**: core compiled with `js_of_ocaml`, runs entirely client-side.

## Core data

```
term     = Int | Sym | Node of term list | Tuple of term list | Store | Hole
pattern  = term with variables, category-restricted by nonterminal name
           (e, e1, e', e_body), repeated names must be equal, contexts E[p]
premise  = Step  of pat * pat        (* (e1, s) -> (e1', s')  : forks per successor *)
         | Steps of pat * pat        (* (e, s) ->* (v, s')     : forks per final config *)
         | Bind  of pat * meta_expr  (* n = n1 + n2 : binds if unbound, else tests *)
         | Cond  of meta_expr        (* l in dom s *)
rule     = { name; lhs : pattern; rhs : pattern; premises : premise list }
grammar  = nonterminal -> productions, plus fixity declarations
```

## Semantics

- **Terms are inert data.** Operators inside an object term are symbols.
- **Rules:** `lhs -> rhs if premises`. Conclusions only rearrange; all
  computation happens in premises. A rule denotes the set
  `{ rhs | lhs matches c, premises }`; premises are read left to right, each
  `Step`/`Steps`/pattern match yields every solution.
- **`step c`** = union over all registered rules. Deterministic as a function;
  nondeterminism is just `step` being a relation.
- **Grammars** define nonterminals. Each nonterminal is a set (`t in e`), a
  pattern category, and — if it has a `[]` production — an evaluation
  context. Built-in sets: `int`, `sym`, `name` (symbols not used as literals
  or nonterminal names in the grammar), `store`.
- **Fixity declarations** + grammar drive a Pratt parser for object terms, so
  programs and rules need no defensive bracketing. Keyword-led productions
  (`if e then e else e`, `while e do e`) parse as prefix forms. A nonterminal
  whose productions are all operators may appear in operator position
  (`e1 op e2`).
- **Stores** are built-in finite maps: `{x |-> 0}`, `dom s`, `s(l)`,
  `s[l |-> n]`. Equality is order-independent.
- **Metalogic** (side conditions): ints, symbols, bools-as-symbols, stores,
  finite sets; `+ - * /`, unary `-`, `not`, chained `= < <= > >=`, `&&`, `||`,
  `x in S`, `if/then/else`, lazy order-independent `where`.
- **No runtime errors.** Anything ill-defined (`1 / 0`, `1 + true`, `s(l)`
  for `l` not in `dom s`) is `arb`. `arb` equals nothing (including itself),
  is in no set, and propagates through operations.

## Runtime API (for the UI)

- `successors : config -> (rule_name * bindings * config) list`
- `id : config -> string` — structural hash, so the UI can merge paths,
  show cycles, and build the configuration graph.
- `run : config -> config list` — run to completion (BFS with visited set),
  returning final configurations; also used for `->*` premises.
- Final configurations are split into values and stuck terms (`in v` or not).

UI: play follows one path (auto when there's one successor, asks when there
are several); branching = choosing another edge. No derivation trees for now
(the premise solver could keep them later at little cost).

## Example: L1 + parallel (text syntax)

```
grammar
  op ::= + | >=
  v  ::= n | true | false | skip
  e  ::= v | l | e op e | !l | l := e | e ; e | if e then e else e
       | while e do e | e || e
  l  ::= name     n ::= int     s ::= store

fixity  infixr 0 ||   infixr 1 ;   infix 2 :=   infix 3 >=   infixl 4 +   prefix 5 !

rule op+     (n1 + n2, s)  -> (n, s)           if n = n1 + n2
rule op>=    (n1 >= n2, s) -> (v, s)           if v = (n1 >= n2)
rule op1     (e1 op e2, s) -> (e1' op e2, s')  if (e1, s) -> (e1', s')
rule op2     (v op e2, s)  -> (v op e2', s')   if (e2, s) -> (e2', s')
rule deref   (!l, s)       -> (n, s)           if l in dom s, n = s(l)
rule assign1 (l := n, s)   -> (skip, s')       if l in dom s, s' = s[l |-> n]
rule assign2 (l := e, s)   -> (l := e', s')    if (e, s) -> (e', s')
rule seq1    (skip ; e2, s) -> (e2, s)
rule seq2    (e1 ; e2, s)  -> (e1' ; e2, s')   if (e1, s) -> (e1', s')
rule if1     (if true then e2 else e3, s)  -> (e2, s)
rule if2     (if false then e2 else e3, s) -> (e3, s)
rule if3     (if e1 then e2 else e3, s) -> (if e1' then e2 else e3, s')
               if (e1, s) -> (e1', s')
rule while   (while e1 do e2, s) -> (if e1 then (e2 ; while e1 do e2) else skip, s)

rule par1    (e1 || e2, s) -> (e1' || e2, s')  if (e1, s) -> (e1', s')
rule par2    (e1 || e2, s) -> (e1 || e2', s')  if (e2, s) -> (e2', s')
rule par3    (skip || e, s) -> (e, s)
rule par4    (e || skip, s) -> (e, s)
```

`run (x := 1 || x := 2, {x |-> 0})` → `{(skip, {x |-> 1}), (skip, {x |-> 2})}`.

Evaluation-context style (`E ::= [] | E + e | v + E | ...` with `E[n1 + n2]`)
is supported too and compiles to the same `step`.

## Build order

1. ~~Core: terms, grammar-driven Pratt reader, matcher (categories, repeated
   names, contexts), premise solver, `successors`, `run`.~~ Done.
2. ~~Text front end + tests: L1 (Sewell), L1 + parallel, context-style L1,
   metalogic (`arb`, chained comparisons, stores, sets).~~ Done (`test/`).
3. ~~`js_of_ocaml` build exposing the runtime API.~~ Done (`web/circ_js.ml`,
   checked by `web/smoke.js` under `dune test`).
4. Minimal UI: play/step/branch, trace, run all — done (`dune build @site`,
   open `_build/default/web/index.html`). Still to do: configuration graph.
5. LaTeX rule editor.

Development uses a project-local opam switch (`_opam/`, OCaml 5.5):
`opam switch create . ocaml-base-compiler.5.5.0 && opam install dune js_of_ocaml-compiler js_of_ocaml`.

## Open

- Whether users ever need helper functions (`fun`) in the web UI.
- Exact LaTeX subset and how the grammar is entered (BNF in LaTeX?).
