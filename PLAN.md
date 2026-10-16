# circular-imp — plan

A player for inductive relations. Users write a grammar and textbook-style
inference rules; the runtime registers them and lets users explore what the
rules derive, seeing every possibility. The system has no built-in notion of
small-step, big-step or typing: users declare the relations they want
(`->`, `⇓`, `⊢ :`, …) and the rules that define them. Nondeterminism (e.g.
parallel composition) falls out of the rules with no special support.

## Architecture

- **Core** (OCaml, no I/O): terms, patterns, grammars, relations, rules,
  matcher, solver, checker.
- **Text front end** (internal): the `.sem` syntax, used for tests and
  debugging and as the storage format. Users may see it (the text tab) but
  don't normally write it.
- **Rule editor** (web): each rule is a card typeset like a textbook rule
  (KaTeX); clicking edits its slots as plain text, checked live with
  problems underlined in place.
- **Web**: core compiled with `js_of_ocaml`, runs entirely client-side.

## Relations

A relation is declared by its notation, the sorts of its arguments (grammar
nonterminals), and a mode saying which arguments are inputs and which are
outputs:

```
relation (e, s) -> (e, s)      mode (in, out)      -- small-step
relation (e, s) ⇓ (v, s)       mode (in, out)      -- big-step
relation G |- e : t            mode (in, in, out)  -- typing (inference)
```

(Exact declaration syntax is open; see below.)

- **Rules conclude any declared relation.** Premises are judgments of any
  relation, or side conditions in the metalogic. A small-step rule may use a
  big-step premise (e.g. `(b, s) ⇓ (true, s)` inside a `while` rule) just by
  using that relation.
- **Solving is mode-directed:** `solve R inputs` returns the set of outputs.
  Match the conclusion's inputs, solve premises left to right (each premise
  yields every solution), build the conclusion's outputs. This is what `step`
  does today for `->`; it generalises to every relation unchanged. Results are
  memoised per (relation, inputs).
- **Mode checking** (static): every input of a premise must be bound by the
  time it is solved, and every output of the conclusion must be bound by the
  end. Generalises today's "used before it is bound" / "never bound" errors.
- **Sort checking** (static): every judgment's arguments must fit its
  relation's sorts, against the user's grammar.
- **No built-in closures.** There is no `->*`. The player iterates relations
  itself (play, run all); textbook uses of `→*` are almost all in theorems
  and definitions *about* a semantics, not in its rules. If a rule ever needs
  multi-step evaluation (e.g. STM-style `atomic`), it can be revisited.

## The player

What "playing" means depends on the relation's shape, not on what kind of
semantics it is:

- **Iterable relations** (input and output of the same sort, e.g. `->` on
  configurations): the current stepper. Play follows one path, choosing at
  branches at random or by asking; run all explores every reachable
  configuration and lists the final ones; the trace records the path.
- **One-shot relations** (e.g. `⇓`, `⊢ :`): a derivation view. Start from a
  goal (`(while …, s) ⇓ ?`), show the derivation tree as it is built, and let
  the user choose where more than one rule applies. Shows all solutions.
- Derivation trees become the general concept: each small step also has one
  (`seq2` because `assign1` …), shown on demand under a step.
- **Termination:** search is fair (breadth-first over choices) with a depth /
  fuel limit, reported as such. A diverging big-step program has no
  derivation, and the player shows the search running out of fuel — a real
  property of big-step semantics worth seeing.

## Core data

```
term     = Int | Sym | Node of term list | Tuple of term list | Store | Set | Hole | Arb
pattern  = term with variables, category-restricted by nonterminal name
           (e, e1, e', e_body), repeated names must be equal, contexts E[p]
relation = { notation; sorts : nonterminal list; mode : (In | Out) list }
judgment = relation applied to patterns
premise  = Judgment of judgment          (* forks per solution *)
         | Bind of var * meta_expr       (* n = n1 + n2 : binds if unbound, else tests *)
         | Cond of meta_expr             (* l in dom s *)
rule     = { name; conclusion : judgment; premises : premise list }
grammar  = nonterminal -> productions, plus fixity declarations
```

## Semantics of the rest

- **Terms are inert data.** Operators inside an object term are symbols.
- **Conclusions only rearrange; all computation happens in premises.**
- **Grammars** define nonterminals. Each nonterminal is a set (`t in e`), a
  pattern category, a relation sort, and — if it has a `[]` production — an
  evaluation context. Built-in sets: `int`, `sym`, `name` (symbols not used
  as literals or nonterminal names in the grammar), `store`.
- **Fixity declarations** + grammar drive a Pratt parser for object terms.
  The grammar declares the object language's operators; the runtime has none
  built in.
- **Stores** are built-in finite maps: `{x |-> 0}`, `dom s`, `s(l)`,
  `s[l |-> n]`. Equality is order-independent. (Likely renamed `map`, since
  typing environments are the same thing.)
- **Metalogic** (side conditions): ints, symbols, bools-as-symbols, stores,
  finite sets; `+ - * /`, unary `-`, `not`, chained `= < <= > >=`, `&&`, `||`,
  `x in S`, `if/then/else`, `dom s`, `s(l)`, lazy order-independent `where`.
- **No runtime errors.** Anything ill-defined (`1 / 0`, `1 + true`, `s(l)`
  for `l` not in `dom s`) is `arb`. `arb` equals nothing (including itself),
  is in no set, and propagates through operations.

## Examples (intended)

Small-step L1 + parallel — works today, with `->` implicit:

```
grammar
  op ::= + | >=
  v  ::= n | true | false | skip
  e  ::= v | l | e op e | !l | l := e | e ; e | if e then e else e
       | while e do e | e || e
  l  ::= name     n ::= int     s ::= store

fixity  infixr 0 ||   infixr 1 ;   infix 2 :=   infix 3 >=   infixl 4 +   prefix 5 !

rule op+     (n1 + n2, s)  -> (n, s)           if n = n1 + n2
rule op1     (e1 op e2, s) -> (e1' op e2, s')  if (e1, s) -> (e1', s')
rule seq2    (e1 ; e2, s)  -> (e1' ; e2, s')   if (e1, s) -> (e1', s')
rule while   (while e1 do e2, s) -> (if e1 then (e2 ; while e1 do e2) else skip, s)
rule par1    (e1 || e2, s) -> (e1' || e2, s')  if (e1, s) -> (e1', s')
...
```

Big-step L1 — the same grammar, a different relation:

```
relation (e, s) ⇓ (v, s)  mode (in, out)

rule num    (n, s) ⇓ (n, s)
rule plus   (e1 + e2, s) ⇓ (n, s'')  if (e1, s) ⇓ (n1, s'), (e2, s') ⇓ (n2, s''), n = n1 + n2
rule whileT (while e1 do e2, s) ⇓ (skip, s''')
              if (e1, s) ⇓ (true, s'), (e2, s') ⇓ (skip, s''), (while e1 do e2, s'') ⇓ (skip, s''')
rule whileF (while e1 do e2, s) ⇓ (skip, s')  if (e1, s) ⇓ (false, s')
```

Typing:

```
grammar  t ::= int | bool | unit     G ::= store

relation G |- e : t  mode (in, in, out)

rule tInt   G |- n : int
rule tPlus  G |- e1 + e2 : int  if G |- e1 : int, G |- e2 : int
rule tIf    G |- if e1 then e2 else e3 : t  if G |- e1 : bool, G |- e2 : t, G |- e3 : t
```

## Status

Done:

1. Core: terms, grammar-driven Pratt reader, matcher (categories, repeated
   names, contexts), premise solver, `successors`, `run` — for a single
   built-in relation `->`.
2. Text front end + cram tests: L1 (Sewell), L1 + parallel, context-style
   L1, user-defined operators, metalogic, checker diagnostics.
3. `js_of_ocaml` build and session API (`web/circ_js.ml`, smoke-tested under
   `dune test`).
4. Web UI: language page (rule cards, live diagnostics, text tab) and run
   page (play / step / branch policy, trace, run all).

Next:

1. Remove the built-in `->*` premise (`Steps` in `lib/rules.ml`); it is
   untested and no longer part of the design.
2. Relation declarations (notation, sorts, modes); rules over any relation;
   `->` becomes an ordinary declared relation.
3. Generic mode-directed `solve`, with mode and sort checking in the checker.
4. Player: iterate view for same-sort relations (today's stepper), derivation
   view for one-shot relations; fuel limits.
5. Configuration graph for the iterate view.

Development uses a project-local opam switch (`_opam/`, OCaml 5.5):
`opam switch create . ocaml-base-compiler.5.5.0 && opam install dune js_of_ocaml-compiler js_of_ocaml`.

## Open

- Relation declaration syntax, and whether modes are explicit or have a
  default (e.g. inputs left of the relation symbol).
- How the derivation view handles large or infinite searches (lazy
  expansion, fuel settings).
- Negated premises ("no rule applies", "is not a value"): useful, but needs
  stratification; left out for now.
- Rename `store` to `map`.
- Whether users ever need helper functions (`fun`).
