// Drives the compiled runtime the way the UI will. Run by `dune test`.
const assert = require("assert");
const fs = require("fs");
const { circ } = require(require("path").resolve(process.argv[2]));
const language = (file) => fs.readFileSync(file, "utf8").split(/\n(?=step|run|trace|query)/)[0];

// small-step: a stepping relation with branches
assert.ok(circ.load(language(process.argv[3])).ok);
const rels = circ.relations();
assert.strictEqual(rels.length, 1);
assert.ok(rels[0].steps);
const start = circ.config("(x := 1 || x := 2, {x |-> 0})");
assert.ok(start.ok);
const succs = circ.successors(0, start.id);
assert.deepStrictEqual(succs.map((s) => s.rule), ["par1", "par2"]);
assert.strictEqual(circ.show(succs[0].target), "(skip || x := 2, {x |-> 1})");
assert.strictEqual(succs[0].why.rule, "par1");
assert.strictEqual(succs[0].why.premises[0].rule, "assign1");
assert.match(circ.latex(start.id), /\\langle .*\\parallel.*\\mapsto/);
// equal configurations get the same id, so paths merge
assert.strictEqual(circ.config("(x := 1 || x := 2, {x |-> 0})").id, start.id);
const r = circ.run(0, start.id);
assert.deepStrictEqual(r.finals.map(circ.show).sort(), ["(skip, {x |-> 1})", "(skip, {x |-> 2})"]);
// errors come back with positions instead of throwing
const bad = circ.config("(x := , {})");
assert.strictEqual(bad.ok, false);
assert.strictEqual(bad.col, 7);

// big-step: solved, not stepped
assert.ok(circ.load(language(process.argv[4])).ok);
assert.strictEqual(circ.relations()[0].steps, false);
const prog = circ.config("(x := !x + 1 ; !x, {x |-> 41})");
const sol = circ.solve(0, prog.id);
assert.strictEqual(sol.solutions.length, 1);
assert.strictEqual(circ.show(sol.solutions[0].outputIds[0]), "(42, {x |-> 42})");
assert.strictEqual(sol.solutions[0].proof.rule, "seq");
assert.strictEqual(circ.solve(0, circ.config("(while true do skip, {})").id).solutions.length, 0);

// CCS: a labelled transition system steps, with actions as labels
assert.ok(circ.load(language(process.argv[5])).ok);
const lts = circ.relations().findIndex((x) => x.steps);
const uni = circ.config("(CM | CS) \\ coin \\ coffee");
const moves = circ.successors(lts, uni.id);
assert.strictEqual(moves.length, 1);
assert.match(moves[0].labels[0], /\\overline/);
const checked = circ.check(language(process.argv[5]));
assert.strictEqual(checked.diagnostics.length, 0, JSON.stringify(checked.diagnostics));
assert.match(checked.rulesTex.find((t) => /com1/.test(t.tex)).tex, /\\xrightarrow\{\{\{\\tau\}\}\}/);

console.log("ok");
