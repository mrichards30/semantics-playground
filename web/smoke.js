// Drives the compiled runtime the way the UI will. Run by `dune test`.
const assert = require("assert");
const fs = require("fs");
const { circ } = require(require("path").resolve(process.argv[2]));

const src = fs.readFileSync(process.argv[3], "utf8").split("\nstep")[0];
const loaded = circ.load(src);
assert.ok(loaded.ok, JSON.stringify(loaded));

const start = circ.config("(x := 1 || x := 2, {x |-> 0})");
assert.ok(start.ok);
assert.strictEqual(circ.show(start.id), "(x := 1 || x := 2, {x |-> 0})");

// two branches, labelled by rule, with the rule's bindings
const succs = circ.successors(start.id);
assert.deepStrictEqual(succs.map((s) => s.rule), ["par1", "par2"]);
assert.strictEqual(circ.show(succs[0].target), "(skip || x := 2, {x |-> 1})");
assert.ok(succs[0].bindings.some(([x, v]) => x === "e1" && v === "x := 1"));

// equal configurations get the same id, so paths merge in the graph
const again = circ.config("(x := 1 || x := 2, {x |-> 0})");
assert.strictEqual(again.id, start.id);

const r = circ.run(start.id);
assert.deepStrictEqual(r.finals.map(circ.show).sort(),
  ["(skip, {x |-> 1})", "(skip, {x |-> 2})"]);

// errors come back with positions instead of throwing
const bad = circ.config("(x := , {})");
assert.strictEqual(bad.ok, false);
assert.strictEqual(bad.col, 7);

console.log("ok");
