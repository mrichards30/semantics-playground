// Builds examples.js from the test .sem files: the language is everything
// before the first command; the program is from a trace, run, query or step.
const fs = require("fs");
const path = require("path");

const titles = {
  calc: "calculator", l1: "L1", par: "L1 + parallel", bigstep: "L1, big-step", ccs: "pure CCS", gcl: "GCL",
  ctx: "L1, evaluation contexts", ops: "user-defined operators",
};
const command = /^(run|trace|step|query|show)\b/;

// A file may offer more programs as extra examples, each marked by a
// "-- example: name" line just before its command.
const examples = process.argv.slice(2).flatMap((file) => {
  const lines = fs.readFileSync(file, "utf8").split("\n");
  const first = lines.findIndex((l) => command.test(l));
  const language = lines.slice(0, first === -1 ? lines.length : first).join("\n").trimEnd() + "\n";
  // the most interesting program: a trace or run if there is one
  const prog = ["trace", "run", "query", "step"].map((c) => lines.find((l) => l.startsWith(c + " "))).find(Boolean);
  const name = path.basename(file, ".sem");
  // a query's input is its judgment without the relation and the ?
  const input = (l) => l.replace(/^(run|trace|step|query)\s+/, "").replace(/\s+\S+\s+\?\s*$/, "");
  const extra = [];
  lines.forEach((l, i) => {
    const m = l.match(/^-- example: (.*)$/);
    const cmd = m && lines.slice(i + 1).find((x) => command.test(x));
    if (cmd) extra.push({ name: m[1], language, program: input(cmd) });
  });
  return [{ name: titles[name] || name, language, program: prog ? input(prog) : "" }, ...extra];
});

process.stdout.write("const EXAMPLES = " + JSON.stringify(examples, null, 2) + ";\n");
