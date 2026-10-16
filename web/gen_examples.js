// Builds examples.js from the test .sem files: the language is everything
// before the first command, the program is the first run/trace/step.
const fs = require("fs");
const path = require("path");

const titles = { l1: "L1", par: "L1 + parallel", ctx: "L1, evaluation contexts", ops: "user-defined operators" };
const command = /^(run|trace|step|show)\b/;

const examples = process.argv.slice(2).map((file) => {
  const lines = fs.readFileSync(file, "utf8").split("\n");
  const first = lines.findIndex((l) => command.test(l));
  const language = lines.slice(0, first === -1 ? lines.length : first).join("\n").trimEnd() + "\n";
  const prog = lines.find((l) => /^(run|trace|step)\b/.test(l));
  const name = path.basename(file, ".sem");
  return {
    name: titles[name] || name,
    language,
    program: prog ? prog.replace(/^(run|trace|step)\s+/, "") : "",
  };
});

process.stdout.write("const EXAMPLES = " + JSON.stringify(examples, null, 2) + ";\n");
