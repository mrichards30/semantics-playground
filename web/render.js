"use strict";

// Turns the ASCII rule syntax into LaTeX, so rules display the way a
// textbook prints them: (e1, s) -> (e1', s')  becomes  ⟨e₁, s⟩ → ⟨e₁′, s′⟩.
// Everything the renderer needs to know about the language (which names are
// nonterminals, which words are keywords) comes from the checker.

const Render = (() => {
  const OWN_OPS = ["|->", "->*", "::=", "->", "<=", ">=", "<>", "!=", "&&", "||"];
  const OP_CHARS = "+-*/<>=!:;|&^~.?@#$%\\";
  const isOpChar = (c) => OP_CHARS.includes(c);
  const isIdStart = (c) => /[A-Za-z_]/.test(c);
  const isIdChar = (c) => /[A-Za-z0-9_']/.test(c);

  const TEX = {
    "->": "\\longrightarrow", "->*": "\\longrightarrow^{*}", "|->": "\\mapsto",
    ">=": "\\geq", "<=": "\\leq", "<>": "\\neq", "!=": "\\neq",
    "::=": "\\mathrel{::=}", ":=": "\\mathrel{:=}", "&&": "\\land",
    "|": "\\mid", ";": "\\mathbin{;}", "=": "=", "<": "<", ">": ">",
    "+": "+", "-": "-", "*": "\\times", "/": "/", "!": "{!}",
  };

  function tokenize(src, ops) {
    const known = [...new Set([...OWN_OPS, ...ops])].sort((a, b) => b.length - a.length);
    const toks = [];
    let i = 0;
    while (i < src.length) {
      const c = src[i];
      if (/\s/.test(c)) { i++; continue; }
      let j = i + 1;
      if (/[0-9]/.test(c)) {
        while (j < src.length && /[0-9]/.test(src[j])) j++;
        toks.push({ kind: "int", text: src.slice(i, j) });
      } else if (isIdStart(c)) {
        while (j < src.length && isIdChar(src[j])) j++;
        toks.push({ kind: "id", text: src.slice(i, j) });
      } else if (isOpChar(c)) {
        const op = known.find((o) => src.startsWith(o, i)) || c;
        j = i + op.length;
        toks.push({ kind: "op", text: op });
      } else {
        toks.push({ kind: "punct", text: c });
      }
      i = j;
    }
    return toks;
  }

  const escape = (s) => s.replace(/[#$%&~_^\\{}]/g, (c) => "\\" + (c === "\\" ? "backslash " : c));

  // The nonterminal a variable name refers to, as the runtime decides it.
  function baseNt(x, nts) {
    const s = x.replace(/'+$/, "");
    if (nts.has(s)) return s;
    const u = s.indexOf("_");
    if (u > 0 && nts.has(s.slice(0, u))) return s.slice(0, u);
    const m = s.match(/^(.*?)([0-9]+)$/);
    if (m && m[1] && nts.has(m[1])) return m[1];
    return null;
  }

  const italic = (s) => (s.length === 1 ? s : `\\mathit{${escape(s)}}`);

  function variable(x, nts) {
    const primes = (x.match(/'+$/) || [""])[0];
    const s = x.slice(0, x.length - primes.length);
    const base = baseNt(x, nts);
    let tex;
    if (s === base) tex = italic(base);
    else if (s.startsWith(base + "_")) tex = `${italic(base)}_{${italic(s.slice(base.length + 1))}}`;
    else tex = `${italic(base)}_{${s.slice(base.length)}}`;
    return tex + primes;
  }

  const META_WORDS = {
    in: "\\in", dom: "\\mathrm{dom}\\,", not: "\\neg", and: "\\;\\text{and}\\;",
    where: "\\;\\text{where}\\;", arb: "\\mathrm{arb}",
  };

  // Keywords like if/then/while sit between subterms, so they get the
  // spacing of a relation; values like skip and true are plain atoms.
  const keyword = (x) => `\\mathrel{\\mathbf{${escape(x)}}}`;
  const atom = (x) => `\\mathbf{${escape(x)}}`;

  // ctx: {nts, literals, keywords, opNts: Set; ops: [string]; meta, grammar: bool}
  function word(t, ctx) {
    const x = t.text;
    const kw = ctx.keywords || new Set();
    const opNts = ctx.opNts || new Set();
    if (ctx.grammar) {
      if (opNts.has(x)) return `\\mathbin{${italic(x)}}`;
      if (ctx.nts.has(x)) return italic(x);
      if (["int", "sym", "name", "store"].includes(x)) return `\\mathsf{${x}}`;
      return kw.has(x) ? keyword(x) : atom(x);
    }
    const base = baseNt(x, ctx.nts);
    if (base) return opNts.has(base) ? `\\mathbin{${variable(x, ctx.nts)}}` : variable(x, ctx.nts);
    if (ctx.meta && META_WORDS[x]) return META_WORDS[x];
    if (kw.has(x)) return keyword(x);
    if (ctx.literals.has(x) || x === "true" || x === "false") return atom(x);
    return italic(x);
  }

  function op(t, ctx) {
    if (t.text === "||") return ctx.meta ? "\\lor" : "\\parallel";
    if (t.text === "!" && ctx.grammar) return "{!}";
    if (t.text === "|" && ctx.grammar) return "\\;\\vert\\;";
    return TEX[t.text] || `\\mathbin{\\texttt{${escape(t.text)}}}`;
  }

  // Render tokens[i..] until a closing bracket, splitting on top-level commas.
  function group(toks, i, ctx, close) {
    const parts = [[]];
    while (i < toks.length) {
      const t = toks[i];
      if (t.kind === "punct" && t.text === close) return [parts.map((p) => p.join(" ")), i + 1];
      if (t.kind === "punct" && t.text === ",") { parts.push([]); i++; continue; }
      const [tex, j] = one(toks, i, ctx);
      parts[parts.length - 1].push(tex);
      i = j;
    }
    return [parts.map((p) => p.join(" ")), i];
  }

  function one(toks, i, ctx) {
    const t = toks[i];
    switch (t.kind) {
      case "int": return [t.text, i + 1];
      case "id": return [word(t, ctx), i + 1];
      case "op": return [op(t, ctx), i + 1];
    }
    if (t.text === "(") {
      const [parts, j] = group(toks, i + 1, ctx, ")");
      return [parts.length > 1 ? `\\langle ${parts.join(",\\, ")} \\rangle` : `(${parts[0]})`, j];
    }
    if (t.text === "{") {
      const [parts, j] = group(toks, i + 1, ctx, "}");
      return [`\\{${parts.join(",\\, ")}\\}`, j];
    }
    if (t.text === "[") {
      if (toks[i + 1] && toks[i + 1].text === "]") return ["[\\,]", i + 2];
      const [parts, j] = group(toks, i + 1, ctx, "]");
      return [`[${parts.join(", ")}]`, j];
    }
    return [escape(t.text), i + 1];
  }

  function tex(src, ctx) {
    const toks = tokenize(src, ctx.ops || []);
    const out = [];
    let i = 0;
    while (i < toks.length) {
      const [s, j] = one(toks, i, ctx);
      out.push(s);
      i = j;
    }
    return out.join(" ");
  }

  // Premises containing a transition go above the bar; side conditions go
  // to the right of the conclusion, as in Sewell's notes.
  function rule(r, info) {
    const obj = { ...info, meta: false };
    const meta = { ...info, meta: true };
    const isTransition = (p) => /->/.test(p.replace(/\|->/g, ""));
    const prems = r.premises.filter((p) => p.trim());
    const above = prems.filter(isTransition).map((p) => tex(p, obj));
    const side = prems.filter((p) => !isTransition(p)).map((p) => tex(p, meta));
    const concl = tex(r.conclusion, obj);
    const name = `\\textrm{(${ruleName(r.name)})}`;
    const body = above.length ? `\\dfrac{${above.join(" \\qquad ")}}{${concl}}` : concl;
    const cond = side.length ? ` \\qquad \\text{if } ${side.join(",\\; ")}` : "";
    return `${name} \\quad ${body}${cond}`;
  }

  const ruleName = (n) =>
    n.replace(/[^A-Za-z0-9]+/g, (ops) => `}{${tex(ops, { nts: new Set(), literals: new Set(), ops: [ops] })}}\\textrm{`);

  function grammar(rows, info) {
    const ctx = { ...info, grammar: true };
    const lines = rows.filter((r) => r.trim()).map((row) => {
      const m = row.match(/^\s*([A-Za-z_][A-Za-z0-9_']*)\s*::=(.*)$/);
      if (!m) return `& & ${tex(row, ctx)}`;
      return `${italic(m[1])} & ::= & ${tex(m[2], ctx)}`;
    });
    return `\\begin{array}{rcl} ${lines.join(" \\\\ ")} \\end{array}`;
  }

  function katex(texSrc, el) {
    if (!window.katex) { el.textContent = texSrc; return; }
    window.katex.render(texSrc, el, { throwOnError: false, displayMode: true });
  }

  return { tex, rule, grammar, katex };
})();
