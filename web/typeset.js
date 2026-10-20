"use strict";

// Typeset object-language text as it is typed: ASCII stays the source of
// truth, but it is shown with textbook glyphs (⟨⟩ for configurations, ↦, ≥,
// ∥ …), bold keywords and math-italic names. What counts as a keyword or an
// operator comes from the loaded grammar.
//
// Used two ways: as CodeMirror decorations in the program editor, and as
// static HTML for configurations on the run page.

const Typeset = (() => {
  const OP_CHARS = "+-*/<>=!:;|&^~.?@#$%\\";
  const OWN_OPS = ["|->", "->*", "::=", "->", "<=", ">=", "<>", "!=", "&&", "||"];
  const GLYPH = {
    "|->": "↦", "->": "→", ">=": "≥", "<=": "≤", "<>": "≠", "!=": "≠",
    "||": "∥", "&&": "∧", "*": "×", "-": "−",
  };
  // LaTeX commands that can be typed for the source they stand for
  const COMMANDS = {
    langle: "(", rangle: ")", mapsto: "|->", to: "->", rightarrow: "->", geq: ">=", ge: ">=",
    leq: "<=", le: "<=", neq: "!=", ne: "!=", parallel: "||", land: "&&", wedge: "&&", times: "*",
  };

  // info: {keywords: Set, atoms: Set, ops: [string]} from the checker.
  let info = { keywords: new Set(), atoms: new Set(), ops: [] };

  function setLanguage(checked) {
    const words = checked.literals.filter((l) => /^[A-Za-z_]/.test(l));
    info = {
      keywords: new Set(checked.keywords),
      atoms: new Set(words.filter((w) => !checked.keywords.includes(w)).concat(["true", "false"])),
      ops: checked.literals.filter((l) => !/^[A-Za-z_]/.test(l)),
    };
  }

  // Tokens with positions. Parentheses that enclose a top-level comma are
  // configurations/tuples and are drawn as angle brackets.
  function tokens(text) {
    const known = [...new Set([...OWN_OPS, ...info.ops])].sort((a, b) => b.length - a.length);
    const out = [];
    const parens = [];
    let i = 0;
    while (i < text.length) {
      const c = text[i];
      if (/\s/.test(c)) { i++; continue; }
      let j = i + 1, kind;
      if (/[0-9]/.test(c)) {
        while (j < text.length && /[0-9]/.test(text[j])) j++;
        kind = "num";
      } else if (/[A-Za-z_]/.test(c)) {
        while (j < text.length && /[A-Za-z0-9_']/.test(text[j])) j++;
        const w = text.slice(i, j);
        kind = info.keywords.has(w) ? "kw" : info.atoms.has(w) ? "atom" : "id";
      } else if (OP_CHARS.includes(c)) {
        const op = known.find((o) => text.startsWith(o, i)) || c;
        j = i + op.length;
        kind = "op";
      } else {
        kind = "punct";
      }
      const tok = { from: i, to: j, kind, text: text.slice(i, j) };
      if (tok.text === "(") parens.push({ tok, comma: false });
      else if (tok.text === "," && parens.length) parens[parens.length - 1].comma = true;
      else if (tok.text === ")" && parens.length) {
        const open = parens.pop();
        if (open.comma) { open.tok.glyph = "⟨"; tok.glyph = "⟩"; }
      }
      if (kind === "op" && GLYPH[tok.text]) tok.glyph = GLYPH[tok.text];
      out.push(tok);
      i = j;
    }
    return out;
  }

  // --- static HTML --------------------------------------------------------

  const esc = (s) => s.replace(/[&<>]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;" })[c]);

  function html(text) {
    let out = "", at = 0;
    for (const t of tokens(text)) {
      out += esc(text.slice(at, t.from));
      const shown = esc(t.glyph || t.text);
      out += `<span class="ts-${t.kind}">${shown}</span>`;
      at = t.to;
    }
    return out + esc(text.slice(at));
  }

  // --- CodeMirror ---------------------------------------------------------

  function codemirror() {
    const { ViewPlugin, Decoration, WidgetType, EditorView } = CM.view;

    class Glyph extends WidgetType {
      constructor(text, kind) { super(); this.text = text; this.kind = kind; }
      eq(o) { return o.text === this.text && o.kind === this.kind; }
      toDOM() {
        const s = document.createElement("span");
        s.className = `ts-${this.kind} ts-glyph`;
        s.textContent = this.text;
        return s;
      }
      ignoreEvent() { return false; }
    }

    const build = (view) => {
      const marks = [], glyphs = [];
      const text = view.state.doc.toString();
      for (const t of tokens(text)) {
        if (t.glyph) glyphs.push(Decoration.replace({ widget: new Glyph(t.glyph, t.kind) }).range(t.from, t.to));
        else marks.push(Decoration.mark({ class: `ts-${t.kind}` }).range(t.from, t.to));
      }
      return { all: Decoration.set([...marks, ...glyphs], true), glyphs: Decoration.set(glyphs, true) };
    };

    const plugin = ViewPlugin.fromClass(
      class {
        constructor(view) { ({ all: this.decorations, glyphs: this.glyphs } = build(view)); }
        update(u) { ({ all: this.decorations, glyphs: this.glyphs } = build(u.view)); }
      },
      {
        decorations: (v) => v.decorations,
        // the cursor steps over a glyph as one character
        provide: (p) => EditorView.atomicRanges.of((view) => view.plugin(p)?.glyphs || Decoration.none),
      });

    // A LaTeX command (\langle, \mapsto …) becomes its source once it is
    // ended by any non-letter; an ending space is swallowed, as in LaTeX.
    const commands = EditorView.inputHandler.of((view, from, to, text) => {
      if (/^[A-Za-z]/.test(text)) return false;
      const before = view.state.sliceDoc(Math.max(0, from - 20), from).match(/\\([A-Za-z]+)$/);
      const src = before && COMMANDS[before[1]];
      if (!src) return false;
      const start = from - before[0].length;
      const insert = src + (text === " " ? "" : text);
      view.dispatch({ changes: { from: start, to, insert }, selection: { anchor: start + insert.length } });
      return true;
    });
    return [plugin, commands];
  }

  // completions for a \command being typed
  function commandCompletions(ctx) {
    const w = ctx.matchBefore(/\\[A-Za-z]*/);
    if (!w) return null;
    return {
      from: w.from,
      options: Object.entries(COMMANDS).map(([name, src]) => ({
        label: "\\" + name, detail: GLYPH[src] || (src === "(" ? "⟨" : src === ")" ? "⟩" : src), apply: src,
      })),
    };
  }

  return { setLanguage, tokens, html, codemirror, commandCompletions, get info() { return info; } };
})();

// The program editor: CodeMirror with the typesetting above, the runtime's
// checker for squiggles, and the grammar's words for autocomplete.
// opts: {parent, doc, onChange(), check(text) -> [{from, to, severity, message}]}
const ProgramEditor = (opts) => {
  const { EditorState } = CM.state;
  const { EditorView, keymap, placeholder } = CM.view;
  const { defaultKeymap, history, historyKeymap } = CM.commands;
  const { autocompletion, completionKeymap } = CM.autocomplete;
  const { linter, forceLinting } = CM.lint;

  const complete = (ctx) => {
    const cmd = Typeset.commandCompletions(ctx);
    if (cmd) return cmd;
    const w = ctx.matchBefore(/[A-Za-z_][A-Za-z0-9_']*/);
    if (!w || (w.from === w.to && !ctx.explicit)) return null;
    const info = Typeset.info;
    const names = new Set(
      Typeset.tokens(ctx.state.doc.toString()).filter((t) => t.kind === "id" && t.from !== w.from).map((t) => t.text));
    return {
      from: w.from,
      options: [
        ...[...info.keywords].map((k) => ({ label: k, type: "keyword" })),
        ...[...info.atoms].map((k) => ({ label: k, type: "constant" })),
        ...[...names].map((n) => ({ label: n, type: "variable" })),
      ],
    };
  };

  const view = new EditorView({
    parent: opts.parent,
    state: EditorState.create({
      doc: opts.doc || "",
      extensions: [
        history(),
        Typeset.codemirror(),
        linter((v) => opts.check(v.state.doc.toString()), { delay: 200 }),
        autocompletion({ override: [complete] }),
        keymap.of([
          ...completionKeymap, ...defaultKeymap, ...historyKeymap,
        ]),
        EditorView.updateListener.of((u) => { if (u.docChanged) opts.onChange(); }),
        EditorView.lineWrapping,
        placeholder("(e, s)"),
      ],
    }),
  });

  return {
    get text() { return view.state.doc.toString(); },
    set text(s) { view.dispatch({ changes: { from: 0, to: view.state.doc.length, insert: s } }); },
    // the language changed: redraw and recheck
    refresh() { view.dispatch({}); forceLinting(view); },
  };
};
