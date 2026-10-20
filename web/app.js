"use strict";

const $ = (id) => document.getElementById(id);

const state = {
  path: [],      // [{id, rule}] — rule is how we got here (null at the start)
  cursor: 0,     // index into path of the configuration being shown
};

// --- language ---------------------------------------------------------

let mode = "rules";   // or "text"

const languageText = () => (mode === "rules" ? Editor.text() : $("source").value);

// The run page always uses the latest language without errors: it is
// loaded when the run page is shown, if it has changed.
let loadedText = null;

function ensureLoaded() {
  const text = languageText();
  if (text === loadedText) return true;
  const banner = $("lang-banner");
  const errors = semplay.check(text).diagnostics.filter((d) => d.severity === "error").length;
  const r = errors ? null : semplay.load(text);
  if (!r || !r.ok) {
    banner.className = "status error-banner";
    banner.replaceChildren(
      `The language has ${errors || 1} error${errors > 1 ? "s" : ""}. `,
      link("Fix them", () => showPage("language")));
    $("session").hidden = true;
    loadedText = null;
    return false;
  }
  loadedText = text;
  Typeset.setLanguage(semplay.check(text));
  program.refresh();
  chooseRelations();
  const n = semplay.rules().length;
  banner.className = "status";
  banner.textContent = `${n} rule${n === 1 ? "" : "s"}`;
  $("session").hidden = true;
  $("query").hidden = true;
  return true;
}

// --- relations ----------------------------------------------------------

// The relation being run, by index into semplay.relations(). Relations that
// can be stepped (one input, an output of the same sort) get the stepper;
// any other relation is queried for all its solutions.
let rels = [];
let rel = 0;
const relation = () => rels[rel];

function chooseRelations() {
  const before = rels[rel] && rels[rel].notation;
  rels = semplay.relations();
  const keep = rels.findIndex((r) => r.notation === before);
  const stepping = rels.findIndex((r) => r.steps);
  rel = keep >= 0 ? keep : stepping >= 0 ? stepping : 0;
  const select = $("relation");
  select.replaceChildren(...rels.map((r, i) => new Option(r.form, i)));
  select.value = rel;
  select.closest(".relation-pick").hidden = rels.length < 2;
}

function link(label, fn) {
  const a = document.createElement("a");
  a.href = "#";
  a.textContent = label;
  a.onclick = (e) => { e.preventDefault(); fn(); };
  return a;
}

let page = "language";

function showPage(p) {
  page = p;
  $("language").hidden = p !== "language";
  $("machine").hidden = p !== "run";
  $("page-language").classList.toggle("on", p === "language");
  $("page-run").classList.toggle("on", p === "run");
  if (p === "run" && ensureLoaded() && $("session").hidden && $("query").hidden && program.text.trim()) start();
}

function showProblems({ errors, warnings }) {
  const p = $("problems");
  const parts = [];
  if (errors) parts.push(`${errors} error${errors === 1 ? "" : "s"}`);
  if (warnings) parts.push(`${warnings} warning${warnings === 1 ? "" : "s"}`);
  p.textContent = parts.length ? parts.join(", ") : "no problems";
  p.className = "status" + (errors ? " has-errors" : "");
}

Editor.onChange = () => {
  showProblems(Editor.summary());
  $("load-status").textContent = "";
};

// The text view is checked too, with problems listed by line.
let textTimer = null;
function checkText() {
  const src = $("source").value;
  const r = semplay.check(src);
  const idx = Editor.byteToIndex(src);
  const list = $("text-problems");
  list.replaceChildren();
  for (const d of r.diagnostics) {
    const at = idx(d.start);
    const before = src.slice(0, at);
    const line = before.split("\n").length, col = at - before.lastIndexOf("\n");
    const item = document.createElement("li");
    item.className = d.severity;
    item.textContent = `${line}:${col}  ${d.message}`;
    item.onclick = () => { $("source").focus(); $("source").setSelectionRange(at, Math.max(idx(d.stop), at)); };
    list.append(item);
  }
  showProblems({
    errors: r.diagnostics.filter((d) => d.severity === "error").length,
    warnings: r.diagnostics.filter((d) => d.severity === "warning").length,
  });
}

function setMode(m) {
  if (m === mode) return;
  if (m === "text") {
    $("source").value = Editor.text();
    checkText();
  } else {
    const r = Editor.load($("source").value);
    if (!r.ok) {
      $("load-status").className = "status error";
      $("load-status").textContent = `fix line ${r.line}, col ${r.col} first: ${r.message}`;
      return;
    }
  }
  mode = m;
  $("cards").hidden = m !== "rules";
  $("text-view").hidden = m !== "text";
  $("tab-rules").classList.toggle("on", m === "rules");
  $("tab-text").classList.toggle("on", m === "text");
}

// --- program ----------------------------------------------------------

// Starts a run of the program as written. While the program doesn't parse,
// the last run stays on screen, faded.
function start() {
  if (!ensureLoaded()) return;
  const text = program.text.trim();
  const status = $("program-status");
  if (!text) {
    status.textContent = "";
    $("session").hidden = true;
    $("query").hidden = true;
    return;
  }
  const r = semplay.config(text);
  $("session").classList.toggle("stale", !r.ok);
  $("query").classList.toggle("stale", !r.ok);
  if (!r.ok) {
    status.className = "status error";
    status.textContent = `col ${r.col}: ${r.message}`;
    return;
  }
  status.textContent = "";
  if (!relation()) return;
  if (relation().steps) {
    say("");
    state.path = [{ id: r.id }];
    state.cursor = 0;
    $("query").hidden = true;
    $("session").hidden = false;
    render();
  } else {
    $("session").hidden = true;
    $("query").hidden = false;
    solve(r.id);
  }
}

const here = () => state.path[state.cursor].id;

function take(succ, of) {
  say("");
  state.path = state.path.slice(0, state.cursor + 1);
  state.path.push({ id: succ.target, step: succ, of });
  state.cursor++;
  render(true);
  $("focus").focus({ preventScroll: true });   // keep the keyboard shortcuts working
}

function back() {
  if (state.cursor > 0) { state.cursor--; render(); }
}

function say(msg) { $("machine-status").textContent = msg; }

function reset() {
  state.cursor = 0;
  render();
}

// --- rendering --------------------------------------------------------

function el(tag, cls, ...kids) {
  const e = document.createElement(tag);
  if (cls) e.className = cls;
  e.append(...kids);
  return e;
}
const li = (cls, ...kids) => el("li", cls, ...kids);
const text = (s) => document.createTextNode(s);
const rule = (s) => el("span", "rule", s);

// LaTeX from the runtime, typeset. \htmlClass marks the part a step
// changed (see semplay.latexDiff).
function tex(src, cls = "term", display = false) {
  const t = el("span", cls);
  if (window.katex)
    katex.render(src, t, {
      throwOnError: false, displayMode: display, strict: false,
      trust: (c) => c.command === "\\htmlClass",
    });
  else t.textContent = src;
  return t;
}

// How a step is labelled: its rule, and any labels (a CCS action, say).
function stepLabel(name, labels) {
  const l = el("span", "step-label", rule(name || "start"));
  if (labels && labels.length) l.append(" ", tex(labels.join(",\\, "), "label"));
  return l;
}

// A derivation as a tree: each node is a judgment and the rule proving it.
// Deep trees open collapsed below [open] levels.
function derivation(d, open = 3) {
  const node = li("deriv-node");
  const head = el("div", "deriv-head", rule(d.rule), tex(d.judgment));
  node.append(head);
  if (d.premises.length) {
    const kids = el("ul", "deriv");
    for (const p of d.premises) kids.append(derivation(p, open - 1));
    kids.hidden = open <= 0;
    node.classList.add("has-kids");
    node.classList.toggle("closed", kids.hidden);
    head.onclick = (e) => {
      e.stopPropagation();
      kids.hidden = !kids.hidden;
      node.classList.toggle("closed", kids.hidden);
    };
    node.append(kids);
  }
  return el("ul", "deriv root", node);
}

// The stepper: where you are in the middle, the step that got you there
// above it, and the possible next steps fanned out below as cards. Click a
// card and it moves to the middle; click the step above to go back.
function render(arriving) {
  const box = $("focus");
  box.replaceChildren();
  const here = state.path[state.cursor];
  const prev = state.cursor > 0 ? state.path[state.cursor - 1] : null;
  const succs = semplay.successors(rel, here.id);

  // breadcrumbs: every step so far; click one to go back to it
  const crumbs = el("div", "crumbs");
  state.path.forEach((p, i) => {
    if (i > state.cursor) return;
    if (i > 0) crumbs.append(el("span", "sep", "→"));
    const b = el("button", null, i === 0 ? "start" : p.step.rule);
    b.disabled = i === state.cursor;
    b.onclick = () => { state.cursor = i; render(); };
    crumbs.append(b);
  });
  box.append(crumbs);

  if (prev) {
    const before = el("div", "prev", tex(semplay.latexDiff(prev.id, here.id, "redex")));
    before.title = "go back to this configuration";
    before.onclick = back;
    const arrow = el("div", "via", el("span", "arrow", "⟶"), stepLabel(here.step.rule, here.step.labels));
    if (here.of > 1) arrow.append(el("span", "branch", `1 of ${here.of}`));
    const why = whyButton(here.step.why);
    arrow.append(why.button);
    box.append(before, el("div", "stem"), arrow, why.tree, el("div", "stem"));
  }

  const centre = el("div", "centre" + (arriving ? " arriving" : ""),
    tex(prev ? semplay.latexDiff(here.id, prev.id, "changed") : semplay.latex(here.id), "term", true));
  box.append(centre);
  if (arriving) requestAnimationFrame(() => requestAnimationFrame(() => centre.classList.remove("arriving")));

  if (succs.length === 0) {
    box.append(el("div", "end", "finished: no rule applies"));
  } else {
    box.append(el("div", "stem"));
    const fan = el("div", "fan" + (succs.length > 1 ? " many" : ""));
    succs.forEach((s, k) => {
      const why = whyButton(s.why);
      const top = el("div", "top", el("span", "key", succs.length > 1 ? String(k + 1) : "⏎"),
        stepLabel(s.rule, s.labels), why.button);
      const card = el("div", "card-step", top, tex(semplay.latexDiff(s.target, here.id, "changed")), why.tree);
      card.tabIndex = 0;
      card.title = "take this step";
      card.onclick = () => take(s, succs.length);
      card.onkeydown = (e) => { if (e.key === "Enter" && e.target === card) take(s, succs.length); };
      fan.append(card);
    });
    box.append(fan);
    // the bar joining the cards runs between the first and last card's centres
    requestAnimationFrame(() => {
      const cards = fan.querySelectorAll(".card-step");
      if (cards.length < 2) return;
      const fb = fan.getBoundingClientRect(), a = cards[0].getBoundingClientRect(), z = cards[cards.length - 1].getBoundingClientRect();
      if (Math.abs(a.top - z.top) > 4) { fan.classList.remove("many"); return; }
      fan.style.setProperty("--l", (a.left + a.width / 2 - fb.left) + "px");
      fan.style.setProperty("--r", (fb.right - z.left - z.width / 2) + "px");
    });
  }

  $("step-count").textContent = state.cursor === 0 ? "start" : `step ${state.cursor}`;
  $("reset").disabled = state.cursor === 0;
  say(succs.length > 1 ? `${succs.length} possible steps: choose one` : "");
}

// A "why" toggle and the derivation it shows.
function whyButton(d) {
  const button = el("button", "small why", "why");
  button.title = "show the derivation of this step";
  const tree = el("div", "why-tree", derivation(d, 2));
  tree.hidden = true;
  tree.onclick = (e) => e.stopPropagation();
  button.onclick = (e) => {
    e.stopPropagation();
    tree.hidden = !tree.hidden;
    button.textContent = tree.hidden ? "why" : "hide";
  };
  return { button, tree };
}

// A relation that can't be stepped is solved: every solution, with its
// derivation.
function solve(id) {
  const r = semplay.solve(rel, id);
  const n = r.solutions.length;
  $("query-meta").textContent =
    (n === 0 ? "none" : `${n}`) + (r.outOfFuel ? ", search ran out of fuel" : "");
  const list = $("solutions");
  list.replaceChildren();
  if (n === 0)
    list.append(li("empty", text(r.outOfFuel
      ? "no derivation found before the search ran out of fuel"
      : "no derivation: nothing can be concluded about this input")));
  for (const s of r.solutions) {
    list.append(li("solution", el("div", "outputs", tex(s.outputs.join(",\; "), "term", true)),
      el("div", "proof", el("h3", null, "Derivation"), derivation(s.proof, 3))));
  }
}

// --- examples ---------------------------------------------------------

function showExample(i) {
  const ex = EXAMPLES[i];
  if (mode === "text") setMode("rules");
  Editor.load(ex.language);
  loadedText = null;
  program.text = ex.program;
  $("session").hidden = true;
  if (page === "run") showPage("run");
}

// The program editor checks against the loaded language as you type.
const program = ProgramEditor({
  parent: $("program"),
  // the run always follows the program
  onChange: () => { if (page === "run") start(); },
  check(text) {
    if (!loadedText || !text.trim()) return [];
    const idx = Editor.byteToIndex(text);
    const ds = semplay.checkProgram(text).map((d) => {
      const from = Math.min(idx(d.start), text.length);
      return { from, to: Math.max(Math.min(idx(d.stop), text.length), from), severity: d.severity, message: d.message };
    });
    $("program-status").textContent = ds.length ? ds[0].message : "";
    $("program-status").className = "status" + (ds.some((d) => d.severity === "error") ? " has-errors" : "");
    return ds;
  },
});

function init() {
  const select = $("example");
  EXAMPLES.forEach((ex, i) => select.append(new Option(ex.name, i)));
  select.onchange = () => showExample(select.value);

  $("page-language").onclick = () => showPage("language");
  $("page-run").onclick = () => showPage("run");
  $("reset").onclick = reset;
  $("focus").addEventListener("keydown", (e) => {
    if (page !== "run" || $("session").hidden) return;
    const succs = semplay.successors(rel, state.path[state.cursor].id);
    if (e.key === "Enter" && e.target === $("focus") && succs.length === 1) { e.preventDefault(); take(succs[0], 1); }
    else if (/^[1-9]$/.test(e.key) && succs[Number(e.key) - 1]) { e.preventDefault(); take(succs[Number(e.key) - 1], succs.length); }
    else if (e.key === "Backspace" && e.target === $("focus")) { e.preventDefault(); back(); }
  });
  $("relation").onchange = () => {
    rel = Number($("relation").value);
    $("session").hidden = true;
    $("query").hidden = true;
    start();
  };
  $("source").onkeydown = (e) => {
    if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); showPage("run"); }
  };
  $("source").oninput = () => { clearTimeout(textTimer); textTimer = setTimeout(checkText, 200); };
  $("tab-rules").onclick = () => setMode("rules");
  $("tab-text").onclick = () => setMode("text");
  document.addEventListener("keydown", (e) => {
    if (e.key === "Enter" && (e.metaKey || e.ctrlKey) && page === "language") { e.preventDefault(); showPage("run"); }
  });

  select.value = 1;
  showExample(1);
}

init();
