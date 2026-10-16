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
  const errors = circ.check(text).diagnostics.filter((d) => d.severity === "error").length;
  const r = errors ? null : circ.load(text);
  if (!r || !r.ok) {
    banner.className = "status error-banner";
    banner.replaceChildren(
      `The language has ${errors || 1} error${errors > 1 ? "s" : ""}. `,
      link("Fix them", () => showPage("language")));
    $("start").disabled = true;
    $("session").hidden = true;
    loadedText = null;
    return false;
  }
  loadedText = text;
  Typeset.setLanguage(circ.check(text));
  program.refresh();
  const n = circ.rules().length;
  banner.className = "status";
  banner.textContent = `${n} rule${n === 1 ? "" : "s"}`;
  $("start").disabled = false;
  $("session").hidden = true;
  return true;
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
  if (p === "run" && ensureLoaded() && $("session").hidden && program.text.trim()) start();
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
  const r = circ.check(src);
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

function start() {
  if (!ensureLoaded()) return;
  const r = circ.config(program.text.trim());
  const status = $("program-status");
  if (!r.ok) {
    status.className = "status error";
    status.textContent = `col ${r.col}: ${r.message}`;
    return;
  }
  status.textContent = "";
  say("");
  state.path = [{ id: r.id, rule: null }];
  state.cursor = 0;
  $("session").hidden = false;
  render();
}

const here = () => state.path[state.cursor].id;

function take(succ) {
  say("");
  state.path = state.path.slice(0, state.cursor + 1);
  state.path.push({ id: succ.target, rule: succ.rule });
  state.cursor++;
  render();
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
// Configurations are shown typeset, the same way the program editor draws them.
function term(s) {
  const t = el("span", "term");
  t.innerHTML = Typeset.html(s);
  return t;
}
const rule = (s) => el("span", "rule", s);

function render() {
  const id = here();
  const succs = circ.successors(id);

  $("current").innerHTML = Typeset.html(circ.show(id));
  $("step-count").textContent = state.cursor === 0 ? "start" : `step ${state.cursor}`;

  const next = $("next");
  next.replaceChildren();
  if (succs.length === 0) next.append(li("empty", text("no rule applies")));
  for (const s of succs) {
    const item = li(null, rule(s.rule), term(circ.show(s.target)));
    item.title = s.bindings.map(([x, v]) => `${x} = ${v}`).join("\n");
    item.onclick = () => take(s);
    next.append(item);
  }

  const trace = $("trace");
  trace.replaceChildren();
  state.path.forEach((p, i) => {
    const siblings = i > 0 ? circ.successors(state.path[i - 1].id) : [];
    const label = el("span", null, rule(p.rule || "start"));
    if (siblings.length > 1) {
      const k = siblings.findIndex((s) => s.target === p.id && s.rule === p.rule) + 1;
      label.append(el("span", "branch", `${k} of ${siblings.length}`));
    }
    const item = li(i === state.cursor ? "here" : null,
      el("span", "n", String(i)), label, term(circ.show(p.id)));
    item.onclick = () => { state.cursor = i; render(); };
    trace.append(item);
  });
  trace.querySelector(".here")?.scrollIntoView({ block: "nearest" });

  $("reset").disabled = state.cursor === 0;
  say(succs.length === 0 ? "finished: no rule applies"
    : succs.length > 1 ? `${succs.length} successors: choose one` : "");
}

// --- examples ---------------------------------------------------------

function showExample(i) {
  const ex = EXAMPLES[i];
  if (mode === "text") setMode("rules");
  Editor.load(ex.language);
  program.text = ex.program;
  loadedText = null;
  $("session").hidden = true;
  if (page === "run") showPage("run");
}

// The program editor checks against the loaded language as you type.
const program = ProgramEditor({
  parent: $("program"),
  onRun: () => start(),
  check(text) {
    if (!loadedText) return [];
    const idx = Editor.byteToIndex(text);
    const ds = circ.checkProgram(text).map((d) => {
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
  $("start").onclick = start;
  $("reset").onclick = reset;
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
