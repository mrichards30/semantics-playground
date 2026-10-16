"use strict";

const $ = (id) => document.getElementById(id);

const state = {
  path: [],      // [{id, rule}] — rule is how we got here (null at the start)
  cursor: 0,     // index into path of the configuration being shown
  timer: null,
};

// --- language ---------------------------------------------------------

function loadLanguage() {
  stop();
  const r = circ.load($("source").value);
  const status = $("load-status");
  if (!r.ok) {
    status.className = "status error";
    status.textContent = `line ${r.line}, col ${r.col}: ${r.message}`;
    $("start").disabled = true;
    return false;
  }
  const n = circ.rules().length;
  status.className = "status";
  status.textContent = `${n} rule${n === 1 ? "" : "s"}`;
  $("start").disabled = false;
  $("session").hidden = true;
  return true;
}

// --- program ----------------------------------------------------------

function start() {
  stop();
  const r = circ.config($("program").value.trim());
  const status = $("program-status");
  if (!r.ok) {
    status.className = "status error";
    status.textContent = `col ${r.col}: ${r.message}`;
    return;
  }
  status.textContent = "";
  state.path = [{ id: r.id, rule: null }];
  state.cursor = 0;
  $("finals-box").hidden = true;
  $("session").hidden = false;
  render();
}

const here = () => state.path[state.cursor].id;

function take(succ) {
  state.path = state.path.slice(0, state.cursor + 1);
  state.path.push({ id: succ.target, rule: succ.rule });
  state.cursor++;
  render();
}

function step() {
  const succs = circ.successors(here());
  if (succs.length === 1) take(succs[0]);
}

function back() {
  if (state.cursor > 0) { state.cursor--; render(); }
}

function reset() {
  stop();
  state.cursor = 0;
  render();
}

// Follow the only successor until there is a choice or nothing left.
function play() {
  if (state.timer) return stop();
  $("play").classList.add("on");
  $("play").textContent = "pause";
  state.timer = setInterval(() => {
    const succs = circ.successors(here());
    if (succs.length !== 1) return stop();
    take(succs[0]);
  }, 350);
}

function stop() {
  if (state.timer) clearInterval(state.timer);
  state.timer = null;
  $("play").classList.remove("on");
  $("play").textContent = "play";
}

function runAll() {
  stop();
  const r = circ.run(here());
  const list = $("finals");
  list.replaceChildren();
  if (r.finals.length === 0) {
    list.append(li("empty", text("none — every path loops")));
  }
  for (const id of r.finals) list.append(li(null, term(circ.show(id))));
  $("finals-meta").textContent =
    `${r.finals.length} reachable, ${r.states} configurations explored` +
    (r.truncated ? " (stopped early)" : "");
  $("finals-box").hidden = false;
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
const term = (s) => el("span", "term", s);
const rule = (s) => el("span", "rule", s);

function render() {
  const id = here();
  const succs = circ.successors(id);

  $("current").textContent = circ.show(id);
  $("step-count").textContent = state.cursor === 0 ? "start" : `step ${state.cursor}`;

  const next = $("next");
  next.replaceChildren();
  if (succs.length === 0) next.append(li("empty", text("no rule applies")));
  for (const s of succs) {
    const item = li(null, rule(s.rule), term(circ.show(s.target)));
    item.title = s.bindings.map(([x, v]) => `${x} = ${v}`).join("\n");
    item.onclick = () => { stop(); take(s); };
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
    item.onclick = () => { stop(); state.cursor = i; render(); };
    trace.append(item);
  });
  trace.querySelector(".here")?.scrollIntoView({ block: "nearest" });

  $("back").disabled = state.cursor === 0;
  $("reset").disabled = state.cursor === 0;
  $("step").disabled = succs.length !== 1;
  $("play").disabled = succs.length === 0 && !state.timer;
}

// --- examples ---------------------------------------------------------

function showExample(i) {
  const ex = EXAMPLES[i];
  $("source").value = ex.language;
  $("program").value = ex.program;
  if (loadLanguage()) start();
}

function init() {
  const select = $("example");
  EXAMPLES.forEach((ex, i) => select.append(new Option(ex.name, i)));
  select.onchange = () => showExample(select.value);

  $("load").onclick = loadLanguage;
  $("start").onclick = start;
  $("step").onclick = () => { stop(); step(); };
  $("back").onclick = () => { stop(); back(); };
  $("reset").onclick = reset;
  $("play").onclick = play;
  $("run").onclick = runAll;
  $("program").onkeydown = (e) => {
    if (e.key === "Enter" && !e.shiftKey) { e.preventDefault(); start(); }
  };
  $("source").onkeydown = (e) => {
    if (e.key === "Enter" && (e.metaKey || e.ctrlKey)) { e.preventDefault(); loadLanguage(); }
  };

  select.value = 1;
  showExample(1);
}

init();
