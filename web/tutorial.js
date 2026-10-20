"use strict";

// A short guided tour: edit a rule of the calculator, then watch the edited
// rule fire on the run page. Each step points at an element; a step with
// `done` moves on by itself once the reader has done what it asks.

const Tutorial = (() => {
  const card = (key) => document.querySelector(`#cards [data-card="${key}"]`);
  const subRule = /^rule sub .* if n\s*=\s*n2\s*-\s*n1\s*$/m;
  const finished = () => state.path.length > 1 && semplay.successors(rel, here()).length === 0;

  const steps = [
    {
      target: () => card("r:0"),
      text: "These are the rules of a calculator, written as inference rules: premises above the line, the conclusion below.",
    },
    {
      target: () => card("r:1"),
      text: "Click the rule sub to edit it.",
      done: () => card("r:1")?.classList.contains("editing"),
    },
    {
      target: () => card("r:1"),
      text: "Make subtraction backwards: change its premise to n = n2 - n1.",
      done: () => subRule.test(Editor.text()),
    },
    {
      target: () => $("page-run"),
      text: "Now open the run page.",
      done: () => page === "run",
    },
    {
      target: () => $("program"),
      text: "This is the program being run. Edit it and the run below follows.",
    },
    {
      target: () => $("focus"),
      text: "In the middle is the current state; below it, one card per rule that applies. Click a card to take that step.",
      done: () => state.path.length > 1,
    },
    {
      target: () => $("focus"),
      text: "Keep stepping until no rule applies. 10 − 4 comes out as −6: your edited rule at work.",
      done: finished,
    },
    {
      target: () => $("example"),
      text: "That's it. Try the other examples here, or change any rule on the language page.",
    },
  ];

  let at = -1, box = null, timer = null, shown = null;

  function el(tag, cls, ...kids) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    e.append(...kids);
    return e;
  }

  function place() {
    const t = steps[at].target();
    if (shown !== t) {
      shown?.classList.remove("tour-target");
      t?.classList.add("tour-target");
      t?.scrollIntoView({ block: "nearest", behavior: "smooth" });
      shown = t;
    }
    if (!t) return;
    const r = t.getBoundingClientRect();
    const w = box.offsetWidth, h = box.offsetHeight;
    const below = r.bottom + 10 + h < window.innerHeight || r.top < h + 10;
    box.style.top = `${window.scrollY + (below ? r.bottom + 10 : r.top - h - 10)}px`;
    box.style.left = `${window.scrollX + Math.max(16, Math.min(r.left, window.innerWidth - w - 16))}px`;
  }

  function show(i) {
    at = i;
    if (at >= steps.length) return stop();
    const s = steps[at];
    const last = at === steps.length - 1;
    const next = el("button", null, last ? "done" : "next");
    next.onclick = () => show(at + 1);
    const skip = el("button", "small", "skip");
    skip.onclick = stop;
    box.replaceChildren(
      el("div", "tour-count", `${at + 1} / ${steps.length}`),
      el("p", null, s.text),
      el("div", "tour-buttons", ...(last ? [] : [skip]), next));
    place();
  }

  function tick() {
    const s = steps[at];
    if (s.done && s.done()) return show(at + 1);
    place();
  }

  function start() {
    stop();
    const calc = EXAMPLES.findIndex((e) => e.name === "calculator");
    $("example").value = calc;
    showPage("language");
    showExample(calc);
    box = el("div", "tour");
    // clicks in the tour shouldn't close the rule being edited
    box.onmousedown = (e) => e.stopPropagation();
    document.body.append(box);
    show(0);
    timer = setInterval(tick, 250);
  }

  function stop() {
    clearInterval(timer);
    shown?.classList.remove("tour-target");
    box?.remove();
    box = shown = null;
  }

  return { start, stop };
})();
