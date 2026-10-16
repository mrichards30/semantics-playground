"use strict";

// The language editor: a grammar card and one card per rule. Cards show the
// rule typeset; clicking one edits its slots as plain text. Every edit is
// checked by the runtime (debounced), and problems are underlined in place.
//
// The model is serialized to the .sem text format for checking and loading,
// remembering where each slot's text landed, so diagnostics can be mapped
// back to the slot (and the character range) they are about.

const Editor = (() => {
  let model = { grammar: [], fixity: "", relations: [], rules: [] };
  // typeset forms from the last check: the grammar, each relation, each rule
  let tex = { grammar: "", relations: [], rules: new Map() };
  let diags = new Map();   // slot key -> [{from, to, severity, message}]
  let cardDiags = [];      // [{card, severity, message}] not inside any slot
  let editing = null;      // card key being edited: "grammar" or "r:<i>"
  let onChange = () => {};
  let timer = null;

  function katexInto(el, src, display) {
    if (!window.katex) { el.textContent = src; return; }
    window.katex.render(src, el, { throwOnError: false, displayMode: display });
  }

  // --- serialization ----------------------------------------------------

  function serialize() {
    let text = "";
    const slots = [];
    const cards = [];
    const put = (key, s) => { slots.push({ key, start: text.length, end: text.length + s.length }); text += s; };
    const rows = model.grammar.map((r, i) => [r, i]).filter(([r]) => r.trim());
    const gStart = text.length;
    if (rows.length) {
      text += "grammar\n";
      for (const [r, i] of rows) { text += "  "; put(`g:${i}`, r); text += "\n"; }
    }
    if (model.fixity.trim()) { text += "fixity "; put("fix", model.fixity); text += "\n"; }
    model.relations.forEach((r, i) => { if (r.trim()) { text += "relation "; put(`rel:${i}`, r); text += "\n"; } });
    cards.push({ card: "grammar", start: gStart, end: text.length });
    model.rules.forEach((r, i) => {
      const start = text.length;
      text += "rule ";
      put(`r:${i}:name`, r.name);
      text += " ";
      put(`r:${i}:c`, r.conclusion);
      const ps = r.premises.map((p, j) => [p, j]).filter(([p]) => p.trim());
      ps.forEach(([p, j], k) => { text += k === 0 ? " if " : ", "; put(`r:${i}:p:${j}`, p); });
      text += "\n";
      cards.push({ card: `r:${i}`, start, end: text.length });
    });
    return { text, slots, cards };
  }

  // A file may define a nonterminal more than once (an extension adding
  // syntax); the meaning is the union, so show it as one row.
  function mergeRows(rows) {
    const out = [];
    const at = new Map();
    for (const row of rows) {
      const m = row.match(/^\s*([A-Za-z_][A-Za-z0-9_']*)\s*::=\s*(.*)$/);
      if (m && at.has(m[1])) out[at.get(m[1])] += " | " + m[2];
      else { if (m) at.set(m[1], out.length); out.push(row); }
    }
    return out;
  }

  function fromOutline(o) {
    model = {
      grammar: mergeRows(o.grammar),
      fixity: o.fixity.join("  "),
      relations: o.relations.slice(),
      rules: o.rules.map((r) => ({ name: r.name, conclusion: r.conclusion, premises: r.premises.slice() })),
    };
  }

  // The checker reports UTF-8 byte offsets; slots are indexed by string index.
  function byteToIndex(text) {
    const map = [];
    let b = 0;
    for (let i = 0; i < text.length; i++) {
      const cp = text.codePointAt(i);
      const n = cp < 0x80 ? 1 : cp < 0x800 ? 2 : cp < 0x10000 ? 3 : 4;
      for (let k = 0; k < n; k++) map[b + k] = i;
      b += n;
      if (cp >= 0x10000) i++;
    }
    map[b] = text.length;
    return (off) => map[Math.min(off, b)];
  }

  // --- checking ---------------------------------------------------------

  function check() {
    const { text, slots, cards } = serialize();
    const r = circ.check(text);
    const idx = byteToIndex(text);
    tex = { grammar: r.grammarTex, relations: r.relationsTex, rules: new Map() };
    for (const t of r.rulesTex) {
      const at = idx(t.from);
      const card = cards.find((c) => at >= c.start && at < c.end);
      if (card) tex.rules.set(card.card, t.tex);
    }
    diags = new Map();
    cardDiags = [];
    for (const d of r.diagnostics) {
      const start = idx(d.start), stop = Math.max(idx(d.stop), start);
      const slot =
        slots.find((s) => start >= s.start && start < s.end) ||
        slots.find((s) => start === s.end && s.end > s.start);
      if (slot) {
        const list = diags.get(slot.key) || [];
        list.push({ from: start - slot.start, to: stop - slot.start, severity: d.severity, message: d.message });
        diags.set(slot.key, list);
      } else {
        const card = cards.find((c) => start >= c.start && start < c.end) || cards[cards.length - 1];
        cardDiags.push({ card: card ? card.card : "grammar", severity: d.severity, message: d.message });
      }
    }
    return r.diagnostics;
  }

  function problemsOf(card) {
    const own = [...diags.entries()]
      .filter(([k]) => k === card || k.startsWith(card + ":") || (card === "grammar" && (k.startsWith("g:") || k === "fix" || k.startsWith("rel:"))))
      .flatMap(([k, ds]) => ds.map((d) => ({ ...d, slot: k })));
    return own.concat(cardDiags.filter((d) => d.card === card));
  }

  const hasErrors = (card) => problemsOf(card).some((d) => d.severity === "error");

  function scheduleCheck() {
    clearTimeout(timer);
    timer = setTimeout(() => { check(); refresh(); onChange(); }, 200);
  }

  // --- slots ------------------------------------------------------------

  function mirrorHTML(text, ds) {
    const marks = new Array(text.length + 1).fill(null);
    for (const d of ds || []) {
      const to = Math.max(d.to, d.from + 1);
      for (let i = d.from; i < to && i <= text.length; i++)
        if (marks[i] !== "error") marks[i] = d.severity;
    }
    let html = "", cur = null;
    const esc = (c) => (c === "<" ? "&lt;" : c === ">" ? "&gt;" : c === "&" ? "&amp;" : c);
    for (let i = 0; i <= text.length; i++) {
      const m = marks[i];
      if (m !== cur) { if (cur) html += "</span>"; if (m) html += `<span class="sq-${m}">`; cur = m; }
      if (i < text.length) html += esc(text[i]);
      else if (m) html += "&nbsp;";
    }
    if (cur) html += "</span>";
    return html + "\n";
  }

  function slot(key, value, set, opts = {}) {
    const wrap = document.createElement("div");
    wrap.className = "slot" + (opts.cls ? " " + opts.cls : "");
    const mirror = document.createElement("div");
    mirror.className = "mirror";
    const ta = document.createElement("textarea");
    ta.rows = 1;
    ta.spellcheck = false;
    ta.value = value;
    ta.placeholder = opts.placeholder || "";
    ta.dataset.key = key;
    const sync = () => {
      mirror.innerHTML = mirrorHTML(ta.value, diags.get(key));
      ta.style.height = "auto";
      ta.style.height = ta.scrollHeight + "px";
    };
    ta.oninput = () => { set(ta.value); sync(); scheduleCheck(); };
    ta.onkeydown = (e) => {
      if (e.key === "Enter") { e.preventDefault(); opts.onEnter && opts.onEnter(); }
    };
    wrap.append(mirror, ta);
    wrap.sync = sync;
    requestAnimationFrame(sync);
    return wrap;
  }

  // --- cards ------------------------------------------------------------

  const root = () => document.getElementById("cards");

  function el(tag, cls, ...kids) {
    const e = document.createElement(tag);
    if (cls) e.className = cls;
    e.append(...kids);
    return e;
  }

  function button(label, title, fn) {
    const b = el("button", "small", label);
    b.title = title;
    b.type = "button";
    b.onmousedown = (e) => e.preventDefault();   // keep focus in the card
    b.onclick = (e) => { e.stopPropagation(); fn(); };
    return b;
  }

  function problemList(card) {
    const ps = problemsOf(card);
    const list = el("ul", "problems");
    for (const p of ps) {
      const item = el("li", p.severity, p.message);
      if (p.slot) item.onclick = () => focusSlot(p.slot, p.from, p.to);
      list.append(item);
    }
    return list;
  }

  function focusSlot(key, from, to) {
    const card = key.startsWith("r:") ? key.split(":").slice(0, 2).join(":") : "grammar";
    if (editing !== card) { editing = card; refresh(); }
    const ta = root().querySelector(`textarea[data-key="${key}"]`);
    if (ta) { ta.focus(); ta.setSelectionRange(from, Math.max(to, from)); }
  }

  function grammarCard() {
    const key = "grammar";
    const card = el("div", "card grammar-card");
    card.dataset.card = key;
    const errs = hasErrors(key);
    if (editing === key || errs) {
      card.classList.add("editing");
      const rows = el("div", "rows");
      model.grammar.forEach((r, i) => {
        const s = slot(`g:${i}`, r, (v) => (model.grammar[i] = v), {
          placeholder: "e ::= v | e + e",
          onEnter: () => { model.grammar.splice(i + 1, 0, ""); refresh(); focusSlot(`g:${i + 1}`, 0, 0); },
        });
        rows.append(el("div", "row", s, button("×", "remove this row", () => { model.grammar.splice(i, 1); changed(); })));
      });
      rows.append(el("div", "row add", button("+ nonterminal", "add a row", () => {
        model.grammar.push(""); refresh(); focusSlot(`g:${model.grammar.length - 1}`, 0, 0);
      })));
      const fix = slot("fix", model.fixity, (v) => (model.fixity = v), { placeholder: "infixl 4 +   prefix 5 !" });
      rows.append(el("div", "row fixity", el("span", "label", "fixity"), fix));
      model.relations.forEach((r, i) => {
        const s = slot(`rel:${i}`, r, (v) => (model.relations[i] = v), {
          placeholder: "(e, s) -> (e, s)",
          onEnter: () => { model.relations.splice(i + 1, 0, ""); refresh(); focusSlot(`rel:${i + 1}`, 0, 0); },
        });
        rows.append(el("div", "row", el("span", "label", "relation"), s,
          button("×", "remove this relation", () => { model.relations.splice(i, 1); changed(); })));
      });
      rows.append(el("div", "row add", button("+ relation", "declare a relation", () => {
        model.relations.push(""); refresh(); focusSlot(`rel:${model.relations.length - 1}`, 0, 0);
      })));
      card.append(rows);
    } else {
      const math = el("div", "math");
      katexInto(math, tex.grammar, true);
      card.append(math);
      if (model.fixity.trim()) card.append(el("div", "fixity-view", "fixity  " + model.fixity));
      if (tex.relations.length) {
        const rels = el("div", "relations-view", el("span", "label", "relations"));
        for (const t of tex.relations) { const m = el("span", "rel"); katexInto(m, t, false); rels.append(m); }
        card.append(rels);
      }
    }
    card.append(problemList(key));
    card.onclick = () => { if (editing !== key) { editing = key; refresh(); } };
    return card;
  }

  function ruleCard(r, i) {
    const key = `r:${i}`;
    const card = el("div", "card rule-card");
    card.dataset.card = key;
    const errs = hasErrors(key);
    if (editing === key || errs) {
      card.classList.add("editing");
      const head = el("div", "row head",
        slot(`${key}:name`, r.name, (v) => (r.name = v), { cls: "name", placeholder: "name" }));
      const prems = el("div", "premises");
      r.premises.forEach((p, j) => {
        const s = slot(`${key}:p:${j}`, p, (v) => (r.premises[j] = v), {
          placeholder: "(e1, s) -> (e1', s')   or   n = n1 + n2",
          onEnter: () => { r.premises.splice(j + 1, 0, ""); refresh(); focusSlot(`${key}:p:${j + 1}`, 0, 0); },
        });
        prems.append(el("div", "row", s, button("×", "remove this premise", () => { r.premises.splice(j, 1); changed(); })));
      });
      prems.append(el("div", "row add", button("+ premise", "add a premise", () => {
        r.premises.push(""); refresh(); focusSlot(`${key}:p:${r.premises.length - 1}`, 0, 0);
      })));
      const concl = slot(`${key}:c`, r.conclusion, (v) => (r.conclusion = v), {
        cls: "conclusion", placeholder: "(e1 + e2, s) -> (e1' + e2, s')",
      });
      card.append(head, prems, el("div", "bar-line"), concl);
    } else {
      const math = el("div", "math");
      const t = tex.rules.get(key);
      if (t) katexInto(math, t, true);
      else math.append(el("span", "untypeset", `${r.name}  ${r.conclusion}`));
      card.append(math);
    }
    card.append(problemList(key), tools(card, i));
    card.onclick = () => { if (editing !== key) { editing = key; refresh(); } };
    dropTarget(card, (e) => {
      const box = card.getBoundingClientRect();
      return e.clientY < box.top + box.height / 2 ? i : i + 1;
    });
    return card;
  }

  // --- arranging rules --------------------------------------------------

  const freshName = (base) => {
    const taken = new Set(model.rules.map((r) => r.name));
    let n = base;
    while (taken.has(n)) n += "'";
    return n;
  };

  function insertRule(at) {
    let k = model.rules.length + 1;
    while (model.rules.some((r) => r.name === `rule${k}`)) k++;
    model.rules.splice(at, 0, { name: `rule${k}`, conclusion: "", premises: [] });
    editing = `r:${at}`;
    changed();
    focusSlot(`r:${at}:c`, 0, 0);
  }

  // Shown when hovering a rule: move (drag), duplicate, delete.
  function tools(card, i) {
    const r = model.rules[i];
    // a plain element: buttons can't be dragged in every browser
    const move = el("span", "handle", "move");
    move.title = "drag to reorder";
    move.draggable = true;
    move.onclick = (e) => e.stopPropagation();
    move.ondragstart = (e) => {
      dragging = i;
      e.dataTransfer.effectAllowed = "move";
      e.dataTransfer.setData("text/plain", r.name);
      e.dataTransfer.setDragImage(card, 16, 16);
      card.classList.add("dragging");
    };
    move.ondragend = () => { dragging = null; clearDropMarks(); card.classList.remove("dragging"); };
    const dup = button("duplicate", "duplicate this rule", () => {
      model.rules.splice(i + 1, 0, { ...r, name: freshName(r.name), premises: r.premises.slice() });
      editing = `r:${i + 1}`;
      changed();
    });
    const del = button("delete", "delete this rule", () => {
      model.rules.splice(i, 1);
      editing = null;
      changed();
    });
    return el("div", "tools", move, dup, del);
  }

  // The gap between two rules: hover to insert a rule there; also a place
  // to drop a rule being moved.
  function separator(at) {
    const sep = el("div", "sep", button("+ insert rule", "insert a rule here", () => insertRule(at)));
    dropTarget(sep, () => at);
    return sep;
  }

  let dragging = null;   // index of the rule being moved

  function clearDropMarks() {
    for (const m of root().querySelectorAll(".drop-before, .drop-after, .drop-here"))
      m.classList.remove("drop-before", "drop-after", "drop-here");
  }

  // [target] gives the index a dragged rule would land at.
  function dropTarget(node, target) {
    node.addEventListener("dragover", (e) => {
      if (dragging === null) return;
      e.preventDefault();
      e.dataTransfer.dropEffect = "move";
      clearDropMarks();
      const at = target(e);
      if (node.classList.contains("sep")) node.classList.add("drop-here");
      else node.classList.add(at === Number(node.dataset.card.slice(2)) ? "drop-before" : "drop-after");
    });
    node.addEventListener("drop", (e) => {
      if (dragging === null) return;
      e.preventDefault();
      let at = target(e);
      const [moved] = model.rules.splice(dragging, 1);
      if (at > dragging) at--;
      model.rules.splice(at, 0, moved);
      dragging = null;
      editing = null;
      changed();
    });
  }

  function refresh() {
    const r = root();
    const focused = document.activeElement && document.activeElement.dataset.key;
    const sel = focused && [document.activeElement.selectionStart, document.activeElement.selectionEnd];
    const cards = [grammarCard()];
    model.rules.forEach((rule, i) => cards.push(separator(i), ruleCard(rule, i)));
    cards.push(separator(model.rules.length));
    r.replaceChildren(...cards);
    if (focused) {
      const ta = r.querySelector(`textarea[data-key="${focused}"]`);
      if (ta) { ta.focus(); ta.setSelectionRange(sel[0], sel[1]); }
    }
    for (const w of r.querySelectorAll(".slot")) w.sync && w.sync();
  }

  function changed() { check(); refresh(); onChange(); }

  // Focusing a slot (e.g. by tabbing) edits its card; clicking outside every
  // card goes back to the typeset view.
  document.addEventListener("focusin", (e) => {
    const card = e.target.closest?.(".card");
    if (card && card.dataset.card !== editing) { editing = card.dataset.card; refresh(); }
  });
  document.addEventListener("mousedown", (e) => {
    if (editing && !e.target.closest?.(".card")) { editing = null; refresh(); }
  });

  return {
    load(text) {
      const o = circ.outline(text);
      if (!o.ok) return o;
      fromOutline(o);
      editing = null;
      check();
      refresh();
      onChange();
      return o;
    },
    text: () => serialize().text,
    byteToIndex,
    check,
    summary() {
      const all = [...diags.values()].flat().concat(cardDiags);
      return {
        errors: all.filter((d) => d.severity === "error").length,
        warnings: all.filter((d) => d.severity === "warning").length,
      };
    },
    set onChange(f) { onChange = f; },
  };
})();
