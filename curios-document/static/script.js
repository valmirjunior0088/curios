// What a page cannot do for itself. A new page's rail starts at the top, so the module it is about is scrolled into view; and the search field, hidden until here so a page is complete without script, ranks the bundle's index — every module, declaration and member, loaded by the script before this one — and shows the hits as rows in place of the tree.
(() => {
  const tree = document.querySelector(".tree");
  const current = document.querySelector(".tree a.current");
  const toggle = document.getElementById("rail-toggle");
  function reveal() {
    if (current) {
      current.scrollIntoView({ block: "center" });
    }
  }
  reveal();
  // Folded at a narrow width, the tree is not laid out at load, so it is scrolled when it opens instead.
  if (toggle) {
    toggle.addEventListener("change", () => {
      if (toggle.checked) {
        reveal();
      }
    });
  }

  const form = document.querySelector(".search");
  const field = form?.querySelector("input");
  const eraser = form?.querySelector(".clear");
  const results = document.querySelector(".results");
  const nav = document.querySelector(".tree nav");
  const label = document.querySelector(".tree label");
  if (!tree || !form || !field || !results || !nav || !window.curiosIndex) {
    return;
  }

  const root = document.documentElement.dataset.root || "";
  const LIMIT = 30;

  // A row's label is what a reader types: a member as owner/member, a declaration as its name, a module as its whole path; the rest of the path is where it lives.
  const corpus = window.curiosIndex.map((row) => {
    const kind = row[0],
      path = row[1],
      href = row[2];
    const segments = path.split("/");
    const keep =
      kind === "mod"
        ? segments.length
        : kind === "case" || kind === "field" || kind === "method"
          ? 2
          : 1;
    const label = segments.slice(segments.length - keep).join("/");
    const where =
      kind === "mod" ? "" : segments.slice(0, segments.length - keep).join("/");
    return {
      kind: kind,
      label: label,
      where: where,
      href: root + href,
      lower: label.toLowerCase(),
      path: path.toLowerCase(),
    };
  });

  let hits = [];
  let chosen = -1;

  function narrow() {
    return label && getComputedStyle(label).display !== "none";
  }

  // Exact label, then label prefix, then label substring, then path substring; ties by label length, then the index's own order, which lists a parent before its children.
  function search(query) {
    const scored = [];
    for (let i = 0; i < corpus.length; i++) {
      const entry = corpus[i];
      const at = entry.lower.indexOf(query);
      let score;
      if (at === 0 && entry.lower.length === query.length) score = 0;
      else if (at === 0) score = 1;
      else if (at > 0) score = 2;
      else if (entry.path.indexOf(query) >= 0) score = 3;
      else continue;
      scored.push({ entry: entry, score: score, at: at, order: i });
    }
    scored.sort(
      (a, b) =>
        a.score - b.score ||
        a.entry.label.length - b.entry.label.length ||
        a.order - b.order,
    );
    return scored.slice(0, LIMIT);
  }

  function span(className, text) {
    const element = document.createElement("span");
    element.className = className;
    element.textContent = text;
    return element;
  }

  function render(query) {
    results.textContent = "";
    chosen = -1;
    if (hits.length === 0) {
      const none = document.createElement("div");
      none.className = "none";
      none.textContent = "No match";
      results.appendChild(none);
    }
    hits.forEach((hit) => {
      const entry = hit.entry;
      const row = document.createElement("a");
      row.className = "entry";
      row.href = entry.href;
      row.appendChild(span("kind", entry.kind));
      const text = span("label", "");
      if (hit.at >= 0) {
        text.appendChild(document.createTextNode(entry.label.slice(0, hit.at)));
        const mark = document.createElement("mark");
        mark.textContent = entry.label.slice(hit.at, hit.at + query.length);
        text.appendChild(mark);
        text.appendChild(
          document.createTextNode(entry.label.slice(hit.at + query.length)),
        );
      } else {
        text.textContent = entry.label;
      }
      row.appendChild(text);
      if (entry.where) {
        row.appendChild(span("where", entry.where));
      }
      results.appendChild(row);
    });
    results.hidden = false;
    tree.classList.add("live");
  }

  function clear() {
    hits = [];
    chosen = -1;
    results.textContent = "";
    results.hidden = true;
    tree.classList.remove("live");
  }

  function choose(index) {
    const rows = results.querySelectorAll("a");
    if (chosen >= 0 && rows[chosen]) {
      rows[chosen].removeAttribute("aria-current");
    }
    chosen = Math.max(-1, Math.min(rows.length - 1, index));
    if (chosen >= 0) {
      rows[chosen].setAttribute("aria-current", "true");
      rows[chosen].scrollIntoView({ block: "nearest" });
    }
  }

  function update() {
    if (eraser) {
      eraser.hidden = field.value === "";
    }
    const query = field.value.trim().toLowerCase();
    if (query === "") {
      clear();
      return;
    }
    hits = search(query);
    render(query);
  }

  function follow(href) {
    // At a narrow width the rail is pinned, so a same-page anchor is reached with the tree folded.
    if (toggle && narrow()) {
      toggle.checked = false;
    }
    location.href = href;
  }

  field.addEventListener("input", update);
  field.addEventListener("keydown", (event) => {
    const live = tree.classList.contains("live");
    if (event.key === "ArrowDown") {
      event.preventDefault();
      if (live) {
        choose(chosen + 1);
      } else {
        if (toggle && narrow()) {
          toggle.checked = true;
        }
        const first = nav.querySelector("a");
        if (first) {
          first.focus();
        }
      }
    } else if (event.key === "ArrowUp") {
      event.preventDefault();
      if (live) {
        choose(chosen - 1);
      }
    } else if (event.key === "Enter") {
      event.preventDefault();
      const rows = results.querySelectorAll("a");
      const target = rows[chosen >= 0 ? chosen : 0];
      if (live && target) {
        follow(target.href);
      }
    } else if (event.key === "Escape") {
      if (field.value === "") {
        field.blur();
      } else {
        field.value = "";
        update();
      }
    }
  });
  if (eraser) {
    eraser.addEventListener("click", () => {
      field.value = "";
      update();
      field.focus();
    });
  }
  results.addEventListener("click", (event) => {
    const row = event.target.closest("a");
    if (row) {
      event.preventDefault();
      follow(row.href);
    }
  });
  nav.addEventListener("click", (event) => {
    const row = event.target.closest("a");
    if (
      row &&
      toggle &&
      narrow() &&
      row.getAttribute("href").charAt(0) === "#"
    ) {
      toggle.checked = false;
    }
  });
  document.addEventListener("keydown", (event) => {
    const editing =
      event.target.isContentEditable ||
      /^(INPUT|TEXTAREA|SELECT)$/.test(event.target.tagName);
    if (
      event.key === "/" &&
      !editing &&
      !event.ctrlKey &&
      !event.metaKey &&
      !event.altKey
    ) {
      event.preventDefault();
      field.focus();
      field.select();
    }
  });
  form.addEventListener("submit", (event) => {
    event.preventDefault();
  });

  form.hidden = false;
  if (field.value !== "") {
    update();
  }
})();
