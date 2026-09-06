// What a page cannot do for itself. A new page's rail starts at the top, so the module it is about is scrolled into view; and the search field, hidden until here so a page is complete without script, ranks the bundle's index — every module, declaration and member, loaded by the script before this one — and shows the hits as rows in place of the tree.
(function () {
    var tree = document.querySelector(".tree");
    var current = document.querySelector(".tree a.current");
    var toggle = document.getElementById("rail-toggle");
    function reveal() {
        if (current) {
            current.scrollIntoView({ block: "center" });
        }
    }
    reveal();
    // Folded at a narrow width, the tree is not laid out at load, so it is scrolled when it opens instead.
    if (toggle) {
        toggle.addEventListener("change", function () {
            if (toggle.checked) {
                reveal();
            }
        });
    }

    var form = document.querySelector(".search");
    var field = form && form.querySelector("input");
    var eraser = form && form.querySelector(".clear");
    var results = document.querySelector(".results");
    var nav = document.querySelector(".tree nav");
    var label = document.querySelector(".tree label");
    if (!tree || !form || !field || !results || !nav || !window.curiosIndex) {
        return;
    }

    var root = document.documentElement.dataset.root || "";
    var LIMIT = 30;

    // A row's label is what a reader types: a member as owner/member, a declaration as its name, a module as its whole path; the rest of the path is where it lives.
    var corpus = window.curiosIndex.map(function (row) {
        var kind = row[0],
            path = row[1],
            href = row[2];
        var segments = path.split("/");
        var keep =
            kind === "mod"
                ? segments.length
                : kind === "case" || kind === "field" || kind === "method"
                  ? 2
                  : 1;
        var label = segments.slice(segments.length - keep).join("/");
        var where =
            kind === "mod"
                ? ""
                : segments.slice(0, segments.length - keep).join("/");
        return {
            kind: kind,
            label: label,
            where: where,
            href: root + href,
            lower: label.toLowerCase(),
            path: path.toLowerCase(),
        };
    });

    var hits = [];
    var chosen = -1;

    function narrow() {
        return label && getComputedStyle(label).display !== "none";
    }

    // Exact label, then label prefix, then label substring, then path substring; ties by label length, then the index's own order, which lists a parent before its children.
    function search(query) {
        var scored = [];
        for (var i = 0; i < corpus.length; i++) {
            var entry = corpus[i];
            var at = entry.lower.indexOf(query);
            var score;
            if (at === 0 && entry.lower.length === query.length) score = 0;
            else if (at === 0) score = 1;
            else if (at > 0) score = 2;
            else if (entry.path.indexOf(query) >= 0) score = 3;
            else continue;
            scored.push({ entry: entry, score: score, at: at, order: i });
        }
        scored.sort(function (a, b) {
            return (
                a.score - b.score ||
                a.entry.label.length - b.entry.label.length ||
                a.order - b.order
            );
        });
        return scored.slice(0, LIMIT);
    }

    function span(className, text) {
        var element = document.createElement("span");
        element.className = className;
        element.textContent = text;
        return element;
    }

    function render(query) {
        results.textContent = "";
        chosen = -1;
        if (hits.length === 0) {
            var none = document.createElement("div");
            none.className = "none";
            none.textContent = "No match";
            results.appendChild(none);
        }
        hits.forEach(function (hit) {
            var entry = hit.entry;
            var row = document.createElement("a");
            row.className = "entry";
            row.href = entry.href;
            row.appendChild(span("kind", entry.kind));
            var text = span("label", "");
            if (hit.at >= 0) {
                text.appendChild(
                    document.createTextNode(entry.label.slice(0, hit.at)),
                );
                var mark = document.createElement("mark");
                mark.textContent = entry.label.slice(
                    hit.at,
                    hit.at + query.length,
                );
                text.appendChild(mark);
                text.appendChild(
                    document.createTextNode(
                        entry.label.slice(hit.at + query.length),
                    ),
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
        var rows = results.querySelectorAll("a");
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
        var query = field.value.trim().toLowerCase();
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
    field.addEventListener("keydown", function (event) {
        var live = tree.classList.contains("live");
        if (event.key === "ArrowDown") {
            event.preventDefault();
            if (live) {
                choose(chosen + 1);
            } else {
                if (toggle && narrow()) {
                    toggle.checked = true;
                }
                var first = nav.querySelector("a");
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
            var rows = results.querySelectorAll("a");
            var target = rows[chosen >= 0 ? chosen : 0];
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
        eraser.addEventListener("click", function () {
            field.value = "";
            update();
            field.focus();
        });
    }
    results.addEventListener("click", function (event) {
        var row = event.target.closest("a");
        if (row) {
            event.preventDefault();
            follow(row.href);
        }
    });
    nav.addEventListener("click", function (event) {
        var row = event.target.closest("a");
        if (
            row &&
            toggle &&
            narrow() &&
            row.getAttribute("href").charAt(0) === "#"
        ) {
            toggle.checked = false;
        }
    });
    document.addEventListener("keydown", function (event) {
        var editing =
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
    form.addEventListener("submit", function (event) {
        event.preventDefault();
    });

    form.hidden = false;
    if (field.value !== "") {
        update();
    }
})();
