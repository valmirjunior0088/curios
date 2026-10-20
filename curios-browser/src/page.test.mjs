// The page a browser build writes, under Node's own test runner: fixture packages written here, each gateway pinned by the hash of what is written, compiled with `--target browser` by the compiler `CURIOS` names, and each page's loader imported and started with the program's bytes, as a browser starts it with what it fetches. It holds what only a written page can get wrong — a gateway answering its row through the loader, the arguments a query spells, an export that is no function, the layout a package's files take, the refusal of a row with no browser table — which neither the harness's suite nor the page's Rust tests reach. `cargo xtask compiler test` builds the compiler as it is released and runs this.

import { test } from "node:test";
import assert from "node:assert/strict";
import { spawnSync } from "node:child_process";
import { createHash } from "node:crypto";
import {
  existsSync,
  mkdirSync,
  mkdtempSync,
  readFileSync,
  rmSync,
  writeFileSync,
} from "node:fs";
import { tmpdir } from "node:os";
import { dirname, join } from "node:path";
import { pathToFileURL } from "node:url";

const compiler = process.env.CURIOS;

assert.ok(compiler, "CURIOS names the compiler under test");

/** The `f1:` hash a manifest pins `contents` by: SHA-256 over its bytes. */
const pinned = (contents) =>
  `f1:${createHash("sha256").update(contents).digest("hex")}`;

/** A directory of its own holding `files`, each contents by its path, gone when the test ends. */
const tree = (t, files) => {
  const root = mkdtempSync(join(tmpdir(), "curios-page-"));

  t.after(() => rmSync(root, { recursive: true, force: true }));

  for (const [path, contents] of Object.entries(files)) {
    mkdirSync(dirname(join(root, path)), { recursive: true });
    writeFileSync(join(root, path), contents);
  }

  return root;
};

/** A package `name`'s manifest, its one `[[foreign]]` row `row` carrying `gateway` at `path`, answering `answers` and serving `statics`, beside `rest`. */
const manifest = ({
  name,
  row,
  path,
  gateway,
  answers,
  statics = {},
  rest = "",
}) => {
  const answered = Object.entries(answers)
    .map(([declaration, name]) => `${declaration} = "${name}"\n`)
    .join("");
  const served = Object.entries(statics)
    .map(([page, path]) => `"${page}" = "${path}"\n`)
    .join("");

  return `compiler = "0.0.0"\nname = "${name}"\n${rest}\n[[foreign]]\nname = "${row}"\n\n[foreign.browser]\npath = "${path}"\nhash = "${pinned(gateway)}"\n\n[foreign.browser.static]\n${served}\n[foreign.answers]\n${answered}`;
};

/** `curios compile --target browser`, run in `directory` with `rest` after it. */
const compiled = (directory, rest = []) =>
  spawnSync(compiler, ["compile", "--target", "browser", ...rest], {
    cwd: directory,
    encoding: "utf8",
  });

/** Compile the package at `directory` for a browser into `page`, then start the page's loader with the program's bytes and `query`, beside the lines it wrote to the console, where a page's output goes with no document to write it into. */
const started = async (t, directory, page, query = "") => {
  const { status, stderr } = compiled(directory, ["-o", page]);

  assert.equal(status, 0, stderr);

  const out = [];
  const err = [];
  t.mock.method(console, "log", (line) => out.push(line));
  t.mock.method(console, "error", (line) => err.push(line));

  const loader = await import(pathToFileURL(join(page, "index.js")).href);
  const outcome = await loader.start({
    program: readFileSync(join(page, "index.wasm")),
    query,
  });

  return { ...outcome, out, err };
};

const DOUBLE = "pub foreign double(Nat) -> Nat;\n";

const DOUBLED = `let main() -> /std/Io({}) =
    let n = /app/double(21)!;
    /std/print(/std/Nat/to_str(n));
main()
`;

test("a gateway's export answers its row through the loader", async (t) => {
  const gateway = "export const twice = (n) => n * 2n;\n";
  const root = tree(t, {
    "app/curios.toml": manifest({
      name: "app",
      row: "dom",
      path: "web/dom.js",
      gateway,
      answers: { double: "twice" },
    }),
    "app/lib.crs": DOUBLE,
    "app/exe.crs": DOUBLED,
    "app/web/dom.js": gateway,
  });

  const outcome = await started(t, join(root, "app"), join(root, "page"));

  assert.equal(outcome.trap, null);
  assert.equal(outcome.exitCode, 0);
  assert.deepEqual(outcome.out, ["42"]);
});

test("an export that is no function is refused before the program runs, naming the row", async (t) => {
  const gateway = "export const twice = 2;\n";
  const root = tree(t, {
    "app/curios.toml": manifest({
      name: "app",
      row: "dom",
      path: "dom.js",
      gateway,
      answers: { double: "twice" },
    }),
    "app/lib.crs": DOUBLE,
    "app/exe.crs": DOUBLED,
    "app/dom.js": gateway,
  });

  const outcome = await started(t, join(root, "app"), join(root, "page"));

  const refusal =
    'the gateway dom.js of the foreign row "dom" of app exports no function twice, which answers /app/double';

  assert.equal(outcome.exitCode, null);
  assert.equal(outcome.trap, refusal);
  assert.deepEqual(outcome.err, [refusal]);
});

test("the program's arguments are its name, then each arg= of the query, percent-decoded to bytes", async (t) => {
  const root = tree(t, {
    "app/curios.toml": 'compiler = "0.0.0"\nname = "app"\n',
    "app/exe.crs": `let main() -> /std/Io({}) =
    let args = /std/Process/args()!;
    let _ = /std/Try/run(/std/Io/write(/std/Process/stdout, /std/Bytes/join(x[124], args)))!;
    /std/Io/pure(());
main()
`,
  });

  const outcome = await started(
    t,
    join(root, "app"),
    join(root, "page"),
    "?arg=one&other=skipped&arg=a+b%20c&arg=%C3%A9&arg=",
  );

  assert.equal(outcome.trap, null);
  assert.deepEqual(outcome.out, ["app|one|a+b c|é|"]);
});

test("a dependency's files stand under its name, and a static entry's at its page path, dotted names skipped", async (t) => {
  const own = "export const twice = (n) => n * 2n;\n";
  const theirs = "export const halve = (n) => n / 2n;\n";
  const root = tree(t, {
    "app/curios.toml": manifest({
      name: "app",
      row: "dom",
      path: "dom.js",
      gateway: own,
      answers: { double: "twice" },
      statics: { assets: "web" },
      rest: '\n[dependencies]\nchart = { source = "path", path = "../chart" }\n',
    }),
    "app/lib.crs": DOUBLE,
    "app/exe.crs": `let main() -> /std/Io({}) =
    let n = /chart/half(/app/double(21)!)!;
    /std/print(/std/Nat/to_str(n));
main()
`,
    "app/dom.js": own,
    "app/web/logo.svg": "<svg/>",
    "app/web/.DS_Store": "",
    "chart/curios.toml": manifest({
      name: "chart",
      row: "draw",
      path: "gateway/draw.js",
      gateway: theirs,
      answers: { half: "halve" },
    }),
    "chart/lib.crs": "pub foreign half(Nat) -> Nat;\n",
    "chart/gateway/draw.js": theirs,
  });
  const page = join(root, "page");

  const outcome = await started(t, join(root, "app"), page);

  assert.equal(outcome.trap, null);
  assert.deepEqual(outcome.out, ["21"]);
  for (const path of [
    "index.html",
    "index.js",
    "index.wasm",
    "dom.js",
    "assets/logo.svg",
    "chart/gateway/draw.js",
  ]) {
    assert.ok(existsSync(join(page, path)), path);
  }
  assert.ok(!existsSync(join(page, "assets/.DS_Store")));
});

test("a page is written under the store when no --output is given, holding nothing an earlier build left", async (t) => {
  const root = tree(t, {
    "app/curios.toml": 'compiler = "0.0.0"\nname = "app"\n',
    "app/exe.crs": '/std/print("hello")\n',
  });
  const page = join(root, "app/.curios/browser/app/app");

  mkdirSync(page, { recursive: true });
  writeFileSync(join(page, "stale.js"), "");

  const { status, stderr } = compiled(join(root, "app"));

  assert.equal(status, 0, stderr);
  assert.ok(existsSync(join(page, "index.html")));
  assert.ok(!existsSync(join(page, "stale.js")));
});

test("a row that states no browser table is refused for a browser, naming the row and the table", async (t) => {
  const root = tree(t, {
    "app/curios.toml": `compiler = "0.0.0"\nname = "app"\n\n[[foreign]]\nname = "native"\n\n[foreign.native]\npath = "plugin.wasm"\nhash = "${pinned("")}"\n\n[foreign.answers]\ndouble = "double"\n`,
    "app/lib.crs": DOUBLE,
    "app/exe.crs": DOUBLED,
  });

  const { status, stderr } = compiled(join(root, "app"), [
    "-o",
    join(root, "page"),
  ]);

  assert.equal(status, 1);
  assert.match(
    stderr,
    /`\/app\/double` is answered by the foreign row "native" of app, which states no `\[foreign\.browser\]`: the browser target has nothing to link for it/,
  );
  assert.ok(!existsSync(join(root, "page")));
});
