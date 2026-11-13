// The page a browser build writes, under Node's own test runner: fixture packages written here, each gateway pinned by the hash of what is written, compiled with `--target browser` by the compiler `CURIOS` names, and each page's loader imported and started with the program's bytes, as a browser starts it with what it fetches. It holds what only a written page can get wrong — a gateway answering its row through the loader, the arguments a query spells, the posts a framed page makes to its parent, an export that is no function, the layout a package's files take, the refusal of a row with no browser table — which neither the harness's suite nor the page's Rust tests reach. `cargo xtask compiler test` builds the compiler as it is released and runs this.

import { test } from "node:test";
import assert from "node:assert/strict";
import { spawn, spawnSync } from "node:child_process";
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

// A package is written for one compiler, so each fixture's manifest states the version of the one under test.
const version = spawnSync(compiler, ["--version"], { encoding: "utf8" })
  .stdout.trim()
  .split(" ")
  .pop();

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

  return `compiler = "${version}"\nname = "${name}"\n${rest}\n[[foreign]]\nname = "${row}"\n\n[foreign.browser]\npath = "${path}"\nhash = "${pinned(gateway)}"\n\n[foreign.browser.static]\n${served}\n[foreign.answers]\n${answered}`;
};

/** `curios compile --target browser`, run in `directory` with `rest` after it. */
const compiled = (directory, rest = []) =>
  spawnSync(compiler, ["compile", "--target", "browser", ...rest], {
    cwd: directory,
    encoding: "utf8",
  });

/** Compile the package at `directory` for a browser into `page`, then start the page's loader with the program's bytes and `query`, beside the lines it wrote to the console, where a page's output goes with no document to write it into — and, `framed`, under a parent window that keeps every message posted to it, beside what it posted. */
const started = async (
  t,
  directory,
  page,
  { query = "", framed = false } = {},
) => {
  const { status, stderr } = compiled(directory, ["-o", page]);

  assert.equal(status, 0, stderr);

  const out = [];
  const err = [];
  const posted = [];
  t.mock.method(console, "log", (line) => out.push(line));
  t.mock.method(console, "error", (line) => err.push(line));

  if (framed) {
    globalThis.parent = {
      postMessage: (message, origin) => posted.push([message, origin]),
    };
    t.after(() => {
      globalThis.parent = undefined;
    });
  }

  const loader = await import(pathToFileURL(join(page, "index.js")).href);
  const outcome = await loader.start({
    program: readFileSync(join(page, "index.wasm")),
    query,
  });

  return { ...outcome, out, err, posted };
};

const DOUBLE = "pub foreign double(Nat) -> Nat;\n";

const DOUBLED = `let main() -> /std/Io({}) =
    let n = /app/double(21)!;
    /std/print(/std/Nat/to_str(n));
main()
`;

/** A package `app` whose one row's gateway at `web/dom.js` answers `double`, and whose program prints `42` through it. */
const doubling = (t) => {
  const gateway = "export const twice = (n) => n * 2n;\n";

  return tree(t, {
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
};

test("a gateway's export answers its row through the loader", async (t) => {
  const root = doubling(t);

  // Under Node the page stands in no frame, so it posts nowhere.
  assert.equal(globalThis.parent, undefined);
  const outcome = await started(t, join(root, "app"), join(root, "page"));

  assert.equal(outcome.trap, null);
  assert.equal(outcome.exitCode, 0);
  // A program's entry answers the unit, which is no word.
  assert.equal(outcome.held, null);
  assert.deepEqual(outcome.out, ["42"]);
  assert.deepEqual(outcome.posted, []);
});

test("a framed page posts started, each write as it is made, then its outcome, at its own origin", async (t) => {
  const root = doubling(t);

  const { posted } = await started(t, join(root, "app"), join(root, "page"), {
    framed: true,
  });

  assert.deepEqual(posted[0], [{ type: "started" }, "/"]);
  const writes = posted.slice(1, -1);
  assert.ok(writes.length > 0);
  for (const [message, origin] of writes) {
    assert.equal(origin, "/");
    assert.equal(message.type, "wrote");
    assert.equal(message.stream, "stdout");
  }
  assert.equal(
    Buffer.concat(writes.map(([message]) => message.bytes)).toString(),
    "42",
  );
  assert.deepEqual(posted.at(-1), [
    { type: "outcome", outcome: { exitCode: 0, trap: null, held: null } },
    "/",
  ]);
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
    "app/curios.toml": `compiler = "${version}"\nname = "app"\n`,
    "app/exe.crs": `let main() -> /std/Io({}) =
    let args = /std/Proc/args()!;
    let _ = /std/Try/run(/std/Io/write(/std/Proc/stdout, /std/Bytes/join(x[124], args)))!;
    /std/Io/pure(());
main()
`,
  });

  const outcome = await started(t, join(root, "app"), join(root, "page"), {
    query: "?arg=one&other=skipped&arg=a+b%20c&arg=%C3%A9&arg=",
  });

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
    "app/curios.toml": `compiler = "${version}"\nname = "app"\n`,
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
    "app/curios.toml": `compiler = "${version}"\nname = "app"\n\n[[foreign]]\nname = "native"\n\n[foreign.native]\npath = "plugin.wasm"\nhash = "${pinned("")}"\n\n[foreign.answers]\ndouble = "double"\n`,
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
    /`\/app\/double` is answered by the foreign row `native` of `\/app`\n│ {2}that row states no `\[foreign\.browser\]`\n│ {2}the browser target has nothing to link for it/,
  );
  assert.ok(!existsSync(join(root, "page")));
});

/** `curios test --target browser` started in `directory` on a port of loopback's own: the port read off the line that says its address, its streams kept, and the command's end awaited. */
const serving = (t, directory) => {
  const child = spawn(
    compiler,
    ["test", "--target", "browser", "--port", "0"],
    {
      cwd: directory,
    },
  );
  const out = [];
  const err = [];
  child.stdout.on("data", (chunk) => out.push(chunk));
  child.stderr.on("data", (chunk) => err.push(chunk));
  t.after(() => child.kill());

  const port = new Promise((resolve) => {
    child.stderr.on("data", (chunk) => {
      const match = /http:\/\/127\.0\.0\.1:(\d+)\//.exec(String(chunk));

      if (match !== null) {
        resolve(Number(match[1]));
      }
    });
  });
  const ended = new Promise((resolve) => {
    child.on("close", (code) =>
      resolve({
        code,
        stdout: Buffer.concat(out).toString(),
        stderr: Buffer.concat(err).toString(),
      }),
    );
  });

  return { port, ended };
};

test("a test page served by the command answers its word through its gateway, and the run ends on its outcome", async (t) => {
  const gateway = "export const twice = (n) => n * 2n;\n";
  const root = tree(t, {
    "app/curios.toml": manifest({
      name: "app",
      row: "dom",
      path: "web/dom.js",
      gateway,
      answers: { double: "twice" },
    }),
    "app/lib.crs": `use /std/{Nat, Test};\n\n${DOUBLE}\ntest doubling = Test/equal(double(21)!, 42);\n`,
    "app/exe.crs": DOUBLED,
    "app/web/dom.js": gateway,
  });
  const { port, ended } = serving(t, join(root, "app"));
  const base = `http://127.0.0.1:${await port}`;

  // The runner at the root, with the key a page spells its arguments under; the page beside it, fetched into a directory of its own as a browser fetches it.
  const runner = await (await fetch(`${base}/`)).text();
  const run = JSON.parse(
    /<script type="application\/json" id="run">(.*?)<\/script>/.exec(runner)[1],
  );
  const page = join(root, "fetched");
  for (const path of ["index.html", "index.js", "web/dom.js"]) {
    mkdirSync(dirname(join(page, path)), { recursive: true });
    writeFileSync(
      join(page, path),
      await (await fetch(`${base}/${path}`)).text(),
    );
  }
  const program = new Uint8Array(
    await (await fetch(`${base}/index.wasm`)).arrayBuffer(),
  );

  // The run's socket, as the runner opens it: sent the report so far and the first step at once.
  const socket = new WebSocket(`ws://127.0.0.1:${await port}/`);
  const replies = [];
  const waiting = [];
  socket.addEventListener("message", (event) => {
    const reply = JSON.parse(event.data);
    const waiter = waiting.shift();
    if (waiter === undefined) {
      replies.push(reply);
    } else {
      waiter(reply);
    }
  });
  const replied = () =>
    replies.length > 0
      ? Promise.resolve(replies.shift())
      : new Promise((resolve) => waiting.push(resolve));
  // The run so far ends on the node of the step it hands over, opened as the step is: the test in hand is the one whose line stands open.
  const opening = await replied();
  assert.ok(
    Buffer.from(opening.written, "base64")
      .toString()
      .endsWith(`serve; ${base}/\n/app\n├─ /app/doubling`),
  );
  assert.deepEqual(opening.next, { subject: 0, test: 0 });

  const posted = [];
  globalThis.parent = {
    postMessage: (message, origin) => posted.push([message, origin]),
  };
  t.after(() => {
    globalThis.parent = undefined;
  });
  t.mock.method(console, "log", () => {});
  const loader = await import(pathToFileURL(join(page, "index.js")).href);
  const outcome = await loader.start({
    program,
    query: `?${run.argument}=0`,
  });

  assert.equal(outcome.trap, null);
  assert.equal(outcome.held, true);
  assert.equal(posted[0][0].type, "started");
  assert.equal(posted.at(-1)[0].outcome.held, true);

  socket.send(JSON.stringify({ outcome: posted.at(-1)[0].outcome }));
  const reply = await replied();
  assert.equal(reply.next, null);
  assert.match(
    Buffer.from(reply.written, "base64").toString(),
    /^; passed in \d+\.\ds\n╰─ 1 passed, 0 failed; \d+\.\ds\n1 passed, 0 failed; \d+\.\ds\n$/,
  );

  // A test that held wrote nothing, and the report is standard error's.
  const { code, stdout } = await ended;
  assert.equal(code, 0);
  assert.equal(stdout, "");
});
