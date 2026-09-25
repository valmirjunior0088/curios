// The browser harness under Node's own test runner, against the bundle `cargo x js` filed: fixture programs compiled by the bundle's `compile` and run by its `run` against scripted hooks. It holds what only a JavaScript host can get wrong — a `foreign` hook held to its row, a handle's exact token, the harness's answers to the host rows — which no Rust test reaches, since the harness runs only under a JavaScript engine. `cargo x js-test` builds the bundle and runs this.

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";

const bundle = new URL(
  "../.artifacts/wasm32-unknown-unknown/",
  import.meta.url,
);
const { initSync, compile, run } = await import(
  new URL("curios_js.js", bundle)
);

initSync({ module: readFileSync(new URL("curios_js_bg.wasm", bundle)) });

const text = (bytes) => new TextDecoder().decode(bytes);

/** Compile `source` and run it against `hooks`, the output streams read as text. */
const execute = async (source, hooks = {}) => {
  const outcome = await run(compile(source), hooks);

  return {
    ...outcome,
    stdout: text(outcome.stdout),
    stderr: text(outcome.stderr),
  };
};

/** `source`, run against `hooks`, is stopped with a trap `pattern` matches. */
const stopped = async (source, hooks, pattern) => {
  const { exitCode, trap } = await execute(source, hooks);

  assert.equal(exitCode, null);
  assert.match(trap, pattern);
};

const BYTE = `
use /std/{Byte, Nat, print};
foreign byte: (Byte) -> Byte;
let b = byte(7)!;
print(Nat/to_str(Byte/to_nat(b)))
`;

const FLAG = `
use /std/{print};
foreign flag: Bool;
let b = flag!;
print(match b | true => "yes" | false => "no" end)
`;

const PAIR = `
use /std/{Nat, Str, print};
foreign pair: (Nat, Bytes) -> {code: Nat, ok: Bool};
let r = pair(1, x[])!;
print(Str/flatten([Nat/to_str(r.code), " ", match r.ok | true => "ok" | false => "not ok" end]))
`;

const TAKE = `
foreign take: (Nat, Int, Bool, Byte, Flt, Bytes, Bits, List(Nat), List(Int), List(Bool), List(Bytes), List(Handle)) -> {};
take(18446744073709551615, -5, true, 200, 1.5, x[1, 2], b[1, 0, 1], [1, 2], [-1, 3], [true, false], [x[7], x[]], [/std/Handle/stdout])
`;

const LISTS = `
use /std/{Nat, Str, List, print};
foreign lists: List(Nat);
let l = lists!;
print(Str/join(",", List/map(l, Nat/to_str)))
`;

const KEEP = `
use /std/{Io};
foreign keep: (Bytes) -> Bytes;
foreign poke: {};
let b = x[0x61, 0x62, 0x63];
let r = keep(b)!;
let _ = poke!;
let _ = Io/write(Io/stdout, b)!;
Io/write(Io/stdout, r)
`;

const WRITE = `
use /std/{Async, Handle, Show, print};
foreign token: Handle;
let h = token!;
let r = Async/Write/write(h, x[0x68, 0x69])!;
match r
| success(_) => print(" wrote")
| failure(e) => print(Show/show(e))
end
`;

const DIRECTION = `
use /std/{Async, Handle, Io, Show, Str, print};
foreign token: Handle;
let said(c: Io/Chunk) -> Str =
    match c | chunk(_) => "chunk" | eof() => "eof" | error(e) => Show/show(e) end;
let w = Async/Write/write(Handle/stdin, x[1])!;
let out = Async/Read/read(Handle/stdout, 4)!;
let nothing = Async/Read/read(Handle/stdin, 0)!;
let rest = Async/Read/read(Handle/stdin, 4)!;
let h = token!;
let unknown = Async/Read/read(h, 4)!;
print(Str/join(" ", [match w | success(_) => "wrote" | failure(f) => Show/show(f) end, said(out), said(nothing), said(rest), said(unknown)]))
`;

const POLL = `
use /std/{Handle, List, Str, print};
use /std/Handle/{Interest, Readiness};
foreign token: Handle;
let said(r: Readiness) -> Str =
    choose
    | r.failed => "err"
    | r.fired.writable => "write"
    | r.fired.readable => "read"
    | _ => "none"
    end;
let h = token!;
let rs = Handle/poll([h, Handle/stdout, Handle/stdin], [Interest/readable, Interest/writable, Interest/readable], 0)!;
print(Str/join(" ", List/map(rs, said)))
`;

const UNEQUAL = `
use /std/{Handle};
use /std/Handle/{Interest};
let _ = Handle/poll([Handle/stdin], [Interest/readable, Interest/writable], 0)!;
/std/print("polled")
`;

const HOOKS = `
foreign used: Nat;
foreign _unused: Nat;
let _ = used!;
/std/print("ran")
`;

test("compile hands back the program beside its own foreign rows as plain data", () => {
  const { program, foreigns } = compile(PAIR);

  assert.ok(program instanceof Uint8Array);
  assert.deepEqual(foreigns, [
    {
      name: "/pair",
      params: ["Nat", "Bytes"],
      results: [
        { label: "code", type: "Nat" },
        { label: "ok", type: "Bool" },
      ],
    },
  ]);
});

test("a Byte crosses whole at 0 and 255, and a hook's answer outside them stops the program", async () => {
  let handed;
  const answering = (answer) => ({
    foreign: {
      "/byte": (b) => {
        handed = b;

        return answer;
      },
    },
  });

  assert.equal((await execute(BYTE, answering(0))).stdout, "0");
  assert.equal(handed, 7);
  assert.equal((await execute(BYTE, answering(255))).stdout, "255");

  for (const answer of [256, -1, 1.5, 7n, "7"]) {
    await stopped(
      BYTE,
      answering(answer),
      /ffi \/byte: the result is .+, not an integer from 0 to 255/,
    );
  }
});

test("a Bool is answered as a boolean, and a 1 in its place stops the program", async () => {
  const answering = (answer) => ({ foreign: { "/flag": () => answer } });

  assert.equal((await execute(FLAG, answering(true))).stdout, "yes");
  assert.equal((await execute(FLAG, answering(false))).stdout, "no");
  await stopped(
    FLAG,
    answering(1),
    /ffi \/flag: the result is number 1, not a boolean/,
  );
});

test("a row with several results is answered by an object keyed by exactly their labels", async () => {
  let handed;
  const answering = (answer) => ({
    foreign: {
      "/pair": (...operands) => {
        handed = operands;

        return answer;
      },
    },
  });

  assert.equal(
    (await execute(PAIR, answering({ code: 3n, ok: true }))).stdout,
    "3 ok",
  );
  assert.deepEqual(handed, [1n, new Uint8Array()]);
  await stopped(PAIR, answering([3n, true]), /not an object of code, ok/);
  await stopped(PAIR, answering({ code: 3n }), /the hook answered no ok/);
  await stopped(
    PAIR,
    answering({ code: 3n, ok: true, extra: 1 }),
    /the hook answered extra, which the row has no result of/,
  );
  await stopped(
    PAIR,
    answering({ code: 3, ok: true }),
    /code is number 3, not a BigInt from 0n to 2⁶⁴ - 1/,
  );
});

test("a hook is handed each operand as the JavaScript value its type names", async () => {
  let handed;

  const { trap } = await execute(TAKE, {
    foreign: {
      "/take": (...operands) => {
        handed = operands;
      },
    },
  });

  assert.equal(trap, null);
  assert.deepEqual(handed, [
    2n ** 64n - 1n,
    -5n,
    true,
    200,
    1.5,
    Uint8Array.of(1, 2),
    Uint8Array.of(0b101),
    [1n, 2n],
    [-1n, 3n],
    [true, false],
    [Uint8Array.of(7), new Uint8Array()],
    [Uint8Array.of(1)],
  ]);
});

test("a list result is an array of its elements, each held to the element type", async () => {
  const answering = (answer) => ({ foreign: { "/lists": () => answer } });

  assert.equal(
    (await execute(LISTS, answering([1n, 2n ** 64n - 1n]))).stdout,
    "1,18446744073709551615",
  );
  await stopped(
    LISTS,
    answering([1n, 1]),
    /the result\[1\] is number 1, not a BigInt/,
  );
  await stopped(LISTS, answering(1n), /the result is 1n, not an array/);
});

test("a buffer a hook keeps is a copy, so changing it afterwards changes nothing the program holds", async () => {
  let kept;
  let answered;

  const { stdout } = await execute(KEEP, {
    foreign: {
      "/keep": (bytes) => {
        kept = bytes;
        answered = Uint8Array.of(0x78, 0x79, 0x7a);

        return answered;
      },
      "/poke": () => {
        kept.fill(0x21);
        answered.fill(0x21);
      },
    },
    onStdout: (bytes) => bytes.fill(0x3f),
  });

  assert.equal(stdout, "abcxyz");
});

test("a handle is its token's exact bytes: the empty, a padded and a long token name no stream", async () => {
  const writing = (token) =>
    execute(WRITE, { foreign: { "/token": () => token } });

  assert.equal((await writing(Uint8Array.of(1))).stdout, "hi wrote");

  const standardError = await writing(Uint8Array.of(2));

  assert.equal(standardError.stderr, "hi");
  assert.equal(standardError.stdout, " wrote");

  for (const token of [
    new Uint8Array(),
    Uint8Array.of(1, 0),
    new Uint8Array(9).fill(0xff),
  ]) {
    const { stdout, stderr } = await writing(token);

    assert.equal(stdout, "not_found");
    assert.equal(stderr, "");
  }

  assert.equal((await writing(Uint8Array.of(0))).stdout, "other(9)");
});

test("a stream used against its direction fails with EBADF, and a handle this host never minted is not found", async () => {
  const { stdout } = await execute(DIRECTION, {
    foreign: { "/token": () => new Uint8Array() },
  });

  assert.equal(stdout, "other(9) other(9) chunk eof not_found");
});

test("a poll reports ERR for a handle it does not know, and refuses interests that do not pair with the handles", async () => {
  const { stdout } = await execute(POLL, {
    foreign: { "/token": () => Uint8Array.of(9) },
  });

  assert.equal(stdout, "err write read");
  await stopped(UNEQUAL, {}, /handle_poll: 1 handles, but 2 interests/);
});

test("listing a directory is denied rather than refused", async () => {
  const { stdout } = await execute(`
use /std/{Try, Show, Path, print};
let r = Try/run(/std/fs/list(Path/of_str(".")))!;
match r | success(_) => print("listed") | failure(e) => print(Show/show(e)) end
`);

  assert.equal(stdout, "permission_denied");
});

test("random bytes are exactly as many as were asked for, past one Web Crypto call", async () => {
  const { stdout } = await execute(`
use /std/{Bytes, Nat, print};
let b = /std/rand/bytes(70000)!;
print(Nat/to_str(Bytes/len(b)))
`);

  assert.equal(stdout, "70000");
});

test("an exit ends the run with its code", async () => {
  for (const code of [0, 3, 255]) {
    const outcome = await execute(`
let _ = /std/print("bye")!;
/std/proc/exit(@{}, ${code})
`);

    assert.deepEqual(
      [outcome.stdout, outcome.exitCode, outcome.trap],
      ["bye", code, null],
    );
  }
});

test("each write reaches its stream and its hook in the order the program made it", async () => {
  const heard = [];

  const outcome = await execute(
    `
use /std/{print, print_err};
let _ = print("one ")!;
let _ = print_err("two ")!;
print("three")
`,
    {
      onStdout: (bytes) => heard.push(["out", text(bytes)]),
      onStderr: (bytes) => heard.push(["err", text(bytes)]),
    },
  );

  assert.equal(outcome.stdout, "one three");
  assert.equal(outcome.stderr, "two ");
  assert.deepEqual(heard, [
    ["out", "one "],
    ["err", "two "],
    ["out", "three"],
  ]);
});

test("a row the program calls needs a hook, a row it never calls needs none, and a hook needs a row", async () => {
  const used = { "/used": () => 1n };

  assert.equal((await execute(HOOKS, { foreign: used })).stdout, "ran");
  await stopped(
    HOOKS,
    { foreign: {} },
    /the program calls ffi \/used, and hooks\.foreign implements no \/used/,
  );
  await stopped(
    HOOKS,
    { foreign: { ...used, "/other": () => 1n } },
    /hooks\.foreign implements \/other, which the program declares no foreign row for/,
  );

  const { trap } = await run(
    { ...compile(HOOKS), foreigns: [] },
    { foreign: {} },
  );

  assert.match(
    trap,
    /the program imports ffi \/used, which its compiled rows do not describe/,
  );
});
