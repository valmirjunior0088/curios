// The browser harness under Node's own test runner, against the bundle `cargo xtask js` filed: fixture programs compiled by the bundle's `compile` and run by its `run` against scripted hooks. It holds what only a JavaScript host can get wrong — a `foreign` hook held to its row, a handle's exact token, the harness's answers to the host rows — which no Rust test reaches, since the harness runs only under a JavaScript engine. `cargo xtask js-test` builds the bundle and runs this.

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
foreign byte(Byte) -> Byte;
let b = byte(7)!;
print(Nat/to_str(Byte/to_nat(b)))
`;

const FLAG = `
use /std/{print};
foreign flag() -> Bool;
let b = flag()!;
print(match b | true => "yes" | false => "no" end)
`;

const PAIR = `
use /std/{Nat, Str, print};
foreign pair(Nat, Bytes) -> {code: Nat, ok: Bool};
let r = pair(1, x[])!;
print(Str/flatten([Nat/to_str(r.code), " ", match r.ok | true => "ok" | false => "not ok" end]))
`;

const SPLIT = `
use /std/{Bytes, List, Nat, Str, print};
foreign split(Bytes) -> {head: Bytes, count: Nat, rest: List(Bytes)};
let r = split(x[1, 2, 3])!;
print(Str/flatten([Nat/to_str(Bytes/to_nat(r.head)), " ", Nat/to_str(r.count), " ", Str/join(",", List/map(r.rest, (b) => Nat/to_str(Bytes/to_nat(b))))]))
`;

const TAKE = `
foreign take(Nat, Int, Bool, Byte, Flt, Bytes, Bits, List(Nat), List(Int), List(Bool), List(Bytes), List(Handle)) -> {};
take(18446744073709551615, -5, true, 200, 1.5, x[1, 2], b[1, 0, 1], [1, 2], [-1, 3], [true, false], [x[7], x[]], [/std/Handle/stdout])
`;

const LISTS = `
use /std/{Nat, Str, List, print};
foreign lists() -> List(Nat);
let l = lists()!;
print(Str/join(",", List/map(l, Nat/to_str)))
`;

const KEEP = `
use /std/{Io};
foreign keep(Bytes) -> Bytes;
foreign poke() -> {};
let b = x[0x61, 0x62, 0x63];
let r = keep(b)!;
let _ = poke()!;
let _ = Io/write(Io/stdout, b)!;
Io/write(Io/stdout, r)
`;

const ABORT = `
foreign abort(Byte) -> Never;
abort(3)
`;

const WRITE = `
use /std/{Async, Handle, Show, print};
foreign token() -> Handle;
let h = token()!;
let r = Async/Write/write(h, x[0x68, 0x69])!;
match r
| success(_) => print(" wrote")
| failure(e) => print(Show/show(e))
end
`;

const DIRECTION = `
use /std/{Async, Handle, Io, Show, Str, print};
foreign token() -> Handle;
let said(c: Io/Chunk) -> Str =
    match c | chunk(_, @_) => "chunk" | eof() => "eof" | error(e) => Show/show(e) end;
let w = Async/Write/write(Handle/stdin, x[1])!;
let out = Async/Read/read(Handle/stdout, 4)!;
let rest = Async/Read/read(Handle/stdin, 4)!;
let h = token()!;
let unknown = Async/Read/read(h, 4)!;
print(Str/join(" ", [match w | success(_) => "wrote" | failure(f) => Show/show(f) end, said(out), said(rest), said(unknown)]))
`;

const POLL = `
use /std/{Handle, List, Str, print};
use /std/Handle/{Interest, Readiness};
foreign token() -> Handle;
let said(r: Readiness) -> Str =
    choose
    | r.failed => "err"
    | r.fired.writable => "write"
    | r.fired.readable => "read"
    | _ => "none"
    end;
let h = token()!;
let rs = Handle/poll([(h, Interest/readable), (Handle/stdout, Interest/writable), (Handle/stdin, Interest/readable)], 0)!;
print(Str/join(" ", List/map(rs, said)))
`;

const HOOKS = `
foreign used() -> Nat;
foreign _unused() -> Nat;
let _ = used()!;
/std/print("ran")
`;

test("compile hands back the program beside the foreign rows it reaches as plain data", () => {
  const { program, foreigns } = compile(PAIR);

  assert.ok(program instanceof Uint8Array);
  // The program's own row, and the standard library's a print reaches, each by the name it is declared under.
  assert.deepEqual(
    foreigns.find(({ name }) => name === "/pair"),
    {
      name: "/pair",
      params: ["Nat", "Bytes"],
      results: [
        { label: "code", type: "Nat" },
        { label: "ok", type: "Bool" },
      ],
    },
  );
  assert.ok(foreigns.some(({ name }) => name === "/std/Handle/handle_write"));
});

test("a row that does not return is described as one, and a hook that returns from it stops the program", async () => {
  assert.deepEqual(
    compile(ABORT).foreigns.find(({ name }) => name === "/abort"),
    { name: "/abort", params: ["Byte"], never: true },
  );

  let handed;
  await stopped(
    ABORT,
    {
      foreign: {
        "/abort": (code) => {
          handed = code;
        },
      },
    },
    /ffi \/abort: the hook returned, and the row does not return/,
  );
  assert.equal(handed, 3);

  // A hook that keeps the row's word ends the program by throwing, and what it threw is the trap.
  await stopped(
    ABORT,
    {
      foreign: {
        "/abort": () => {
          throw new Error("aborted by the hook");
        },
      },
    },
    /aborted by the hook/,
  );
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

test("a row's references may stand in any slot, and several may, each read back where it stands", async () => {
  const { stdout } = await execute(SPLIT, {
    foreign: {
      "/split": (bytes) => ({
        head: bytes.slice(0, 1),
        count: 7n,
        rest: [bytes.slice(1, 2), bytes.slice(2)],
      }),
    },
  });

  assert.equal(stdout, "1 7 2,3");
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

  assert.equal(stdout, "other(9) other(9) eof not_found");
});

test("a poll reports a failure for a handle it does not know, beside the streams ready for what each was waited on", async () => {
  const { stdout } = await execute(POLL, {
    foreign: { "/token": () => Uint8Array.of(9) },
  });

  assert.equal(stdout, "err write read");
});

test("listing a directory is denied rather than refused", async () => {
  const { stdout } = await execute(`
use /std/{Try, Show, Path, print};
let r = Try/run(/std/fs/list(Path/of_str(".")))!;
match r | success(_) => print("listed") | failure(e) => print(Show/show(e)) end
`);

  assert.equal(stdout, "permission_denied");
});

test("a program that reads its arguments is refused by the row, which has none to hand it", async () => {
  await stopped(
    `
use /std/{List, Nat, print};
let args = /std/proc/args()!;
print(Nat/to_str(List/len(args)))
`,
    {},
    /proc_args: the playground runs a program with no arguments/,
  );
});

test("random bytes are exactly as many as were asked for, past one Web Crypto call", async () => {
  const { stdout } = await execute(`
use /std/{Bytes, Nat, print};
let b = /std/rand/bytes(70000)!;
print(Nat/to_str(Bytes/len(b)))
`);

  assert.equal(stdout, "70000");
});

test("the monotonic clock answers the nanoseconds within a second, a reading just under one included", async (t) => {
  // The engine's clock read twice: at its origin, then a tenth of a nanosecond under two seconds, which rounded would be a billion nanoseconds and the next second's.
  const readings = [0, 1999.9999999];

  t.mock.method(globalThis.performance, "now", () =>
    readings.length > 1 ? readings.shift() : readings[0],
  );

  const { stdout } = await execute(`
use /std/{Nat, Str, print};
use /std/time/{Instant, Duration};
let a = Instant/now()!;
let b = Instant/now()!;
let d = Instant/diff(b, a);
print(Str/flatten([Nat/to_str(Duration/secs(d)), " ", Nat/to_str(Duration/nanos(d))]))
`);

  assert.equal(stdout, "1 999999999");
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
