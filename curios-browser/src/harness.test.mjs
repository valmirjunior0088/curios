// The browser harness under Node's own test runner, against the bundle `CURIOS_JS` names, the directory `cargo xtask bundle build` filed: fixture programs compiled by the bundle's `compile` and run by its `run` against scripted hooks. It holds what only a JavaScript host can get wrong — a `foreign` hook held to its row, a handle's exact token, the harness's answers to the host rows — which no Rust test reaches, since the harness runs only under a JavaScript engine. `cargo xtask bundle test` builds the bundle and runs this.

import { test } from "node:test";
import assert from "node:assert/strict";
import { readFileSync } from "node:fs";
import { pathToFileURL } from "node:url";

const bundled = process.env.CURIOS_JS;

assert.ok(bundled, "CURIOS_JS names the bundle under test");

const bundle = pathToFileURL(`${bundled}/`);
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
let main() -> /std/Io({}) =
    let b = byte(7)!;
    print(Nat/to_str(Byte/to_nat(b)));
main()
`;

const FLAG = `
use /std/{print};
foreign flag() -> Bool;
let main() -> /std/Io({}) =
    let b = flag()!;
    print(match b | true => "yes" | false => "no" end);
main()
`;

const PAIR = `
use /std/{Nat, Str, print};
foreign pair(Nat, Bytes) -> {code: Nat, ok: Bool};
let main() -> /std/Io({}) =
    let r = pair(1, x[])!;
    print(Str/flatten([Nat/to_str(r.code), " ", match r.ok | true => "ok" | false => "not ok" end]));
main()
`;

const SPLIT = `
use /std/{Bytes, List, Nat, Str, print};
foreign split(Bytes) -> {head: Bytes, count: Nat, rest: List(Bytes)};
let main() -> /std/Io({}) =
    let r = split(x[1, 2, 3])!;
    print(Str/flatten([Nat/to_str(Bytes/to_nat(r.head)), " ", Nat/to_str(r.count), " ", Str/join(",", List/map(r.rest, (b) => Nat/to_str(Bytes/to_nat(b))))]));
main()
`;

const TAKE = `
foreign token() -> Handle;
foreign take(Nat, Int, Bool, Byte, Flt, Bytes, List(Nat), List(Int), List(Bool), List(Bytes), List(Handle)) -> {};
/std/Io/bind(token(), (h) => take(18446744073709551615, -5, true, 200, 1.5, x[1, 2], [1, 2], [-1, 3], [true, false], [x[7], x[]], [h]))
`;

const LISTS = `
use /std/{Nat, Str, List, print};
foreign lists() -> List(Nat);
let main() -> /std/Io({}) =
    let l = lists()!;
    print(Str/join(",", List/map(l, Nat/to_str)));
main()
`;

const OPTIONS = `
use /std/{Nat, Str, Option, print};
foreign maybe(Option(Nat), Option(Bytes)) -> Option(Nat);
let main() -> /std/Io({}) =
    let a = maybe(Option/some(5), Option/none())!;
    let b = maybe(Option/none(), Option/some(x[1, 2]))!;
    print(Str/flatten([Nat/to_str(Option/unwrap_or(a, 0)), " ", match b | some(_) => "some" | none() => "none" end]));
main()
`;

const KEEP = `
use /std/{Io, Process};
foreign keep(Bytes) -> Bytes;
foreign poke() -> {};
let main() -> /std/Try(Io, Io/Error, {}) =
    let b = x[0x61, 0x62, 0x63];
    let r = keep(b)!;
    let _ = poke()!;
    let _ = Io/write(Process/stdout, b)!;
    Io/write(Process/stdout, r);
main()
`;

const ABORT = `
foreign abort(Byte) -> Never;
abort(3)
`;

const WRITE = `
use /std/{Io, Show, Try, print};
foreign token() -> Handle;
let main() -> /std/Io({}) =
    let h = token()!;
    let r = Try/run(Io/write(h, x[0x68, 0x69]))!;
    match r
    | success(_) => print(" wrote")
    | failure(e) => print(Show/show(e))
    end;
main()
`;

const DIRECTION = `
use /std/{Io, Show, Str, Try, Result, print};
foreign input() -> Handle;
foreign output() -> Handle;
foreign token() -> Handle;
let main() -> /std/Io({}) =
    let said(r: Result(Io/Error, Io/Chunk)) -> Str =
        match r | success(bytes(_, @_)) => "chunk" | success(ended()) => "eof" | failure(e) => Show/show(e) end;
    let stdin = input()!;
    let stdout = output()!;
    let w = Try/run(Io/write(stdin, x[1]))!;
    let out = Try/run(Io/read(stdout, 4))!;
    let rest = Try/run(Io/read(stdin, 4))!;
    let h = token()!;
    let unknown = Try/run(Io/read(h, 4))!;
    print(Str/join(" ", [match w | success(_) => "wrote" | failure(f) => Show/show(f) end, said(out), said(rest), said(unknown)]));
main()
`;

const HOOKS = `
foreign used() -> Nat;
foreign _unused() -> Nat;
let main() -> /std/Io({}) =
    let _ = used()!;
    /std/print("ran");
main()
`;

test("entry exports select the direct or turn protocol for the program they run", async () => {
  for (const [source, turns, status, output] of [
    ["/std/Io/pure(())", false, 0, ""],
    ['/std/print("hello")', true, 0, "hello"],
    ["/std/Io/wait_filled([])", true, 1, ""],
  ]) {
    const compiled = compile(source);
    const module = await WebAssembly.compile(compiled.program);
    const exports = new Set(
      WebAssembly.Module.exports(module).map(({ name }) => name),
    );
    assert.ok(exports.has("func/main"), source);
    assert.equal(exports.has("func/advance"), turns, source);
    const outcome = await run(compiled);
    assert.equal(outcome.exitCode, status, source);
    assert.equal(outcome.trap, null, source);
    assert.equal(text(outcome.stdout), output, source);
    if (status === 1) {
      assert.match(text(outcome.stderr), /^deadlock: /);
    } else {
      assert.equal(text(outcome.stderr), "", source);
    }
  }
});

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
  assert.ok(
    foreigns.some(({ name }) => name === "/std/internal/Handle/handle_write"),
  );
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
  assert.deepEqual(handed.slice(0, -1), [1n, new Uint8Array()]);
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

test("a hook is handed each operand as the JavaScript value its type names, then the signal of its cancel", async () => {
  let handed;

  const { trap } = await execute(TAKE, {
    foreign: {
      "/token": () => Uint8Array.of(1),
      "/take": (...operands) => {
        handed = operands;
      },
    },
  });

  assert.equal(trap, null);
  assert.ok(handed.at(-1) instanceof AbortSignal);
  assert.deepEqual(handed.slice(0, -1), [
    2n ** 64n - 1n,
    -5n,
    true,
    200,
    1.5,
    Uint8Array.of(1, 2),
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

test("an option is handed as its value or null, and answered the same way", async () => {
  const handed = [];

  const { stdout } = await execute(OPTIONS, {
    foreign: {
      "/maybe": (n, bytes) => {
        handed.push([n, bytes]);

        return n === null ? null : n + 1n;
      },
    },
  });

  assert.equal(stdout, "6 none");
  assert.deepEqual(handed, [
    [5n, null],
    [null, Uint8Array.of(1, 2)],
  ]);
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
    foreign: {
      "/input": () => Uint8Array.of(0),
      "/output": () => Uint8Array.of(1),
      "/token": () => new Uint8Array(),
    },
  });

  assert.equal(stdout, "other(9) other(9) eof not_found");
});

test("listing a directory is denied rather than refused", async () => {
  const { stdout } = await execute(`
use /std/{Try, Show, Path, print};
let main() -> /std/Io({}) =
    let r = Try/run(/std/fs/list(Path/of_str(".")))!;
    match r | success(_) => print("listed") | failure(e) => print(Show/show(e)) end;
main()
`);

  assert.equal(stdout, "permission_denied");
});

test("a program that reads its arguments is refused by the row, which has none to hand it", async () => {
  await stopped(
    `
use /std/{List, Nat, print};
let main() -> /std/Io({}) =
    let args = /std/Process/args()!;
    print(Nat/to_str(List/len(args)));
main()
`,
    {},
    /proc_args: the playground runs a program with no arguments/,
  );
});

test("random bytes are exactly as many as were asked for, past one Web Crypto call", async () => {
  const { stdout } = await execute(`
use /std/{Bytes, Nat, print};
let main() -> /std/Io({}) =
    let b = /std/rand/bytes(70000)!;
    print(Nat/to_str(Bytes/len(b)));
main()
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
use /std/{Int, print};
use /std/time/{Instant, Duration};
let main() -> /std/Io({}) =
    let a = Instant/now()!;
    let b = Instant/now()!;
    print(Int/to_str(Duration/to_nanos(Instant/diff(b, a))));
main()
`);

  assert.equal(stdout, "+1999999999");
});

test("an exit ends the run with its code", async () => {
  for (const code of [0, 3, 255]) {
    const outcome = await execute(`
let main() -> /std/Io({}) =
    let _ = /std/print("bye")!;
    /std/Process/exit(@{}, ${code});
main()
`);

    assert.deepEqual(
      [outcome.stdout, outcome.exitCode, outcome.trap],
      ["bye", code, null],
    );
  }
});

test("a program that finishes its standard output writes nothing more there, and a print after it ends the program saying why", async () => {
  const { stdout, stderr, exitCode } = await execute(`
use /std/{Try, Io, Process, print};
let main() -> /std/Io({}) =
    let _ = print("before")!;
    let _ = Try/run(Io/finish(Process/stdout))!;
    print("after");
main()
`);

  assert.deepEqual(
    [stdout, stderr, exitCode],
    ["before", "standard output could not be written: not_found\n", 1],
  );
});

test("a program suspended on a timer wakes into its rest, and the event loop runs while it sleeps", async () => {
  // Scheduled once the program has printed, so only a sleep that yields to the event loop lets it run before the program ends.
  let ticked = false;
  const heard = [];
  const onStdout = (bytes) => {
    heard.push(text(bytes));
    setTimeout(() => {
      ticked = true;
    }, 0);
  };

  const { exitCode } = await execute(
    `
use /std/{Io, Nat, Str, print};
use /std/time/{Instant, Duration};
let main() -> /std/Io({}) =
    let _ = print("slept ")!;
    let start = Instant/now()!;
    let woke = Io/bind(Io/sleep(Duration/of_millis(20)), (_) => Io/pure(7))!;
    let slept = Duration/to_millis(Instant/diff(Instant/now()!, start));
    print(Str/concat(Nat/to_str(woke), match slept >= 20 | true => "" | false => " early" end));
main()
`,
    { onStdout },
  );

  assert.deepEqual([exitCode, heard.join(""), ticked], [0, "slept 7", true]);
});

test("a program suspended on no handle runs its cancel, reports the deadlock and ends with code 1", async () => {
  const { exitCode, stdout, stderr } = await execute(`
use /std/{Io, print};
let main() -> /std/Io({}) =
    let _ = Io/Scope/run((scope) => Io/bind(Io/Scope/defer(scope, (_) => print("abandoned")), (_) => Io/bind(Io/wait_filled([]), (_) => print("resumed"))))!;
    print("after");
main()
`);

  assert.deepEqual([exitCode, stdout], [1, "abandoned"]);
  assert.match(stderr, /^deadlock: /);
});

test("a hook's promise answers its row later, and the rest of the program runs while it is pending", async () => {
  const { exitCode, stdout } = await execute(
    `
foreign fetch(Nat) -> Nat;
use /std/{Io, Nat, print};
use /std/time/{Duration};
let main() -> /std/Io({}) =
    let tick = Io/bind(Io/sleep(Duration/of_millis(5)), (_) => print("tick "));
    let (n, _) = Io/both(fetch(3), tick)!;
    print(Nat/to_str(n));
main()
`,
    {
      foreign: {
        "/fetch": (n) =>
          new Promise((resolve) => {
            setTimeout(() => resolve(n * 2n), 40);
          }),
      },
    },
  );

  assert.deepEqual([exitCode, stdout], [0, "tick 6"]);
});

test("a call cancelled while its hook's promise is pending aborts the hook's signal", async () => {
  let aborted = false;

  const { exitCode, stdout } = await execute(
    `
foreign hang(Nat) -> Nat;
use /std/{Io, print};
use /std/time/{Duration};
let main() -> /std/Io({}) =
    let came = Io/timeout(Duration/of_millis(10), hang(1))!;
    print(match came | some(_) => "came" | none() => "timed out" end);
main()
`,
    {
      foreign: {
        "/hang": (_, signal) =>
          new Promise(() => {
            signal.addEventListener("abort", () => {
              aborted = true;
            });
          }),
      },
    },
  );

  assert.deepEqual([exitCode, stdout, aborted], [0, "timed out", true]);
});

const INTERLEAVED = `
use /std/{print, print_err};
let main() -> /std/Io({}) =
    let _ = print("one ")!;
    let _ = print_err("two ")!;
    print("three");
main()
`;

test("each write reaches its stream's hook in the order the program made it", async () => {
  const heard = [];

  await execute(INTERLEAVED, {
    onStdout: (bytes) => heard.push(["out", text(bytes)]),
    onStderr: (bytes) => heard.push(["err", text(bytes)]),
  });

  assert.deepEqual(heard, [
    ["out", "one "],
    ["err", "two "],
    ["out", "three"],
  ]);
});

test("a stream a hook takes is the hook's, and the run keeps only the streams no hook takes", async () => {
  const heard = [];

  const outcome = await execute(INTERLEAVED, {
    onStdout: (bytes) => heard.push(text(bytes)),
  });

  assert.deepEqual(heard, ["one ", "three"]);
  assert.equal(outcome.stdout, "");
  assert.equal(outcome.stderr, "two ");
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

  // The rows are what the import object is built from, so a module importing a row they do not describe is refused by the engine where it is instantiated, naming the import.
  const compiled = compile(HOOKS);
  const { trap } = await run(
    {
      ...compiled,
      foreigns: compiled.foreigns.filter(({ name }) => name !== "/used"),
    },
    { foreign: {} },
  );

  assert.match(trap, /\/used/);
});

test("a deadlock cancellation that suspends finishes its cleanup before reporting", async () => {
  let ticked = false;
  const heard = [];
  const outcome = await execute(
    `
use /std/{Io, print};
use /std/time/{Duration};
let release(_: Io/Ended) -> Io({}) =
    let _ = print("closing ")!;
    let _ = Io/sleep(Duration/of_millis(10))!;
    print("closed");
let main() -> Io({}) =
    let _ = Io/Scope/run((scope) => Io/bind(Io/Scope/defer(scope, release), (_) => Io/bind(Io/wait_filled([]), (_) => print("resumed"))))!;
    print("after");
main()
`,
    {
      onStdout: (bytes) => {
        heard.push(text(bytes));

        if (text(bytes) === "closing ") {
          setTimeout(() => {
            ticked = true;
          }, 0);
        }
      },
    },
  );
  assert.equal(outcome.exitCode, 1);
  assert.equal(outcome.trap, null);
  assert.equal(heard.join(""), "closing closed");
  assert.match(outcome.stderr, /^deadlock: /);
  assert.equal(ticked, true);
});
