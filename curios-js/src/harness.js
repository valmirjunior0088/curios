// The browser run harness: instantiates a compiled curios program against a JS implementation of the host boundary and drives its entrypoint. Like any embedder, this file spells the wire names itself — the `sys.*` import keys, the `sys`/`ffi` namespaces, the `func/main` export; the contract is pinned by the Rust test suite and by the Node suite under `tests/`. The codes and the standard streams' tokens arrive via `config`, built Rust-side from curios-abi (see src/abi.rs), and the program's own `foreign` rows arrive with the program, as `compile` described them (see src/foreigns.rs).
//
// A `Nat` or `Int` crosses as an `i64`, which JavaScript sees as a `BigInt` in both directions: a count arrives as one, and every status, size and time handed back must be one — the status codes in `config` already are. A `Bool` and a `Byte`, the exit code among them, cross as plain numbers.
//
// A handle crosses as its token's bytes, and this harness keys a token on the hex of exactly those bytes, never on a number decoded from them: the empty token is not stdin's `00`, a padded `0100` is not stdout's `01`, and a token longer than any number stays itself.
//
// The browser host is deliberately shallow: stdout/stderr accumulate (and stream via hooks), stdin is at EOF, the clocks and randomness are real, and everything filesystem/network answers PERMISSION_DENIED. A handle this host never minted names nothing, and answers NOT_FOUND as a closed one does on every host.

/** Thrown by the `proc_exit` import to unwind the wasm stack with an exit code. */
export class ExitSignal extends Error {
  constructor(code) {
    super(`exit(${code})`);
    this.code = code;
  }
}

/** Thrown by the `panic` import — the compiler's own refusal, an overflow or a read past the end — to unwind the wasm stack with the sentence the program handed over. Surfaces as the run's `trap`, prefixed `panicked:` exactly as the native runtime prints it. */
export class PanicSignal extends Error {
  constructor(message) {
    super(`panicked: ${message}`);
  }
}

/** The largest `Nat` and the `Int` range a 64-bit wire integer holds. */
const NAT_MAX = 2n ** 64n - 1n;
const INT_MIN = -(2n ** 63n);
const INT_MAX = 2n ** 63n - 1n;

/** A value as a refusal names it. */
const describe = (value) => {
  if (typeof value === "bigint") {
    return `${value}n`;
  }

  if (value instanceof Uint8Array) {
    return `a Uint8Array of ${value.length}`;
  }

  if (Array.isArray(value)) {
    return `an array of ${value.length}`;
  }

  if (value === null || value === undefined) {
    return String(value);
  }

  return typeof value === "object"
    ? "an object"
    : `${typeof value} ${String(value)}`;
};

/**
 * Run a compiled program. `config` carries:
 * - `compiled`: what `compile` returned — `program`, the module bytes, and `foreigns`, the program's own `foreign` rows as `{ name, params, results }`;
 * - `bridge`: the bridge module's bytes, from `bridge_bytes`;
 * - `status`, `event`, `stdio`: the wire code tables and the standard streams' tokens from `abi`;
 * - `hooks`: optional `{ onStdout?, onStderr?, foreign? }` — `onStdout`/`onStderr` are streaming callbacks, each receiving a copy of every write as a `Uint8Array`; `foreign` implements the program's own `foreign` declarations, keyed by fully qualified name (e.g. `{ "/frobnicate": fn }`), each held to its row as `checked` states.
 *
 * Resolves to `{ stdout, stderr, exitCode, trap }`: the accumulated output bytes, the code the program exited with (0 when `main` returns), and the trap message if execution failed instead.
 */
export async function run(config) {
  const bridge = (await WebAssembly.instantiate(config.bridge, {})).instance
    .exports;

  // Byte strings cross the boundary through the bridge's memory in one `bytes_load`/`bytes_store` call per string; the memory starts empty and grows here, JS-side, to fit the largest string seen so far.
  const memory = bridge.memory;

  const ensureCapacity = (length) => {
    const missing = length - memory.buffer.byteLength;

    if (missing > 0) {
      memory.grow(Math.ceil(missing / 65536));
    }
  };

  const decodeBytes = (ref) => {
    const length = bridge.bytes_len(ref);
    ensureCapacity(length);
    bridge.bytes_load(ref);

    return new Uint8Array(memory.buffer, 0, length).slice();
  };

  const encodeBytes = (bytes) => {
    ensureCapacity(bytes.length);
    new Uint8Array(memory.buffer).set(bytes);

    return bridge.bytes_store(bytes.length);
  };

  const emptyBytes = () => encodeBytes(new Uint8Array(0));

  const hex = (bytes) =>
    Array.from(bytes, (byte) => byte.toString(16).padStart(2, "0")).join("");

  // A handle's key: the hex of its token's exact bytes.
  const keyOf = (handle) => hex(decodeBytes(handle));

  const { status, event } = config;
  const { STDIN, STDOUT, STDERR } = config.stdio;
  const standard = new Set([STDIN, STDOUT, STDERR]);

  const output = { stdout: [], stderr: [] };

  const hooks = config.hooks ?? {};

  // The output streams take every write whole, and hand each hook a copy of it, so a hook that keeps and changes what it was given changes nothing the run reports. Standard input is not open for writing, as on every host.
  const write = (handle, ref) => {
    const bytes = decodeBytes(ref);

    switch (keyOf(handle)) {
      case STDOUT:
        output.stdout.push(bytes);
        hooks.onStdout?.(bytes.slice());
        break;
      case STDERR:
        output.stderr.push(bytes);
        hooks.onStderr?.(bytes.slice());
        break;
      case STDIN:
        return [status.EBADF, 0n];
      default:
        return [status.NOT_FOUND, 0n];
    }

    return [status.OK, BigInt(bytes.length)];
  };

  const deniedHandle = () => [status.PERMISSION_DENIED, emptyBytes()];

  const deniedList = () => [status.PERMISSION_DENIED, bridge.list_new(0)];

  const denied = () => status.PERMISSION_DENIED;

  // The standard streams take `SO_REUSEADDR` like a file — recording nothing and answering OK, as OsHost and MockHost do; no other handle exists in the browser, so anything else names nothing and is not found.
  const reuseaddr = (handle) =>
    standard.has(keyOf(handle)) ? status.OK : status.NOT_FOUND;

  const unsupported = (name) => () => {
    throw new Error(`${name} is not supported in the browser playground`);
  };

  // The `sys` import object, keyed by wire name. A `host_ops` row without a browser implementation surfaces as a `LinkError` naming the import when a program calls it.
  const sysEnv = {
    // Standard input is at its end, so a read of it answers `EOF` — or, asked for nothing, succeeds with nothing, since a request for nothing only asks whether the handle could be read. The output streams are not open for reading.
    handle_read: (handle, count) => {
      switch (keyOf(handle)) {
        case STDIN:
          return [count === 0n ? status.OK : status.EOF, emptyBytes()];
        case STDOUT:
        case STDERR:
          return [status.EBADF, emptyBytes()];
        default:
          return [status.NOT_FOUND, emptyBytes()];
      }
    },
    handle_write: write,
    file_open: deniedHandle,
    dns_lookup: deniedHandle,
    dns_resolve: deniedList,
    socket_open: deniedHandle,
    socket_bind: denied,
    socket_connect: denied,
    socket_finish_connect: denied,
    socket_listen: denied,
    socket_accept: deniedHandle,
    tls_start: denied,
    tls_server_config: deniedHandle,
    tls_start_server: denied,
    socket_set_reuseaddr: reuseaddr,
    // Readiness in the playground: the standard streams are always ready — stdin is at its end, which a read answers at once, and the output streams accept everything — and any other handle names nothing, so it reports `ERR`, which wakes whatever waits on it to find `NOT_FOUND`. The timeout is ignored, since nothing here can become ready later, and the browser cannot sleep inside a synchronous import anyway. A call whose interests do not pair one with each handle, or that asks for readiness no guest can ask for, is malformed and refused.
    // The masks cross as one byte per handle, in the handles' order.
    handle_poll: (handles, events, _timeout) => {
      const count = bridge.list_len(handles);
      const interest = decodeBytes(events);

      if (interest.length !== count) {
        throw new Error(
          `handle_poll: ${count} handles, but ${interest.length} interests`,
        );
      }

      const ready = new Uint8Array(count);

      for (let i = 0; i < count; i += 1) {
        if ((interest[i] & ~event.INTEREST) !== 0) {
          throw new Error(
            `handle_poll: ${interest[i]} asks for readiness a guest cannot`,
          );
        }

        ready[i] = standard.has(keyOf(bridge.list_get(handles, i)))
          ? interest[i]
          : event.ERR;
      }

      return encodeBytes(ready);
    },
    handle_close: () => {},
    // Nothing is held back in the playground — every write is delivered as it is made — so a standard stream's flush has nothing to drain, and no other handle exists here.
    handle_flush: (handle) =>
      standard.has(keyOf(handle)) ? status.OK : status.NOT_FOUND,
    clock_wall: () => {
      const millis = Date.now();

      return [BigInt(Math.floor(millis / 1000)), BigInt((millis % 1000) * 1_000_000)];
    },
    clock_mono: () => {
      const millis = performance.now();

      // Floor, not round: a fractional millisecond just below 1000 would otherwise round the nanos limb up to exactly 10⁹, which the seconds limb owns.
      return [
        BigInt(Math.floor(millis / 1000)),
        BigInt(Math.floor((millis % 1000) * 1_000_000)),
      ];
    },
    // Exactly `count` bytes, or a refusal where this host cannot hold that many — never fewer.
    rand_bytes: (count) => {
      const length = BigInt.asUintN(64, count);
      let bytes;

      try {
        bytes = new Uint8Array(Number(length));
      } catch {
        throw new Error(`rand_bytes: this host cannot hold ${length} bytes`);
      }

      // Web Crypto caps one `getRandomValues` at 65536 bytes (a `QuotaExceededError` past it), so a larger request is filled a slice at a time; the native host has no such ceiling, and `rand/bytes` promises none.
      for (let offset = 0; offset < bytes.length; offset += 65536) {
        crypto.getRandomValues(bytes.subarray(offset, offset + 65536));
      }

      return encodeBytes(bytes);
    },
    // `proc/args` has no failure lane, and the playground has no arguments it could truthfully call a program's, so it refuses the call rather than answer an empty list a program would read as a fact.
    proc_args: unsupported("proc_args"),
    proc_env: () => [status.NOT_FOUND, emptyBytes()],
    // The playground has no terminal to switch or measure, so both tty rows are denied as `file_open` is.
    tty_raw: denied,
    tty_size: () => [status.PERMISSION_DENIED, 0n, 0n],
    // No serial devices either: Web Serial asks the user to pick a port, which no row can do, so opening one is denied as `file_open` is.
    serial_open: deniedHandle,
    serial_control: denied,
    // No filesystem either: every filesystem row is denied as `file_open` is, listing a directory among them.
    file_stat: () => [status.PERMISSION_DENIED, 0n, 0n, 0n, 0n],
    file_remove: denied,
    file_rename: denied,
    dir_list: deniedList,
    dir_create: denied,
    dir_remove: denied,
    proc_cwd: deniedHandle,
    // WASI has no process creation and neither does the playground.
    proc_spawn: deniedHandle,
    proc_stream: deniedHandle,
    proc_wait: () => [status.PERMISSION_DENIED, 0n, 0n],
    proc_kill: denied,
    // The row diverges, so this never returns: the signal unwinds the guest, and a code outside a byte is a module this compiler did not emit.
    proc_exit: (code) => {
      if (!Number.isInteger(code) || code < 0 || code > 255) {
        throw new Error(`proc_exit: ${code} is not a byte`);
      }

      throw new ExitSignal(code);
    },
    panic: (message) => {
      throw new PanicSignal(new TextDecoder().decode(decodeBytes(message)));
    },
  };

  const lengthOf = {
    longs: bridge.longs_len,
    words: bridge.words_len,
    list: bridge.list_len,
  };

  const elements = (kind, ref, get) =>
    Array.from({ length: lengthOf[kind](ref) }, (_, i) => get(ref, i));

  // A `Bool` the program sent: its word is `0` or `1`, and anything else is a module this compiler did not emit.
  const truth = (word, fail) =>
    word === 0 || word === 1 ? word === 1 : fail(`a Bool arrived as ${word}`);

  // What a hook is handed for each type a row spells: a `BigInt` for a `Nat` or an `Int`, a boolean for a `Bool`, a number for a `Byte` or an `Flt`, a `Uint8Array` for a `Bytes`, `Bits` or `Handle`, and an array of those for a `List` — each a copy the hook owns.
  const lifts = {
    Nat: (value) => BigInt.asUintN(64, value),
    Int: (value) => value,
    Bool: truth,
    Byte: (value) => value,
    Flt: (value) => value,
    Bytes: decodeBytes,
    Bits: decodeBytes,
    Handle: decodeBytes,
    "List(Nat)": (ref) =>
      elements("longs", ref, bridge.longs_get).map((value) =>
        BigInt.asUintN(64, value),
      ),
    "List(Int)": (ref) => elements("longs", ref, bridge.longs_get),
    "List(Bool)": (ref, fail) =>
      elements("words", ref, bridge.words_get).map((word) => truth(word, fail)),
    "List(Bytes)": (ref) =>
      elements("list", ref, bridge.list_get).map(decodeBytes),
    "List(Bits)": (ref) =>
      elements("list", ref, bridge.list_get).map(decodeBytes),
    "List(Handle)": (ref) =>
      elements("list", ref, bridge.list_get).map(decodeBytes),
  };

  // What a hook must answer for each type a row spells, the same JavaScript type it is handed, and what crosses back for it: the value checked strictly — a value of another JavaScript type is refused rather than converted, since a conversion would hide the disagreement — and every buffer copied in.
  const scalar = {
    Nat: [
      (value) => typeof value === "bigint" && value >= 0n && value <= NAT_MAX,
      (value) => BigInt.asIntN(64, value),
      "a BigInt from 0n to 2⁶⁴ - 1",
    ],
    Int: [
      (value) =>
        typeof value === "bigint" && value >= INT_MIN && value <= INT_MAX,
      (value) => value,
      "a BigInt from -2⁶³ to 2⁶³ - 1",
    ],
    Bool: [
      (value) => typeof value === "boolean",
      (value) => (value ? 1 : 0),
      "a boolean",
    ],
    Byte: [
      (value) => Number.isInteger(value) && value >= 0 && value <= 255,
      (value) => value,
      "an integer from 0 to 255",
    ],
    Flt: [(value) => typeof value === "number", (value) => value, "a number"],
    Bytes: [(value) => value instanceof Uint8Array, encodeBytes, "a Uint8Array"],
    Bits: [(value) => value instanceof Uint8Array, encodeBytes, "a Uint8Array"],
    Handle: [
      (value) => value instanceof Uint8Array,
      encodeBytes,
      "a Uint8Array",
    ],
  };

  const lists = {
    Nat: [bridge.longs_new, bridge.longs_set],
    Int: [bridge.longs_new, bridge.longs_set],
    Bool: [bridge.words_new, bridge.words_set],
    Bytes: [bridge.list_new, bridge.list_set],
    Bits: [bridge.list_new, bridge.list_set],
    Handle: [bridge.list_new, bridge.list_set],
  };

  const lower = (type, value, what, fail) => {
    const element = /^List\((\w+)\)$/.exec(type)?.[1];

    if (element === undefined) {
      const [holds, crossed, expected] = scalar[type];

      return holds(value)
        ? crossed(value)
        : fail(`${what} is ${describe(value)}, not ${expected}`);
    }

    if (!Array.isArray(value)) {
      return fail(`${what} is ${describe(value)}, not an array`);
    }

    const [make, put] = lists[element];
    const list = make(value.length);

    value.forEach((item, i) =>
      put(list, i, lower(element, item, `${what}[${i}]`, fail)),
    );

    return list;
  };

  // A program's own `foreign` row, implemented by `hook`: the operands decoded into copies the hook owns, and what it answers held to the row's results and copied in — nothing for a row with none, the one value for a row with one, and an object keyed by exactly the results' labels for a row with several. A violation stops the program, naming the row.
  const checked = (row, hook) => (...operands) => {
    const fail = (what) => {
      throw new Error(`ffi ${row.name}: ${what}`);
    };

    const answer = hook(
      ...row.params.map((type, i) => lifts[type](operands[i], fail)),
    );

    switch (row.results.length) {
      case 0:
        return answer === undefined
          ? undefined
          : fail(
              `the hook answered ${describe(answer)}, and the row answers nothing`,
            );
      case 1:
        return lower(row.results[0].type, answer, "the result", fail);
      default: {
        const labels = row.results.map(({ label }) => label);

        if (
          typeof answer !== "object" ||
          answer === null ||
          Array.isArray(answer) ||
          answer instanceof Uint8Array
        ) {
          return fail(
            `the hook answered ${describe(answer)}, not an object of ${labels.join(", ")}`,
          );
        }

        for (const label of Object.keys(answer)) {
          if (!labels.includes(label)) {
            fail(`the hook answered ${label}, which the row has no result of`);
          }
        }

        return row.results.map(({ label, type }) =>
          label in answer
            ? lower(type, answer[label], label, fail)
            : fail(`the hook answered no ${label}`),
        );
      }
    }
  };

  // The `ffi` import object, built from the module's own imports: each one the program calls must be a row `compile` described, and a hook must implement it. A declaration the program never calls is never imported, so it needs no hook; a hook naming no row is a hook for a program other than this one.
  const ffiEnv = (module) => {
    const rows = new Map(
      config.compiled.foreigns.map((row) => [row.name, row]),
    );
    const implemented = hooks.foreign ?? {};

    for (const name of Object.keys(implemented)) {
      if (!rows.has(name)) {
        throw new Error(
          `hooks.foreign implements ${name}, which the program declares no foreign row for`,
        );
      }
    }

    const env = {};

    for (const { module: namespace, name } of WebAssembly.Module.imports(
      module,
    )) {
      if (namespace !== "ffi") {
        continue;
      }

      const row = rows.get(name);

      if (row === undefined) {
        throw new Error(
          `the program imports ffi ${name}, which its compiled rows do not describe`,
        );
      }

      if (typeof implemented[name] !== "function") {
        throw new Error(
          `the program calls ffi ${name}, and hooks.foreign implements no ${name}`,
        );
      }

      env[name] = checked(row, implemented[name]);
    }

    return env;
  };

  const concat = (chunks) => {
    const bytes = new Uint8Array(
      chunks.reduce((length, chunk) => length + chunk.length, 0),
    );

    let offset = 0;

    for (const chunk of chunks) {
      bytes.set(chunk, offset);
      offset += chunk.length;
    }

    return bytes;
  };

  const result = () => ({
    stdout: concat(output.stdout),
    stderr: concat(output.stderr),
    exitCode: 0,
    trap: null,
  });

  try {
    const module = await WebAssembly.compile(config.compiled.program);
    const instance = await WebAssembly.instantiate(module, {
      sys: sysEnv,
      ffi: ffiEnv(module),
    });

    instance.exports["func/main"]();

    return result();
  } catch (error) {
    if (error instanceof ExitSignal) {
      return { ...result(), exitCode: error.code };
    }

    // A panic is a trap that says why: its message is the whole report, where an engine trap is rendered as the engine spells it.
    if (error instanceof PanicSignal) {
      return { ...result(), exitCode: null, trap: error.message };
    }

    return { ...result(), exitCode: null, trap: String(error) };
  }
}
