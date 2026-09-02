// The browser run harness: instantiates a compiled curios program against a JS implementation of the host boundary and drives its entrypoint. Like any embedder, this file spells the wire names itself — the `sys.*` import keys, the `sys`/`ffi` namespaces, the `func/main` export; the contract is pinned by the Rust test suite. Only the numeric status/stdio codes arrive via `config`, built Rust-side from curios-abi (see src/abi.rs).
//
// The browser host is deliberately shallow: stdout/stderr accumulate (and stream via hooks), stdin is at EOF, the clocks and randomness are real, and everything filesystem/network answers PERMISSION_DENIED.

/** Thrown by the `exit` import to unwind the wasm stack with an exit code. */
export class ExitSignal extends Error {
  constructor(code) {
    super(`exit(${code})`);
    this.code = code;
  }
}

/**
 * Run a compiled program. `config` carries:
 * - `program`, `bridge`: the module bytes (the program from `compile`, the bridge from `bridge_bytes`);
 * - `status`, `stdio`: the wire code tables from `abi`;
 * - `hooks`: optional `{ onStdout?, onStderr?, foreign? }` — `onStdout`/`onStderr` are streaming callbacks, each receiving a `Uint8Array` per write; `foreign` implements the program's own `foreign` declarations, keyed by fully qualified name (e.g. `{ "/frobnicate": fn }`) — it is passed through as the `ffi` import object, so a missing implementation surfaces as a `LinkError` naming the import.
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

  // A handle's wire encoding is the little-endian bytes of its token — `[0]` for stdin, since the encoder mints one zero byte for zero. This decoder also happens to read the empty string as 0, but nothing ever sends it.
  const tokenOf = (handle) => {
    const bytes = decodeBytes(handle);
    let token = 0;

    for (let i = bytes.length - 1; i >= 0; i--) {
      token = token * 256 + bytes[i];
    }

    return token;
  };

  const emptyBytes = () => encodeBytes(new Uint8Array(0));

  const output = { stdout: [], stderr: [] };

  const hooks = config.hooks ?? {};

  const write = (handle, ref) => {
    const bytes = decodeBytes(ref);

    switch (tokenOf(handle)) {
      case config.stdio.STDOUT:
        output.stdout.push(bytes);
        hooks.onStdout?.(bytes);
        break;
      case config.stdio.STDERR:
        output.stderr.push(bytes);
        hooks.onStderr?.(bytes);
        break;
      // Writing to stdin is a loud tripwire (a trap), mirroring MockHost's panic; the browser has no fd 0 for OsHost's POSIX passthrough.
      case config.stdio.STDIN:
        throw new Error("write to stdin");
      default:
        return [config.status.PERMISSION_DENIED, 0];
    }

    return [config.status.OK, bytes.length];
  };

  const deniedHandle = () => [config.status.PERMISSION_DENIED, emptyBytes()];

  const denied = () => config.status.PERMISSION_DENIED;

  // The standard streams take `SO_REUSEADDR` like a file — recording nothing and answering OK, as OsHost and MockHost do; no other handle exists in the browser, so anything else stays denied.
  const reuseaddr = (handle) => {
    switch (tokenOf(handle)) {
      case config.stdio.STDIN:
      case config.stdio.STDOUT:
      case config.stdio.STDERR:
        return config.status.OK;
      default:
        return config.status.PERMISSION_DENIED;
    }
  };

  const unsupported = (name) => () => {
    throw new Error(`${name} is not supported in the browser playground`);
  };

  // The `sys` import object, keyed by wire name. A `host_ops` row without a browser implementation surfaces as a `LinkError` naming the import when a program calls it.
  const sysEnv = {
    read: () => [config.status.EOF, emptyBytes()],
    write: write,
    open: deniedHandle,
    lookup: deniedHandle,
    resolve: unsupported("resolve"),
    socket: deniedHandle,
    bind: denied,
    connect: denied,
    finish_connect: denied,
    listen: denied,
    accept: deniedHandle,
    start_tls: denied,
    tls_server_config: deniedHandle,
    start_tls_server: denied,
    set_reuseaddr: reuseaddr,
    poll: unsupported("poll"),
    close: () => {},
    clock_wall: () => {
      const millis = Date.now();
      const secs = Math.floor(millis / 1000);

      // The runtime splits the 64-bit seconds base-10⁹ into two Nat limbs.
      return [
        Math.floor(secs / 1_000_000_000),
        secs % 1_000_000_000,
        (millis % 1000) * 1_000_000,
      ];
    },
    clock_mono: () => {
      const millis = performance.now();

      // Floor, not round: a fractional millisecond just below 1000 would otherwise round the nanos limb up to exactly 10⁹, which the seconds limb owns.
      return [
        Math.floor(millis / 1000),
        Math.floor((millis % 1000) * 1_000_000),
      ];
    },
    random: (count) => {
      const bytes = new Uint8Array(count);

      // Web Crypto caps one `getRandomValues` at 65536 bytes (a `QuotaExceededError` past it), so a larger request is filled a slice at a time; the native host has no such ceiling, and `rand/bytes` promises none.
      for (let offset = 0; offset < count; offset += 65536) {
        crypto.getRandomValues(bytes.subarray(offset, offset + 65536));
      }

      return encodeBytes(bytes);
    },
    args: unsupported("args"),
    env: () => [config.status.NOT_FOUND, emptyBytes()],
    // The playground has no terminal to switch or measure, so both tty rows are denied as `open` is.
    raw: denied,
    size: () => [config.status.PERMISSION_DENIED, 0, 0],
    // No filesystem either: every filesystem row is denied as `open` is. `list` would answer a `List(Bytes)`, a shape the bridge has no encoder for — `resolve` and `args` are unsupported for the same reason — so it traps by name rather than returning a value it cannot build.
    stat: () => [config.status.PERMISSION_DENIED, 0, 0, 0, 0, 0, 0],
    remove_file: denied,
    rename: denied,
    list: unsupported("list"),
    create_dir: denied,
    remove_dir: denied,
    cwd: deniedHandle,
    // WASI has no process creation and neither does the playground.
    spawn: deniedHandle,
    stream: deniedHandle,
    wait: () => [config.status.PERMISSION_DENIED, 0, 0],
    kill: denied,
    exit: (code) => {
      throw new ExitSignal(code);
    },
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
    const { instance } = await WebAssembly.instantiate(config.program, {
      sys: sysEnv,
      ffi: hooks.foreign ?? {},
    });

    instance.exports["func/main"]();

    return result();
  } catch (error) {
    if (error instanceof ExitSignal) {
      return { ...result(), exitCode: error.code };
    }

    return { ...result(), exitCode: null, trap: String(error) };
  }
}
