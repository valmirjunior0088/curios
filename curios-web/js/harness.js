// The browser run harness: instantiates a compiled curios program against a
// JS implementation of the host boundary and drives its entrypoint. Like any
// embedder, this file spells the wire names itself — the `sys.io_*` import
// keys, the `sys`/`ffi` namespaces, the `func/main` export; the contract is
// pinned by the Rust test suite. Only the numeric status/stdio codes arrive
// via `config`, built Rust-side from curios-abi (see src/abi.rs).
//
// The browser host is deliberately shallow: stdout/stderr accumulate (and
// stream via hooks), stdin is at EOF, the clocks and randomness are real, and
// everything filesystem/network answers PERMISSION_DENIED.

/** Thrown by the `io_exit` import to unwind the wasm stack with an exit code. */
export class ExitSignal extends Error {
  constructor(code) {
    super(`exit(${code})`);
    this.code = code;
  }
}

/**
 * Run a compiled program. `config` carries:
 * - `program`, `bridge`: the module bytes (the program from `compile`, the
 *   bridge from `bridge_bytes`);
 * - `status`, `stdio`: the wire code tables from `abi`;
 * - `hooks`: optional `{ onStdout?, onStderr?, foreign? }` — `onStdout`/
 *   `onStderr` are streaming callbacks, each receiving a `Uint8Array` per
 *   write; `foreign` implements the program's own `foreign` declarations,
 *   keyed by fully qualified name (e.g. `{ "/frobnicate": fn }`) — it is
 *   passed through as the `ffi` import object, so a missing implementation
 *   surfaces as a `LinkError` naming the import.
 *
 * Resolves to `{ stdout, stderr, exitCode, trap }`: the accumulated output
 * bytes, the code the program exited with (0 when `main` returns), and the
 * trap message if execution failed instead.
 */
export async function run(config) {
  const bridge = (await WebAssembly.instantiate(config.bridge, {})).instance
    .exports;

  // Byte strings cross the boundary through the bridge's memory in one
  // `bin_load`/`bin_store` call per string; the memory starts empty and grows
  // here, JS-side, to fit the largest string seen so far.
  const memory = bridge.memory;

  const ensureCapacity = (length) => {
    const missing = length - memory.buffer.byteLength;

    if (missing > 0) {
      memory.grow(Math.ceil(missing / 65536));
    }
  };

  const decodeBin = (bin) => {
    const length = bridge.bin_len(bin);
    ensureCapacity(length);
    bridge.bin_load(bin);

    return new Uint8Array(memory.buffer, 0, length).slice();
  };

  const encodeBin = (bytes) => {
    ensureCapacity(bytes.length);
    new Uint8Array(memory.buffer).set(bytes);

    return bridge.bin_store(bytes.length);
  };

  // A handle's wire encoding is the little-endian bytes of its token, so the
  // empty byte string decodes to 0 (stdin).
  const tokenOf = (handle) => {
    const bytes = decodeBin(handle);
    let token = 0;

    for (let i = bytes.length - 1; i >= 0; i--) {
      token = token * 256 + bytes[i];
    }

    return token;
  };

  const emptyBin = () => encodeBin(new Uint8Array(0));

  const output = { stdout: [], stderr: [] };

  const hooks = config.hooks ?? {};

  const write = (handle, bin) => {
    const bytes = decodeBin(bin);

    switch (tokenOf(handle)) {
      case config.stdio.STDOUT:
        output.stdout.push(bytes);
        hooks.onStdout?.(bytes);
        break;
      case config.stdio.STDERR:
        output.stderr.push(bytes);
        hooks.onStderr?.(bytes);
        break;
      default:
        return [config.status.PERMISSION_DENIED, 0];
    }

    return [config.status.OK, bytes.length];
  };

  const deniedHandle = () => [config.status.PERMISSION_DENIED, emptyBin()];

  const denied = () => config.status.PERMISSION_DENIED;

  const unsupported = (name) => () => {
    throw new Error(`${name} is not supported in the browser playground`);
  };

  // The `sys` import object, keyed by wire name. A `sys_io` row without a
  // browser implementation surfaces as a `LinkError` naming the import when
  // a program calls it.
  const sysEnv = {
    io_read: () => [config.status.EOF, emptyBin()],
    io_write: write,
    io_open: deniedHandle,
    io_lookup: deniedHandle,
    io_resolve: unsupported("io_resolve"),
    io_socket: deniedHandle,
    io_bind: denied,
    io_connect: denied,
    io_listen: denied,
    io_accept: deniedHandle,
    io_start_tls: denied,
    io_tls_server_config: deniedHandle,
    io_start_tls_server: denied,
    io_set_nonblocking: denied,
    io_set_recv_timeout: denied,
    io_set_send_timeout: denied,
    io_set_reuseaddr: denied,
    io_poll: unsupported("io_poll"),
    io_close: () => {},
    io_clock_wall: () => {
      const millis = Date.now();
      const secs = Math.floor(millis / 1000);

      // The runtime splits the 64-bit seconds base-10⁹ into two Nat limbs.
      return [
        Math.floor(secs / 1_000_000_000),
        secs % 1_000_000_000,
        (millis % 1000) * 1_000_000,
      ];
    },
    io_clock_mono: () => {
      const millis = performance.now();

      // Floor, not round: a fractional millisecond just below 1000 would
      // otherwise round the nanos limb up to exactly 10⁹, which the seconds
      // limb owns.
      return [
        Math.floor(millis / 1000),
        Math.floor((millis % 1000) * 1_000_000),
      ];
    },
    io_random: (count) => {
      const bytes = new Uint8Array(count);
      crypto.getRandomValues(bytes);

      return encodeBin(bytes);
    },
    io_args: unsupported("io_args"),
    io_env: () => [config.status.NOT_FOUND, emptyBin()],
    io_exit: (code) => {
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
