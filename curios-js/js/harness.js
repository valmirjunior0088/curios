// The browser run harness: instantiates a compiled curios program against a
// JS implementation of the host boundary and drives its entrypoint. Every
// name, code, and roster arrives via `config`, built Rust-side from
// curios-abi (see src/harness.rs / src/abi.rs), so this file cannot drift
// from the compiler or runtime — the sole exception is `io_exit`, hardcoded
// at every end.
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
 * - `sysNamespace`, `envNamespace`, `mainExport`, `importNames`, `status`,
 *   `stdio`: the ABI facts from `abi`;
 * - `foreignNames`: `compile`'s `foreignNames` roster — the `env`-tier
 *   imports the program's own `foreign` declarations require;
 * - `hooks`: optional `{ onStdout?, onStderr?, foreign? }` — `onStdout`/
 *   `onStderr` are streaming callbacks, each receiving a `Uint8Array` per
 *   write; `foreign` is a `{ name: fn, ... }` map implementing
 *   `foreignNames`.
 *
 * Resolves to `{ stdout, stderr, exitCode, trap }`: the accumulated output
 * bytes, the code the program exited with (0 when `main` returns), and the
 * trap message if execution failed instead.
 */
export async function run(config) {
  const bridge = (await WebAssembly.instantiate(config.bridge, {})).instance
    .exports;

  const decodeBin = (bin) => {
    const bytes = new Uint8Array(bridge.bin_len(bin));

    for (let i = 0; i < bytes.length; i++) {
      bytes[i] = bridge.bin_get(bin, i);
    }

    return bytes;
  };

  const encodeBin = (bytes) => {
    const bin = bridge.bin_new(bytes.length);

    for (let i = 0; i < bytes.length; i++) {
      bridge.bin_set(bin, i, bytes[i]);
    }

    return bin;
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

  const implementations = {
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

      return [
        Math.floor(millis / 1000),
        Math.round((millis % 1000) * 1_000_000),
      ];
    },
    io_random: (count) => {
      const bytes = new Uint8Array(count);
      crypto.getRandomValues(bytes);

      return encodeBin(bytes);
    },
    io_args: unsupported("io_args"),
    io_env: () => [config.status.NOT_FOUND, emptyBin()],
  };

  const env = {};

  for (const name of config.importNames) {
    const implementation = implementations[name];

    if (!implementation) {
      throw new Error(`no browser implementation for env.${name}`);
    }

    env[name] = implementation;
  }

  // `exit` is a hardcoded primitive at every end (see curios-abi's module
  // doc), so its import name is spelled directly here too.
  env.io_exit = (code) => {
    throw new ExitSignal(code);
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

  const foreignEnv = {};

  for (const name of config.foreignNames ?? []) {
    const implementation = hooks.foreign?.[name];

    if (!implementation) {
      throw new Error(`no foreign implementation supplied for env.${name}`);
    }

    foreignEnv[name] = implementation;
  }

  try {
    const { instance } = await WebAssembly.instantiate(config.program, {
      [config.sysNamespace]: env,
      [config.envNamespace]: foreignEnv,
    });

    instance.exports[config.mainExport]();

    return result();
  } catch (error) {
    if (error instanceof ExitSignal) {
      return { ...result(), exitCode: error.code };
    }

    return { ...result(), exitCode: null, trap: String(error) };
  }
}
