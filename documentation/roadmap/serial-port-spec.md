# A program talks to a serial device

## Status

Researched and probed, not started. This file records a survey of seven serial-port libraries across seven languages, the distance between what they agree on and what the host and `/std` offer — read from source and probed through `wonder` on 2026-09-01, every claim marked *probed* elaborated through it, and *run* where it says so — the one host row and one module that close it, the test that pins each, the decisions taken, and the two still open. It is written over the runtime prerequisites the indispensable tier landed in `curios-runtime` — stdin read on the raw descriptor, a pipe kind the host makes non-blocking for real, and a codec carrying seven-slot rows and byte-string lists — and it depends on nothing else from that campaign. The module keeps its own contract in `curios-prelude-archive/std/Serial.crs` once written, and nothing here restates one. Nothing is started.

## Why it exists

A program that talks to a device over a serial line — a microcontroller, a sensor, a modem, a radio — opens a path, sets a speed, exchanges bytes, and closes. Every language with a host surface has a library for it, and a Curios program cannot do it by any spelling: `file/open` has no read-write mode, the line discipline is never configured, and a file's reads block the scheduler. The objective is exactly that exchange and nothing more. Everything a serial library grows after its first release — control lines, enumeration, break, buffer counters — is deferred until a program asks, in the order the survey says they will be asked for.

## What the survey settles

pySerial, serialport-rs, go.bug.st/serial, Node's SerialPort, the Web Serial API, .NET's `System.IO.Ports.SerialPort` and Java's jSerialComm were read. Every one of the seven offers the same core with the same defaults, and the same long tail.

**What every one of them has.** Open by path with a baud rate; a frame of eight data bits, no parity, one stop bit and no flow control by default, each spelling it "8N1" where it spells it at all; byte read and write with nothing serial-specific on the data path — Rust and Java expose the port as their ordinary stream traits, Web Serial as a readable and a writable stream; close; a read timeout held as port state; and enumeration of ports by path in six of seven.

**What most have and few programs use.** DTR and RTS out and CTS, DSR, RI and CD in; a break signal; discarding the input or output buffer and waiting for output to drain; bytes-available counters, inter-byte timeouts, exclusive locks, reconfiguring an open port; and a framing layer — Node's delimiter and fixed-length parsers, jSerialComm's listeners, .NET's `ReadLine` — which is the one long-tail feature on the user side of nearly every serial program.

**What the survey gets wrong, so this does not.** The timeout every library carries as port state exists because none of them has a scheduler: a read that may never return needs a knob on the port. serialport-rs defaults it to zero, so a first read errors at once, which is that library's most-reported pitfall. Here a read parks on `poll` and `Async/timeout` bounds it, so the port carries no timeout and the pitfall has no place to occur. Every library documents a flush-after-open recipe against the stale bytes a device sends while resetting; under a non-blocking read, discarding pending input is a loop the library writes over the rows it has.

## What is certain

Read from source, and probed where it says so.

- **The handle rows are descriptor-generic, and a file is never non-blocking.** `read`, `write`, `poll` and `close` serve any resource holding a descriptor: `OsHost` polls a `File` through its raw descriptor and reads and writes it as a plain file. But `set_nonblocking` records nothing and answers `Ok` on a `File`, so `Async/read` on one blocks the whole scheduler on its first read, which is harmless for a regular file and fatal for a device that answers when it pleases. The tier's runtime prerequisites add a resource kind holding an owned descriptor whose non-blocking setter applies `O_NONBLOCK` through `fcntl`; a serial device is that kind.
- **`file/open` cannot open a port.** Its modes are read-only, write-with-truncate and append, and a port needs read plus write with `O_NOCTTY`. The row is not extended: a fourth mode would change `/std/File`'s contract for one consumer.
- **The termios recipe is where first contact fails, and the libraries never state it.** Raw mode alone is not enough. Without `CLOCAL` the line discipline ties the port to carrier detect and a device with no modem lines reads as hung up; without `CREAD` nothing is received; without raw mode, echo loops the device's bytes back at it and the XON and XOFF bytes vanish from binary data. pySerial, serialport-rs and go.bug.st all apply `cfmakeraw`, `CREAD` and `CLOCAL`, and none documents it as a design point.
- **rustix 1.1.4 has every call the host step needs and nothing else is added.** `tcgetattr`, `tcsetattr` with `OptionalActions::Now`, `Termios::make_raw`, `set_speed`, the `CREAD`, `CLOCAL`, `CSIZE`, `CS5` to `CS8`, `PARENB`, `PARODD`, `CSTOPB`, `CRTSCTS`, `IXON` and `IXOFF` flags, and `ioctl_tiocexcl` are under the `termios` feature the tier enables; the `OFlags` for the open are under `fs`, which is dependency-free and which the `pty` feature pulls in anyway; `openpt`, `grantpt`, `unlockpt` and `ptsname` are under `pty`. `set_speed` maps a non-standard rate to `BOTHER` on Linux and hands it to `cfsetspeed` on macOS, where the driver may refuse it at apply time. `curios-runtime` has no `libc` dependency, and this adds none.
- **The release targets are Linux and macOS**, so termios is the whole of line configuration and there is no Windows `DCB` to serve.
- **A program cannot name `/sys`** (probed). `/sys/Handle/read` in an entrypoint is refused with `sys is internal to the standard library; use the corresponding /std module`. A row is reached only through its `/std` wrapper, so the module below is the interface, not a convenience.
- **The framing loop and the discard loop elaborate over existing handles, and run** (probed, run). `read_until`, written generic over a `(Nat) -> Async(Handle/Read)` and reading one byte per call until the delimiter, elaborates in `Async` and, run over `File/read` on a real file, returns the bytes before the first newline. `discard_input`, written over `/std/Handle/read` — whose error sum already folds a would-block status into `would_block()` — and ending at `eof` or that error, elaborates and runs to end-of-file on a blocking handle; its would-block arm is elaborated and not exercised, since no non-blocking handle exists to probe it on until the row lands. The four settings inducts, the `Settings` struct, its default, a struct update replacing the baud, and a code function over a tag all elaborate as written below.
- **One spelling rule bit during the probe, and the sketch below uses the accepted form.** An arm `error(would_block())` beside `error(e)` is refused — a binder and a concrete shape cannot share a column in one group — so the error is matched in a nested `match`.
- **Timeouts need no probe of their own.** `curios/src/tests/scheduler.rs` pins `Async/timeout` returning `none` and running the cancelled body's finalizer, and `host/net_tests.rs` pins a socket read under one. The claim here is only that a port's read parks as a socket's does, which follows from the resource kind it is filed under.
- **Apple's `poll(2)` manual page states, under bugs, that poll does not support devices.** pySerial and go.bug.st use `select` on every Unix, and libuv special-cases terminals on Darwin. Whether a USB serial device polls correctly on current macOS is unverified from this tree, and the host's only readiness path is `poll`. rustix's `event` feature, already enabled, carries `select` and `kqueue` beside it.

## The design

### 1. The row

One host row, six operands and two results, inside the codec arities the tier widens to seven. Its method name is its wasm import name, and `open` is the file row's, so it is named as `socket as socket/open` is:

```
open_serial as serial/open [path: Bytes, baud: Nat, data_bits: Nat, parity: Nat, stop_bits: Nat, flow: Nat] [status: Status, handle: Handle];
```

Three tag families join `curios-abi`'s `codes.rs` as `parity` (`NONE`, `ODD`, `EVEN`), `stop_bits` (`ONE`, `TWO`) and `flow` (`NONE`, `HARDWARE`, `SOFTWARE`), each mirrored into `/sys` by the prelude and into the browser configuration by `curios-js`'s `abi.rs`, exactly as `mode` is and as the tier's `kind` and `stdio_mode` are. Data bits and baud are the numbers themselves. The row is written the five times every row is — the table, the mirror, `OsHost` with `MockHost` beside it, the codec binding, and the browser harness, which answers `permission_denied` as it answers `open`.

### 2. The host

`OsHost::open_serial` performs these steps in order, and the pseudo-terminal test below reads each back:

1. Open with `O_RDWR | O_NOCTTY | O_NONBLOCK | O_CLOEXEC`.
2. Take exclusive access with `TIOCEXCL`, so a second opener answers busy rather than interleaving bytes with the first.
3. Read the termios record. A path that is not a terminal — a regular file, say — closes the descriptor and answers `other(ENOTTY)`.
4. Apply raw mode, then set `CREAD` and `CLOCAL`, clear `CSIZE` and set the character size, set or clear `PARENB` and `PARODD`, `CSTOPB`, and either `CRTSCTS` or `IXON | IXOFF`, then the speed through `set_speed`. `HUPCL` is left as the driver has it, so close drops DTR as every surveyed library's close does.
5. Apply with `OptionalActions::Now`. A refused setting closes the descriptor and answers the errno.
6. File the descriptor under the tier's owned-descriptor kind.

There is no restore obligation: the settings belong to a device the program alone opened, and closing it is the whole exit. Under the existing rows, a read returns one to n bytes or would-block and the scheduler parks on read readiness; a write may take a prefix and `Async/write` resends the tail; an unplugged adapter reads as `eof` or an errno, and hang-up and error already count as readiness in `Async`'s `is_ready`, so no disconnect signal is added. The standard set of speeds up to 230400 is guaranteed on both targets; above it, Linux passes any rate through `BOTHER` and macOS is unverified.

`MockHost` gains a scripted port in its table: a path mapped to the settings it records, the bytes it answers reads with — served in chunks, with a would-block after each scripted chunk so the library's wait path is exercised — and the writes it captures. Opening a path not scripted answers `not_found`.

### 3. The library

`/std/Serial`, a type module as `File` is, registered in `std.crs` with its namesake re-exported:

```
pub struct Serial: Type { Handle }

pub induct DataBits: pub Type | five() | six() | seven() | eight() end
pub induct Parity:   pub Type | none() | odd() | even() end
pub induct StopBits: pub Type | one() | two() end
pub induct Flow:     pub Type | none() | hardware() | software() end

pub struct Settings: pub Type { baud: Nat, data_bits: DataBits, parity: Parity, stop_bits: StopBits, flow: Flow }
pub let default: Settings                                   -- 9600, eight, none, one, none

pub let open(path: Str, baud: Nat) -> Async(Result(Serial, Handle/Error))
pub let open_with(path: Str, settings: Settings) -> Async(Result(Serial, Handle/Error))
pub let read(s: Serial, n: Nat) -> Async(Handle/Read)
pub let write(s: Serial, b: Bytes) -> Async(Result({}, Handle/Error))
pub let read_until(s: Serial, delimiter: Byte) -> Async(Result(Bytes, Handle/Error))
pub let discard_input(s: Serial) -> Async(Result({}, Handle/Error))
pub let close(s: Serial) -> Async({})
pub let with(@A: Type, path: Str, settings: Settings, body: (Serial) -> Async(A)) -> Async(Result(A, Handle/Error))
```

`open_with` maps each tag to its `/sys` code by a match, as `File/of_mode` does, calls the row, reads the status through `Handle/error_of`, sets non-blocking through `Async/nonblocking`, and acquires the handle with `close` as its finalizer — the sequence `File/open` spells, closing the handle on either failure. `close` is `Async/release`. `open` is `open_with` at `default` with the baud replaced by a struct update. The inducts make every settings value total, so no invalid frame reaches the host and the row's `EINVAL` lane is reached only by a speed. The default is 9600 8N1 with no flow control because every surveyed library defaults to it.

`read_until` reads one byte per host call until the delimiter, `eof` or an error, and returns the bytes before the delimiter, dropping it. It is the shape the top-level stdin `read` already has, and it lives here rather than in `/std/Async` because one host call per byte is cheap at serial speeds and a trap at socket speeds. It keeps no remainder, so a program that mixes `read` and `read_until` on one port sees each byte once and in order, and a buffered reader that reads ahead is the deferred version. `discard_input` is the probed loop over `Handle/read`, lifted: it reads until would-block or `eof` and drops what it read, and reports every other error.

### 4. Tests

Three, and each names its file.

- **The library, through `MockHost`, in `curios/src/tests/host/serial_tests.rs`.** `open` records the codes its settings map to; `read` and `write` cross the scripted port; `read_until` returns the bytes before the delimiter across a chunk boundary and stops at `eof`; `discard_input` empties the scripted bytes; `with` closes on the body's exit; a refused path and a refused speed surface as the `Handle/Error` the status names; and cancelling a task that holds a port runs its finalizer.
- **The native open, over a pseudo-terminal pair, in `curios-runtime/src/os_host/tests.rs`.** `openpt` under rustix's `pty` feature as a dev-dependency feature alone, so the isolated launcher build is untouched. Open the secondary's path through `open_serial`: the flags read back with `O_NONBLOCK` set; the termios reads back raw with `CREAD` and `CLOCAL` set and the requested frame; bytes written to the primary are readable after `poll` reports readiness and a read past them answers would-block; bytes written to the secondary reach the primary; a regular file's path answers `ENOTTY`; a second open of the same secondary answers busy. A pseudo-terminal accepts every setting and ignores the speed, so the speed's effect is the hardware test's.
- **Hardware, by hand, recorded in this file when it is run.** A USB-to-serial adapter with a jumper from its transmit pin to its receive pin, at 115200: what is written comes back, and one line ends at `read_until`. 115200 is above the rate where the historical fixed constants end, so it exercises the speed mapping on both targets. On macOS the path is the `cu.` device, the callout convention every library documents, and the run there also answers the readiness question: before it, `python3 -c 'import os, select; fd=os.open(PATH, os.O_RDWR|os.O_NOCTTY|os.O_NONBLOCK); p=select.poll(); p.register(fd, select.POLLIN); print(p.poll(1000))'` prints an empty list or a readable hit when `poll` serves the device, and a `POLLNVAL` hit when it does not, in which case the host gains `select` or `kqueue` on that target before this item is complete there. Linux is the target this item lands on; macOS is complete when that probe has been run and recorded.

## Decisions taken

- One row, not an open and a configure. Folding the settings into the open leaves no open-but-unconfigured state and nothing to say about re-applying settings under in-flight bytes; reconfiguring an open port is deferred pain, and a second row is what would pay for it.
- Separate operands, not a packed frame code. The tier widens the codec to seven, and its spawn row sets the style: each tag its own operand, each family its own code module.
- The port is the tier's owned-descriptor kind, not a `File` and not a kind of its own. The `File` kind's no-op non-blocking setter is exactly the behaviour that would stall the scheduler, and a parallel kind would duplicate the pipe's.
- No timeout on the port. `Async/timeout` is the timeout, and the port is non-blocking so it can fire.
- Exclusive access on open. One ioctl, and the alternative failure is silent.
- The termios recipe is spelled in this file and read back by the test, because it is where every first contact fails and no surveyed library states it.
- `/std/Serial` is a type module, capitalized as `File` is, because the module's identity is its type; `serial` is the `/sys` subject, lowercase as `file` is under `File`.
- `read_until` is byte-per-call, keeps no remainder, and lives in `Serial` rather than `Async`. `discard_input` is over `Handle/read`, whose error sum already carries would-block, rather than over the raw status.
- The browser harness answers `permission_denied`. Web Serial exists, is asynchronous and permission-gated, and is outside a harness whose job is to run the playground.
- Linux first. The macOS readiness probe is recorded as the step that completes the item there, not as a blocker to landing it.

## Decisions to take

1. **The name of the tier's owned-descriptor kind** — settled as `Descriptor`, holding the `OwnedFd`, in `curios-runtime/src/os_host.rs`: named by what it holds, as `File` and `Listener` are, and saying the one thing that separates it from `File`, that `set_nonblocking` applies `O_NONBLOCK` to it for real. `Stream` would collide with the TLS variants, and `Pipe`, its first spelling, would have needed this rename the day the second consumer arrived.
2. **The device the hardware test names.** Recommended: a USB-to-serial adapter with a loopback jumper, since it has no firmware and tests exactly the byte path; an Arduino running an echo sketch as the second test, which adds a real line for `read_until`, the reset-on-open that DTR causes, and `discard_input` after the boot noise.

## Findings that are not this specification's

- `File`'s no-op non-blocking setter and the resource kind that replaces it for pipes are the tier's runtime prerequisites, and this file consumes them rather than restating them.
- A buffered reader that keeps a remainder, if a consumer asks for one, belongs beside `Async/drain` and serves sockets and files too; it is not a serial concern.

## Deliberately not specified

DTR and RTS setters and the modem status lines. Enumeration, which becomes a library-side scan of `/dev` over the tier's `dir/list` on Linux and needs IOKit on macOS. Break. Output drain and output discard. Bytes-available counters and inter-byte timeouts. Reconfiguring an open port. Speeds above 230400 on macOS. A parser layer beyond one delimiter. Web Serial in the browser harness. Windows.
