//! The numeric wire codes for `/sys/Handle`'s status, poll-event, open-mode, file-kind, stdio-wiring, serial-parity, serial-flow, serial-op, and stdio-handle tags, each module named by the tag it holds, and the [`errno`]s named rather than passed through from an operating system. Each set is mirrored by a guest-side `/sys` module of the same name; the host vocabulary's `Failure`, `Readiness` and closed-code types cite these constants for the codes they cross as, and both ends cite [`stdio`] for the well-known handle tokens.
//!
//! **The codes are one table, and every mirror of them is read off it.** A row is a code's constant, its value, the name the guest reads it by where the guest reads it at all, and, for a status that names a failure, the [`Failure`] it is the code of. The constants below, [`Failure::code`], the `/sys` modules the compiler mints ([`code_sets`]) and the browser's tables are each a projection of those rows, so a code cannot be given one value on one side of the wire and another on the other, a new status cannot go unnamed in the guest, and a failure cannot be left without a code.

use crate::Failure;

/// One code as the guest reads it: the constant it is here, the name `/sys` mints it under, and its value.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Code {
    pub constant: &'static str,
    pub guest: &'static str,
    pub value: u64,
}

/// What the guest reads a set's codes as: a `Nat`, or the `Byte` a set of flags rides.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CodeCarrier {
    Nat,
    Byte,
}

/// One set of codes as the guest reads it: the `/sys` module that mirrors it, named as its module is here, and the codes that module holds. A constant the guest has no name for — a bound, a token minted elsewhere — is not among them.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeSet {
    pub name: &'static str,
    pub carrier: CodeCarrier,
    pub codes: Vec<Code>,
}

/// Project the table to a module of constants for each set, the sets as the guest reads them, and the code of every failure a status names.
macro_rules! code_sets {
    ($(
        $(#[doc = $set_doc:literal])*
        $set:ident: $ty:ty as $carrier:ident {
            $(
                $(#[doc = $doc:literal])*
                $name:ident = $value:expr $(=> $guest:ident)? $(, $failure:ident)?;
            )*
        }
    )*) => {
        $(
            $(#[doc = $set_doc])*
            pub mod $set {
                $($(#[doc = $doc])* pub const $name: $ty = $value;)*
            }
        )*

        /// Every set of codes the guest reads, in the table's order, each code under the name `/sys` mints it by: what the compiler's `/sys` and the browser's tables are built from.
        pub fn code_sets() -> Vec<CodeSet> {
            vec![$(
                CodeSet {
                    name: stringify!($set),
                    carrier: CodeCarrier::$carrier,
                    codes: vec![$($(
                        Code {
                            constant: stringify!($name),
                            guest: stringify!($guest),
                            value: u64::from($set::$name),
                        },
                    )?)*],
                },
            )*]
        }

        impl Failure {
            /// Every named failure, in the table's order: each status the table names a failure by.
            pub const NAMED: &'static [Failure] = &[$($($(Failure::$failure,)?)*)*];

            /// The wire code the guest decodes. The named failures have fixed tags; `Other(errno)` lowers as [`OTHER_BASE`](status::OTHER_BASE) plus its carried errno, keeping the errno lane disjoint from the named codes.
            pub fn code(self) -> u64 {
                match self {
                    $($($(Failure::$failure => u64::from($set::$name),)?)*)*
                    Failure::Other(errno) => status::OTHER_BASE + u64::from(errno),
                }
            }
        }
    };
}

code_sets! {
    /// Status codes of failable IO ops, mirrored by the guest's `/sys/status` and decoded into `/std/Io/Error`. `Other` has no fixed code here: it lowers its carried errno offset by `OTHER_BASE`, keeping the errno lane disjoint from the named codes.
    status: u64 as Nat {
        /// The op succeeded — the reply's payload fields are meaningful only under this code.
        OK = 0 => ok;
        /// A `handle_read` reached the end of the stream and returned no bytes. Terminal but not a fault — the guest's stream consumers stop on it rather than erroring.
        EOF = 1 => eof;
        /// The named thing does not exist: a `file_open` path, a `proc_env` variable, or a `dns_resolve` that yielded no addresses (mapped at that call site, since the OS reports it errno-less).
        NOT_FOUND = 2 => not_found, NotFound;
        /// The OS denied access to the path or socket op (`ErrorKind::PermissionDenied`).
        PERMISSION_DENIED = 3 => permission_denied, PermissionDenied;
        /// The target the op would create already exists (`ErrorKind::AlreadyExists`).
        ALREADY_EXISTS = 4 => exists, AlreadyExists;
        /// A `socket_connect` was actively refused — no listener at the target address.
        CONNECTION_REFUSED = 5 => refused, ConnectionRefused;
        /// A non-blocking op could not make progress right now. Retriable by design: `/std/Handle` matches on it to poll the handle and retry instead of surfacing a failure.
        WOULD_BLOCK = 6 => would_block, WouldBlock;
        /// A TLS upgrade or server-config build failed. `rustls` errors carry no OS errno, so they collapse to this one named code instead of riding the errno passthrough.
        TLS_ERROR = 7 => tls, TlsError;
        /// A `dir_remove` on a directory that still has entries (`ErrorKind::DirectoryNotEmpty`). Named, with the two below, because a program removing a directory has to tell them apart portably and the browser has no errno to pass through.
        NOT_EMPTY = 8 => not_empty, NotEmpty;
        /// A file operation applied to a directory (`ErrorKind::IsADirectory`).
        IS_DIRECTORY = 9 => is_directory, IsDirectory;
        /// A directory operation applied to something that is not one (`ErrorKind::NotADirectory`).
        NOT_DIRECTORY = 10 => not_directory, NotDirectory;
        /// The peer reset the connection (`ErrorKind::ConnectionReset`). Named because the operating systems disagree on its errno — 104 on Linux, 54 on macOS — so a program telling a reset from any other failure could not spell it once.
        CONNECTION_RESET = 11 => reset, ConnectionReset;
        /// The errno passthrough lane: `Status::Other(errno)` lowers as `OTHER_BASE + errno`, one past the last named code, so a raw OS errno — EIO is 5, ENXIO is 6 — can never masquerade as `OK` or a named failure. The guest's `Io/Error/of` subtracts it back out.
        OTHER_BASE = CONNECTION_RESET + 1 => other_base;
        /// The largest errno the lane carries: an OS reports a positive `int`, so a code past `OTHER_BASE + ERRNO_MAX` is no failure any host can answer.
        ERRNO_MAX = i32::MAX as u64;
    }

    /// What `handle_poll` reports of one handle of one of its lists — a bitmask of one byte, mirrored by `/sys/event`. A poll states its interest by the list a handle stands in, so no flag asks for anything: all three are a host's to report.
    event: u8 as Byte {
        /// The handle is ready for what its list waits on: to read, in the readers', and to write, in the writers'.
        READY = 0b001 => ready;
        /// The handle is in an error state, or names nothing a poll can watch.
        ERR = 0b010 => err;
        /// The peer hung up.
        HUP = 0b100 => hup;
    }

    /// `file_open` modes, mirrored by `/sys/open_mode` and the guest's `/std/File/Mode`.
    open_mode: u64 as Nat {
        /// Open an existing file read-only.
        READ = 0 => read;
        /// Open for writing: created if absent, truncated if present.
        WRITE = 1 => write;
        /// Open for appending: created if absent, every write lands at the end.
        APPEND = 2 => append;
    }

    /// What `file_stat` found at a path, mirrored by `/sys/file_kind` and the guest's `/std/fs/Kind`. `file_stat` follows symbolic links, so `SYMLINK` is reported only where the link's target is missing.
    file_kind: u64 as Nat {
        /// A regular file.
        FILE = 0 => file;
        /// A directory.
        DIRECTORY = 1 => directory;
        /// A symbolic link whose target is missing — the one case following the link finds nothing to report.
        SYMLINK = 2 => symlink;
        /// Anything else: a device, a socket, a pipe.
        OTHER = 3 => other;
    }

    /// How `proc_spawn` wires each of a child's standard streams, mirrored by `/sys/stdio_mode` and the guest's `/std/Command/Stdio`: the shape Lean's `Stdio`, Haskell's `StdStream`, Rust's `Stdio` and Zig's `StdIo` share.
    stdio_mode: u64 as Nat {
        /// The child shares the parent's stream.
        INHERIT = 0 => inherit;
        /// The stream is a pipe the parent holds the other end of, as a handle.
        PIPE = 1 => pipe;
        /// The stream is attached to the null device.
        NULL = 2 => null;
    }

    /// The parity `serial_open` frames a character with, mirrored by `/sys/serial_parity` and the guest's `/std/Serial/Parity`. Mark and space parity are deliberately absent: the one modern design that pruned by usage, Web Serial, ships exactly these three.
    serial_parity: u64 as Nat {
        /// No parity bit.
        NONE = 0 => none;
        /// A parity bit making the count of one bits even.
        EVEN = 1 => even;
        /// A parity bit making the count of one bits odd.
        ODD = 2 => odd;
    }

    /// How `serial_open` paces the wire, mirrored by `/sys/serial_flow` and the guest's `/std/Serial/Flow`. Software (XON/XOFF) flow control is deliberately absent, as it is from Web Serial.
    serial_flow: u64 as Nat {
        /// No flow control.
        NONE = 0 => none;
        /// Hardware flow control over RTS and CTS.
        HARDWARE = 1 => hardware;
    }

    /// What `serial_control` does to a port, mirrored by `/sys/serial_op`. The two output lines a program drives to reset a board or enter its bootloader, and the one buffer it drops afterwards.
    serial_op: u64 as Nat {
        /// Set the Data Terminal Ready line to the given level.
        DTR = 0 => dtr;
        /// Set the Request To Send line to the given level.
        RTS = 1 => rts;
        /// Drop what the device sent and the program has not read.
        DISCARD_INPUT = 2 => discard_input;
    }

    /// The well-known stdio handle tokens minted by the `/sys/Handle` prelude. A handle's wire encoding is the little-endian `Natural` bytes of its token (see `Handle::encode`), which mints one zero byte for zero — so STDIN encodes as `[0]`, never the empty byte string.
    stdio: u32 as Nat {
        /// Standard input — the token whose wire encoding is the single zero byte `[0]`.
        STDIN = 0;
        /// Standard output.
        STDOUT = 1;
        /// Standard error.
        STDERR = 2;
        /// The handle that is always ready: no host mints it or files anything under it, so every wait on it ends at once. What a description suspends on to let what waits beside it run.
        READY = 3;
    }

    /// The errnos named here rather than passed through from an operating system, each the value both release targets give it: what a host with no operating system of its own — the mock host, the browser — answers in one's place, and what the standard library raises for an argument it refuses before the call. `/sys/errno` mirrors the one the standard library raises, so that failure is one status whichever side names it.
    errno: u32 as Nat {
        /// A stream used in a direction it is not open for: reading `stdout` or `stderr`, writing `stdin`, or using a file against its open mode.
        EBADF = 9;
        /// An argument no host could act on: a serial frame outside the row's ranges, or an environment entry with no name.
        EINVAL = 22 => inval;
    }
}
