//! Children of the native host: spawning a program with each standard stream wired as the guest asked, reaping it on a thread that signals a pipe the scheduler polls, and killing it by pid.
//!
//! The reaping follows `os_resolver`'s pattern for a finished lookup: a thread does the blocking `wait`, fills a slot, and writes one byte to a pipe whose read end is the child's handle, so `poll` sees the exit as readiness and `wait` drains the slot at once. One thread per child is the native host's cost for observing an exit without a signal handler; the guest never sees it.

use {
    super::{Status, status_from_error},
    curios_abi::stdio_mode,
    rustix::process::{Pid, Signal, kill_process},
    std::{
        ffi::OsStr,
        os::{
            fd::OwnedFd,
            unix::{ffi::OsStrExt, process::ExitStatusExt},
        },
        process::{Command, Stdio},
        sync::{Arc, Mutex},
        thread,
    },
};

/// How a child ended: its exit code, or the signal that ended it — exactly one is meaningful, and `signal` being zero says which.
#[derive(Clone, Copy)]
pub(crate) struct Exit {
    pub(crate) code: u32,
    pub(crate) signal: u32,
}

/// The cell the reaper fills once the child has exited, drained by the host's `wait`. Cloning shares the one underlying cell — the reaper holds one handle, the host the other.
#[derive(Clone, Default)]
pub(crate) struct ExitSlot {
    cell: Arc<Mutex<Option<Exit>>>,
}

impl ExitSlot {
    /// Host side: the exit if the reaper has recorded it, else `None` — the child still runs. Drains the cell.
    pub(crate) fn get(&self) -> Option<Exit> {
        self.cell.lock().unwrap().take()
    }
}

/// A running child as the host files it: `done` is the read end of a pipe the reaper writes to once the child has exited, so `poll` sees the exit; `exit` then holds it; `pid` is what `kill` addresses. Dropping it closes the pipe's read end; the reaper's later write fails with `EPIPE` and is discarded, and the child, already killed or finished, is reaped regardless.
pub(crate) struct Running {
    pub(crate) done: OwnedFd,
    pub(crate) exit: ExitSlot,
    pid: Pid,
}

impl Running {
    /// `SIGKILL` the child. The reaper thread still reaps it, so `wait` then reports the signal.
    pub(crate) fn kill(&self) -> Status {
        match kill_process(self.pid, Signal::KILL) {
            Ok(()) => Status::Ok,
            Err(errno) => status_from_error(std::io::Error::from(errno)),
        }
    }
}

/// Everything `spawn` hands back: the running child and whichever of its streams were piped.
pub(crate) struct Spawned {
    pub(crate) child: Running,
    pub(crate) stdin: Option<OwnedFd>,
    pub(crate) stdout: Option<OwnedFd>,
    pub(crate) stderr: Option<OwnedFd>,
}

/// One standard stream's wiring from its [`stdio_mode`] tag; an unknown tag inherits, the harmless reading.
fn wiring(tag: u32) -> Stdio {
    match tag {
        stdio_mode::PIPE => Stdio::piped(),
        stdio_mode::NULL => Stdio::null(),
        _ => Stdio::inherit(),
    }
}

/// Start the program `argv[0]` with the arguments after it, in `cwd` unless it is empty, with `env`'s `NAME=VALUE` entries laid over the inherited environment (an entry without `=` names a variable set to the empty string), and each standard stream wired by its tag. The reaper thread is started before the call returns, so an exit is never missed. An empty `argv` names no program and is refused as invalid input.
pub(crate) fn spawn(
    argv: &[Vec<u8>],
    cwd: &[u8],
    env: &[Vec<u8>],
    (stdin, stdout, stderr): (u32, u32, u32),
) -> std::io::Result<Spawned> {
    let Some((program, args)) = argv.split_first() else {
        return Err(std::io::Error::from(std::io::ErrorKind::InvalidInput));
    };

    let mut command = Command::new(OsStr::from_bytes(program));

    command.args(args.iter().map(|arg| OsStr::from_bytes(arg)));

    if !cwd.is_empty() {
        command.current_dir(OsStr::from_bytes(cwd));
    }

    for entry in env {
        let split = entry.iter().position(|&byte| byte == b'=');
        let (name, value) = match split {
            Some(index) => (&entry[..index], &entry[index + 1..]),
            None => (entry.as_slice(), &[][..]),
        };

        command.env(OsStr::from_bytes(name), OsStr::from_bytes(value));
    }

    command
        .stdin(wiring(stdin))
        .stdout(wiring(stdout))
        .stderr(wiring(stderr));

    let mut child = command.spawn()?;
    let (done, signal) = std::io::pipe()?;
    let exit = ExitSlot::default();
    let pid = Pid::from_child(&child);

    let stdin = child.stdin.take().map(OwnedFd::from);
    let stdout = child.stdout.take().map(OwnedFd::from);
    let stderr = child.stderr.take().map(OwnedFd::from);

    // The reaper: the blocking `wait`, off the scheduler thread. Slot first, then the wakeup byte, in that order — the host drains the slot the moment the read end fires.
    let slot = exit.clone();
    thread::spawn(move || {
        let ended = match child.wait() {
            Ok(status) => Exit {
                code: status.code().unwrap_or(0).unsigned_abs(),
                signal: status.signal().unwrap_or(0).unsigned_abs(),
            },
            // `wait` on a child this process spawned fails only if something else reaped it; report a clean zero rather than invent a code.
            Err(_) => Exit { code: 0, signal: 0 },
        };

        *slot.cell.lock().unwrap() = Some(ended);

        let _ = rustix::io::write(OwnedFd::from(signal), &[1]);
    });

    Ok(Spawned {
        child: Running {
            done: OwnedFd::from(done),
            exit,
            pid,
        },
        stdin,
        stdout,
        stderr,
    })
}
