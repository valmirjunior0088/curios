//! Children of the native host: spawning a program with each standard stream wired as the guest asked, reaping it on a thread that signals a pipe the scheduler polls, and killing it by pid only while that pid is still its own.
//!
//! The reaping follows `os_resolver`'s pattern for a finished lookup: a thread does the blocking wait, records the end, and writes one byte to a pipe whose read end is the child's handle, so `handle_poll` sees the exit as readiness and `proc_wait` reads the record at once. One thread per child is the native host's cost for observing an exit without a signal handler; the guest never sees it.
//!
//! **The pid is the one raw OS identity the host holds that the OS recycles**, so the reaper never frees it behind `kill`'s back. It waits with `waitid(WEXITED | WNOWAIT)`, which leaves the exited child a zombie whose pid stays reserved, and only then, holding the child's state, reaps it and records how it ended — or the errno that kept it from knowing. `kill` holds the same state, so it signals only a child with no recorded end, whose pid is still its own, and answers `ok` without signaling once an end is recorded. **Host assumption:** the embedding process leaves `SIGCHLD` at its default and reaps nothing itself. A process that auto-reaps lets the OS free a pid the moment its child exits, which only a pidfd could guard, and macOS has none.
//!
//! Every fallible step of spawning comes before the child exists: the arguments are checked, each piped stream's pipe is made with the parent's end non-blocking, the reaper's pipe is made, and the reaper thread is started, waiting for the child to be handed to it. Once the child exists nothing can fail, so there is no half-spawned state to unwind.

use {
    super::{ChildExit, Failure, StdioMode, failure_from_error},
    rustix::{
        fs::{OFlags, fcntl_getfl, fcntl_setfl},
        io::Errno,
        process::{Pid, Signal, WaitId, WaitIdOptions, kill_process, waitid},
    },
    std::{
        ffi::OsStr,
        io::{Error, pipe},
        num::NonZeroU32,
        os::{
            fd::OwnedFd,
            unix::{ffi::OsStrExt, process::ExitStatusExt},
        },
        process::{Child, Command, ExitStatus, Stdio},
        sync::{Arc, Mutex, mpsc},
        thread,
    },
};

/// How a child ended, once the reaper has recorded it: its exit, or the failure that kept the reaper from observing one. `None` while it runs — and while it runs, its pid is its own.
type End = Option<Result<ChildExit, Failure>>;

/// A running child as the host files it: `done` is the read end of a pipe the reaper writes to once it has recorded the end, so `handle_poll` sees the exit; `end` then holds it; `pid` is what `proc_kill` addresses while no end is recorded. Dropping it closes the pipe's read end; the reaper's later write fails with `EPIPE` and is discarded, and the child is reaped regardless.
pub(crate) struct Running {
    pub(crate) done: OwnedFd,
    end: Arc<Mutex<End>>,
    pid: Pid,
}

impl Running {
    /// How the child ended, once the reaper has recorded it.
    pub(crate) fn end(&self) -> End {
        *self.end.lock().unwrap()
    }

    /// `SIGKILL` the child if no end is recorded, under the lock the reaper records one under — so a pid the reaper has reaped, and the OS may have handed to another process, is never signaled. An ended child answers `ok` unsignaled.
    pub(crate) fn kill(&self) -> Result<(), Failure> {
        let end = self.end.lock().unwrap();

        if end.is_some() {
            return Ok(());
        }

        kill_process(self.pid, Signal::KILL).map_err(errno_failure)
    }
}

/// Everything `proc_spawn` hands back: the running child and the parent's end of each piped stream, already non-blocking.
pub(crate) struct Spawned {
    pub(crate) child: Running,
    pub(crate) stdin: Option<OwnedFd>,
    pub(crate) stdout: Option<OwnedFd>,
    pub(crate) stderr: Option<OwnedFd>,
}

/// An OS errno as the failure the row reports.
fn errno_failure(errno: Errno) -> Failure {
    failure_from_error(Error::from(errno))
}

/// Whether `argv`, `cwd` and `env` can name a program at all: an empty `argv` names none, a NUL fits in no C string, and an environment entry needs a name before its `=`.
fn valid(argv: &[Vec<u8>], cwd: &[u8], env: &[Vec<u8>]) -> bool {
    let no_nul = |bytes: &[u8]| !bytes.contains(&0);

    !argv.is_empty()
        && argv.iter().all(|arg| no_nul(arg))
        && no_nul(cwd)
        && env
            .iter()
            .all(|entry| no_nul(entry) && !entry.is_empty() && entry[0] != b'=')
}

/// One standard stream's wiring, and the parent's end of its pipe when it is piped. The parent's end is non-blocking, since a fiber drains it and a read that blocked on one pipe while the child filled the other is the deadlock every process library documents; the child's end stays blocking, as a program expects its standard streams to be. Both ends are close-on-exec, so no other child inherits either.
fn wiring(mode: StdioMode, parent_reads: bool) -> Result<(Stdio, Option<OwnedFd>), Failure> {
    match mode {
        StdioMode::Inherit => Ok((Stdio::inherit(), None)),
        StdioMode::Null => Ok((Stdio::null(), None)),
        StdioMode::Pipe => {
            let (reader, writer) = pipe().map_err(failure_from_error)?;
            let (reader, writer) = (OwnedFd::from(reader), OwnedFd::from(writer));
            let (child, parent) = match parent_reads {
                true => (writer, reader),
                false => (reader, writer),
            };

            let flags = fcntl_getfl(&parent).map_err(errno_failure)?;
            fcntl_setfl(&parent, flags | OFlags::NONBLOCK).map_err(errno_failure)?;

            Ok((Stdio::from(child), Some(parent)))
        }
    }
}

/// How a reaped child ended. A signal that ended a child is never zero, and an exit code is `WEXITSTATUS`, a byte.
fn child_exit(status: ExitStatus) -> ChildExit {
    match status
        .signal()
        .and_then(|signal| NonZeroU32::new(signal.unsigned_abs()))
    {
        Some(signal) => ChildExit::Signal(signal),
        None => ChildExit::Code(status.code().map_or(0, |code| code as u8)),
    }
}

/// Wait for `child` to exit without reaping it, then reap it and record the end under the state `kill` reads, and only then signal `done`.
fn reap(mut child: Child, end: &Mutex<End>) {
    let pid = Pid::from_child(&child);

    // An interruption is not an exit, so the wait resumes; any other failure means the child cannot be waited on — something else reaped it — and is recorded as the end, which also keeps `kill` from a pid that may no longer be the child's.
    let exited = loop {
        match waitid(
            WaitId::Pid(pid),
            WaitIdOptions::EXITED | WaitIdOptions::NOWAIT,
        ) {
            Ok(_) => break Ok(()),
            Err(Errno::INTR) => continue,
            Err(errno) => break Err(errno_failure(errno)),
        }
    };

    let mut recorded = end.lock().unwrap();

    *recorded =
        Some(exited.and_then(|()| child.wait().map(child_exit).map_err(failure_from_error)));
}

/// Start the program `argv[0]` with the arguments after it, in `cwd` unless it is empty, with `env`'s `NAME=VALUE` entries laid over the inherited environment (an entry without `=` names a variable set to the empty string), and each standard stream wired as its mode says. Arguments no program can take are refused with `EINVAL` before anything is made.
pub(crate) fn spawn(
    argv: &[Vec<u8>],
    cwd: &[u8],
    env: &[Vec<u8>],
    (stdin, stdout, stderr): (StdioMode, StdioMode, StdioMode),
) -> Result<Spawned, Failure> {
    if !valid(argv, cwd, env) {
        return Err(errno_failure(Errno::INVAL));
    }

    let (program, args) = argv.split_first().expect("a valid argv names a program");
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

    let (stdin_wiring, stdin) = wiring(stdin, false)?;
    let (stdout_wiring, stdout) = wiring(stdout, true)?;
    let (stderr_wiring, stderr) = wiring(stderr, true)?;

    command
        .stdin(stdin_wiring)
        .stdout(stdout_wiring)
        .stderr(stderr_wiring);

    let (done, signal) = pipe().map_err(failure_from_error)?;
    let end = Arc::new(Mutex::new(None));
    let (hand_off, handed) = mpsc::channel::<Child>();

    // The reaper is started before the child, waiting to be handed it, so starting it is a failure that leaves nothing behind. A child that does not start drops the sender, and the reaper ends without one.
    let recorded = end.clone();
    thread::Builder::new()
        .name("curios-reaper".to_string())
        .spawn(move || {
            if let Ok(child) = handed.recv() {
                reap(child, &recorded);

                let _ = rustix::io::write(OwnedFd::from(signal), &[1]);
            }
        })
        .map_err(failure_from_error)?;

    let child = command.spawn().map_err(failure_from_error)?;
    let pid = Pid::from_child(&child);

    hand_off
        .send(child)
        .expect("the reaper waits for its child");

    Ok(Spawned {
        child: Running {
            done: OwnedFd::from(done),
            end,
            pid,
        },
        stdin,
        stdout,
        stderr,
    })
}
