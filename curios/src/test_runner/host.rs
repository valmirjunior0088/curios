//! The host a test runs under natively: the operating system's, with the rows a test writes on the standard streams through read on their way, so the runner knows whether the node it opened for the test still has its line.

#[cfg(test)]
mod tests;

use {
    crate::{Wrote, cut},
    curios_abi::{ChildHandles, Failure, Handle, HostFront, HostOps, StdioMode},
    std::sync::{
        Arc,
        atomic::{AtomicBool, Ordering},
    },
};

/// What the test in hand has written on the standard streams, shared by the host that sees each write and the runner that closes the test's node.
#[derive(Default)]
pub struct Frame {
    /// Whether the test wrote anything.
    wrote: AtomicBool,
    /// Whether the last byte it wrote left a line open.
    open: AtomicBool,
}

impl Frame {
    /// The test is about to write on a standard stream, or to hand one to something that will. Its node's line is ended ahead of the first such write, which is what keeps the bytes that follow off the end of the test's name.
    fn writing(&self) {
        if !self.wrote.swap(true, Ordering::SeqCst) {
            cut();
        }
    }

    /// What the test wrote, as its node is closed by.
    pub fn wrote(&self) -> Wrote {
        match (
            self.wrote.load(Ordering::SeqCst),
            self.open.load(Ordering::SeqCst),
        ) {
            (false, _) => Wrote::Nothing,
            (true, false) => Wrote::Lines,
            (true, true) => Wrote::Open,
        }
    }
}

/// A host in front of `host`, which answers every row: the test's own writes are neither held nor changed, only seen.
pub struct TestHost<H> {
    host: H,
    frame: Arc<Frame>,
}

impl<H> TestHost<H> {
    /// `host`, with what the test writes through it told to `frame`.
    pub fn new(host: H, frame: Arc<Frame>) -> Self {
        Self { host, frame }
    }
}

impl<H: HostOps> HostFront for TestHost<H> {
    type Behind = H;

    fn behind(&self) -> &H {
        &self.host
    }

    fn handle_write(&self, h: Handle, b: Vec<u8>) -> Result<u64, Failure> {
        if !matches!(h, Handle::Stdout | Handle::Stderr) || b.is_empty() {
            return self.host.handle_write(h, b);
        }

        self.frame.writing();
        // The bytes are handed over by value and the reply says how many of them were taken, so they are kept to read the last one taken off.
        let kept = b.clone();
        let reply = self.host.handle_write(h, b);
        let last = reply
            .as_ref()
            .ok()
            .and_then(|taken| usize::try_from(*taken).ok()?.checked_sub(1))
            .and_then(|last| kept.get(last));
        if let Some(last) = last {
            self.frame.open.store(*last != b'\n', Ordering::SeqCst);
        }

        reply
    }

    fn proc_spawn(
        &self,
        argv: Vec<Vec<u8>>,
        cwd: Vec<u8>,
        env: Vec<Vec<u8>>,
        stdin: StdioMode,
        stdout: StdioMode,
        stderr: StdioMode,
    ) -> Result<ChildHandles, Failure> {
        // A child that inherits an output writes past this host. Where it leaves the line is not seen, and is taken as a line's end, which is where a program that ends leaves one.
        if stdout == StdioMode::Inherit || stderr == StdioMode::Inherit {
            self.frame.writing();
        }

        self.host.proc_spawn(argv, cwd, env, stdin, stdout, stderr)
    }

    fn tty_take(&self, h: Handle, leave: Vec<u8>) -> Result<(), Failure> {
        // A terminal taken is written to past this host: the host behind writes `leave` on it when it is given back.
        self.frame.writing();

        self.host.tty_take(h, leave)
    }
}
