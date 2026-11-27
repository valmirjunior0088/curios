//! The host a test runs under natively: the operating system's, with what a test writes on the standard streams relayed through the transcript on its way, a line each quoted beneath the test's node, so what a test wrote reads as the test's.

#[cfg(test)]
mod tests;

use {
    crate::{Opened, Quoted, Relayed, Wrote, relay, written_past},
    curios_abi::{ChildHandles, Failure, Handle, HostFront, HostOps, StdioMode},
    std::sync::{Arc, Mutex, MutexGuard, PoisonError},
};

/// The test in hand, by the tree its node stands open on, and what it has written on the standard streams: shared by the host that relays each write and the runner that closes the test's node.
pub struct Frame {
    tree: Opened,
    relayed: Mutex<Relayed>,
}

impl Frame {
    /// The test whose node stands open on `tree`, before it writes anything.
    pub fn of(tree: Opened) -> Self {
        Self {
            tree,
            relayed: Mutex::default(),
        }
    }

    /// What the test has written, held. A writer that panicked left the column it reached, so its poison is passed over.
    fn relayed(&self) -> MutexGuard<'_, Relayed> {
        self.relayed.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// What the test wrote, as its node is closed by.
    pub fn wrote(&self) -> Wrote {
        self.relayed().wrote()
    }
}

/// A host in front of `host`, which answers every row: the test's own writes are never held back, and each line of them is written behind the frame.
pub struct TestHost<H> {
    host: H,
    frame: Arc<Frame>,
}

impl<H> TestHost<H> {
    /// `host`, with what the test writes through it framed, and told to `frame`.
    pub fn new(host: H, frame: Arc<Frame>) -> Self {
        Self { host, frame }
    }
}

impl<H: HostOps> HostFront for TestHost<H> {
    type Behind = H;

    fn behind(&self) -> &H {
        &self.host
    }

    fn handle_write(&self, h: Handle, b: Vec<u8>) -> Result<(), Failure> {
        let quoted = match h {
            Handle::Stdout => Quoted::Output,
            Handle::Stderr => Quoted::Error,
            _ => return self.host.handle_write(h, b),
        };
        if b.is_empty() {
            return self.host.handle_write(h, b);
        }

        // Each run the transcript frames is taken whole by the stream the test wrote on.
        relay(
            self.frame.tree,
            &mut self.frame.relayed(),
            quoted,
            &b,
            |run| self.host.handle_write(h.clone(), run.to_vec()),
        )
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
        // A child that inherits an output writes past this host, so what it writes is not framed. Where it leaves the line is not seen, and is taken as where the test left it.
        if stdout == StdioMode::Inherit || stderr == StdioMode::Inherit {
            written_past(&mut self.frame.relayed());
        }

        self.host.proc_spawn(argv, cwd, env, stdin, stdout, stderr)
    }

    fn tty_take(&self, h: Handle, leave: Vec<u8>) -> Result<(), Failure> {
        // A terminal taken is written to past this host, unframed: the host behind writes `leave` on it when it is given back.
        written_past(&mut self.frame.relayed());

        self.host.tty_take(h, leave)
    }
}
