//! The host a test runs under natively: the operating system's, with what a test writes on the standard streams framed on its way through, a line each behind the dashed trunk beneath the test's node, so what a test wrote reads as the test's.

#[cfg(test)]
mod tests;

use {
    crate::{Column, Owed, Wrote, answered, cut, frame_of, framed_run, reader},
    curios_abi::{ChildHandles, Failure, Handle, HostFront, HostOps, StdioMode},
    curios_common::Style,
    std::sync::{
        Arc, Mutex, MutexGuard, PoisonError,
        atomic::{AtomicBool, Ordering},
    },
};

/// What the test in hand has written on the standard streams, shared by the host that frames each write and the runner that closes the test's node.
#[derive(Default)]
pub struct Frame {
    /// Whether the test wrote anything.
    wrote: AtomicBool,
    /// Where its writes stand in their line. The two streams are read as one line, which is what they are on a terminal and in a pipe that joins them; apart, a line one of them leaves open is taken for the other's too.
    column: Mutex<Column>,
}

impl Frame {
    /// The test is about to write on a standard stream, or to hand one to something that will. Its node's line is ended ahead of the first such write, which is what keeps the bytes that follow off the end of the test's name.
    fn writing(&self) {
        if !self.wrote.swap(true, Ordering::SeqCst) {
            cut();
        }
    }

    /// Where the test's writes stand, held. A writer that panicked left the column it reached, so its poison is passed over.
    fn column(&self) -> MutexGuard<'_, Column> {
        self.column.lock().unwrap_or_else(PoisonError::into_inner)
    }

    /// What the test wrote, as its node is closed by.
    pub fn wrote(&self) -> Wrote {
        match (self.wrote.load(Ordering::SeqCst), *self.column()) {
            (false, _) => Wrote::Nothing,
            (true, Column::Start) => Wrote::Lines,
            (true, Column::Within | Column::Returned) => Wrote::Open,
        }
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

impl<H: HostOps> TestHost<H> {
    /// Write all of `bytes`, the runner's own, on `h`: a frame is never left half written before the test's bytes. A stream that takes nothing is left as it is.
    fn put(&self, h: &Handle, mut bytes: Vec<u8>) -> Result<(), Failure> {
        while !bytes.is_empty() {
            let wrote = self.host.handle_write(h.clone(), bytes.clone())?;
            let wrote = usize::try_from(wrote).map_or(bytes.len(), |wrote| wrote.min(bytes.len()));
            if wrote == 0 {
                break;
            }
            bytes.drain(..wrote);
        }

        Ok(())
    }

    /// Write `own`, a run of the test's bytes on one line, on `h` behind what the line is `owed`, and answer how many of them the host took. `column` is left where the stream stands.
    fn run(
        &self,
        h: &Handle,
        owed: Owed,
        own: &[u8],
        style: &Style,
        column: &mut Column,
    ) -> Result<usize, Failure> {
        if owed != Owed::Nothing {
            self.put(h, frame_of(owed, style).into_bytes())?;
            // The frame opened the line, whatever becomes of the bytes behind it.
            *column = Column::Within;
        }
        let wrote = self.host.handle_write(h.clone(), own.to_vec())?;
        let wrote = usize::try_from(wrote).map_or(own.len(), |wrote| wrote.min(own.len()));
        *column = column.after(&own[..wrote]);

        Ok(wrote)
    }
}

impl<H: HostOps> HostFront for TestHost<H> {
    type Behind = H;

    fn behind(&self) -> &H {
        &self.host
    }

    fn handle_write(&self, h: Handle, b: Vec<u8>) -> Result<u64, Failure> {
        // Each stream's frame is painted for whoever reads that stream.
        let style = match h {
            Handle::Stdout => answered(),
            Handle::Stderr => reader(),
            _ => return self.host.handle_write(h, b),
        };
        if b.is_empty() {
            return self.host.handle_write(h, b);
        }

        self.frame.writing();
        let mut column = self.frame.column();
        let mut taken = 0;
        while taken < b.len() {
            let (owed, length) = framed_run(*column, &b[taken..]);
            let own = &b[taken..taken + length];
            let wrote = match self.run(&h, owed, own, style, &mut column) {
                Ok(wrote) => wrote,
                Err(failure) if taken == 0 => return Err(failure),
                // What the host took before it failed was written: the test is told of that much, and meets the failure at its next write.
                Err(_) => break,
            };
            taken += wrote;
            if wrote < own.len() {
                break;
            }
        }

        Ok(u64::try_from(taken).unwrap_or(u64::MAX))
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
            self.frame.writing();
        }

        self.host.proc_spawn(argv, cwd, env, stdin, stdout, stderr)
    }

    fn tty_take(&self, h: Handle, leave: Vec<u8>) -> Result<(), Failure> {
        // A terminal taken is written to past this host, unframed: the host behind writes `leave` on it when it is given back.
        self.frame.writing();

        self.host.tty_take(h, leave)
    }
}
