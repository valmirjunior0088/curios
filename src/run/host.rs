use std::{
    io::{Read, Write, stderr, stdin, stdout},
    sync::{
        Arc, Mutex,
        mpsc::{self, Receiver, RecvError, Sender},
    },
};

/// Number → string conversions used by both the wasm runtime (via the
/// `nat_to_str`/`int_to_str`/`flt_to_str` imports) and the `scalar_eval`
/// compile-time folder. Free functions, not trait methods, so the
/// compile-time and runtime conversions cannot diverge.
pub fn nat_to_str(value: u32) -> Vec<u8> {
    format!("{value}").into_bytes()
}

pub fn int_to_str(value: i32) -> Vec<u8> {
    format!("{value:+}").into_bytes()
}

pub fn flt_to_str(value: f32) -> Vec<u8> {
    format!("{value:+}").into_bytes()
}

pub trait Host {
    /// Read up to `count` bytes from `handle`, blocking until at least one
    /// byte is available. An empty return means EOF — nothing else.
    fn read(&self, handle: u32, count: u32) -> Vec<u8>;

    /// Write `bytes` to `handle`.
    fn write(&self, handle: u32, bytes: &[u8]);

    fn flt_to_le_bin(&self, value: f32) -> Vec<u8>;
}

/// The well-known handle tokens minted by the `/sys/Io` prelude constants.
pub const STDIN: u32 = 0;
pub const STDOUT: u32 = 1;
pub const STDERR: u32 = 2;

pub struct StdioHost;

impl Host for StdioHost {
    fn read(&self, handle: u32, count: u32) -> Vec<u8> {
        if handle != STDIN {
            return vec![];
        }

        let mut buffer = vec![0; count as usize];

        match stdin().lock().read(&mut buffer) {
            Ok(n) => {
                buffer.truncate(n);
                buffer
            }
            Err(_) => vec![],
        }
    }

    fn write(&self, handle: u32, bytes: &[u8]) {
        match handle {
            STDOUT => stdout().write_all(bytes).unwrap(),
            STDERR => stderr().write_all(bytes).unwrap(),
            _ => {}
        }
    }

    fn flt_to_le_bin(&self, value: f32) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }
}

pub struct ChannelHost {
    input_receiver: Mutex<Receiver<Vec<u8>>>,
    /// Bytes received from the channel but not yet consumed by `read` —
    /// short reads must never drop the remainder of a message.
    input_leftover: Mutex<Vec<u8>>,
    /// Writes to stdout and stderr both land here; tests do not distinguish
    /// the two streams.
    output_sender: Arc<Mutex<Sender<Vec<u8>>>>,
}

impl ChannelHost {
    pub fn in_out<L, I>(lines: I) -> (Self, Receiver<Vec<u8>>)
    where
        L: AsRef<[u8]>,
        I: IntoIterator<Item = L>,
    {
        let (input_sender, input_receiver) = mpsc::channel();
        let (output_sender, output_receiver) = mpsc::channel();

        for line in lines {
            input_sender.send(line.as_ref().to_vec()).unwrap();
        }

        (
            ChannelHost {
                input_receiver: Mutex::new(input_receiver),
                input_leftover: Mutex::new(Vec::new()),
                output_sender: Arc::new(Mutex::new(output_sender)),
            },
            output_receiver,
        )
    }

    pub fn out() -> (Self, Receiver<Vec<u8>>) {
        Self::in_out::<&[u8], [&[u8]; 0]>([])
    }
}

impl Host for ChannelHost {
    fn read(&self, handle: u32, count: u32) -> Vec<u8> {
        if handle != STDIN {
            return vec![];
        }

        let mut leftover = self.input_leftover.lock().unwrap();

        // Each channel message is one injected line; the newline the terminal
        // would deliver is appended here. Refill only when the buffer is dry,
        // then serve up to `count` bytes and stash the rest.
        if leftover.is_empty() {
            match self.input_receiver.lock().unwrap().recv() {
                Ok(line) => {
                    leftover.extend(line);
                    leftover.push(b'\n');
                }
                Err(RecvError) => return vec![],
            }
        }

        let served = leftover.len().min(count as usize);
        leftover.drain(..served).collect()
    }

    fn write(&self, handle: u32, bytes: &[u8]) {
        if handle == STDOUT || handle == STDERR {
            self.output_sender
                .lock()
                .unwrap()
                .send(bytes.to_owned())
                .unwrap();
        }
    }

    fn flt_to_le_bin(&self, value: f32) -> Vec<u8> {
        value.to_le_bytes().to_vec()
    }
}
