use std::{
    io::{BufRead, Write, stdin, stdout},
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
    fn read(&self) -> Vec<u8>;

    fn print(&self, bytes: &[u8]);
}

pub struct StdioHost;

impl Host for StdioHost {
    fn read(&self) -> Vec<u8> {
        let mut line = String::new();

        match stdin().lock().read_line(&mut line) {
            Ok(0) => vec![],
            Ok(_) => line.into_bytes(),
            Err(_) => vec![],
        }
    }

    fn print(&self, bytes: &[u8]) {
        stdout().write_all(bytes).unwrap();
    }
}

pub struct ChannelHost {
    input_receiver: Mutex<Receiver<Vec<u8>>>,
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
    fn read(&self) -> Vec<u8> {
        match self.input_receiver.lock().unwrap().recv() {
            Ok(line) => line.into_iter().chain([b'\n']).collect(),
            Err(RecvError) => vec![],
        }
    }

    fn print(&self, bytes: &[u8]) {
        self.output_sender
            .lock()
            .unwrap()
            .send(bytes.to_owned())
            .unwrap();
    }
}
