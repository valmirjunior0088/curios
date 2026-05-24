use std::{
    collections::VecDeque,
    io::{BufRead, Write, stdin, stdout},
    sync::{
        Arc, Mutex,
        mpsc::{self, Receiver, Sender},
    },
};

pub trait Provider {
    fn print(&self, bytes: &[u8]);
    fn read(&self) -> Vec<u8>;
}

pub struct StdioProvider;

impl Provider for StdioProvider {
    fn print(&self, bytes: &[u8]) {
        stdout().write_all(bytes).unwrap();
    }

    fn read(&self) -> Vec<u8> {
        let mut line = String::new();

        match stdin().lock().read_line(&mut line) {
            Ok(0) => vec![],
            Ok(_) => line.into_bytes(),
            Err(_) => vec![],
        }
    }
}

pub struct ChannelProvider {
    sender: Arc<Mutex<Sender<Vec<u8>>>>,
    input: Arc<Mutex<VecDeque<Vec<u8>>>>,
}

impl ChannelProvider {
    pub fn out() -> (Self, Receiver<Vec<u8>>) {
        let (sender, receiver) = mpsc::channel();
        (
            ChannelProvider {
                sender: Arc::new(Mutex::new(sender)),
                input: Arc::new(Mutex::new(VecDeque::new())),
            },
            receiver,
        )
    }

    pub fn io(lines: Vec<Vec<u8>>) -> (Self, Receiver<Vec<u8>>) {
        let (sender, receiver) = mpsc::channel();
        (
            ChannelProvider {
                sender: Arc::new(Mutex::new(sender)),
                input: Arc::new(Mutex::new(VecDeque::from(lines))),
            },
            receiver,
        )
    }
}

impl Provider for ChannelProvider {
    fn print(&self, bytes: &[u8]) {
        self.sender.lock().unwrap().send(bytes.to_vec()).unwrap();
    }

    fn read(&self) -> Vec<u8> {
        self.input.lock().unwrap().pop_front().unwrap_or_default()
    }
}
