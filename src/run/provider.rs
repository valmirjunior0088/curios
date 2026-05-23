use std::sync::{
    Arc, Mutex,
    mpsc::{self, Receiver},
};

pub trait Provider {
    fn print(&self, bytes: &[u8]);
}

pub struct StdoutProvider;

impl Provider for StdoutProvider {
    fn print(&self, bytes: &[u8]) {
        std::io::Write::write_all(&mut std::io::stdout(), bytes).unwrap();
    }
}

pub struct ChannelProvider {
    sender: Arc<Mutex<mpsc::Sender<Vec<u8>>>>,
}

impl ChannelProvider {
    pub fn new() -> (Self, Receiver<Vec<u8>>) {
        let (sender, receiver) = mpsc::channel();

        (
            ChannelProvider {
                sender: Arc::new(Mutex::new(sender)),
            },
            receiver,
        )
    }
}

impl Provider for ChannelProvider {
    fn print(&self, bytes: &[u8]) {
        self.sender.lock().unwrap().send(bytes.to_vec()).unwrap();
    }
}
