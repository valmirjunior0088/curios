//! The front a suite puts a host's failure before the program through: one row failing, every other answered as the mock host behind it answers, each at the row's own type, so a reply reaches the program through the adapter every host answers by.

use {
    curios_abi::{
        ChildExit, ChildHandles, Failure, FileStat, Handle, HostFront, HostOp, HostOps, Later,
        Mode, SerialFlow, SerialOp, SerialParity, StdioMode, TtySize,
    },
    curios_native::test_support::*,
    std::future,
};

/// A host failing every call of `row` with `failure`, and answering every other row as `behind` does.
pub struct Failing {
    pub behind: MockHost,
    pub row: HostOp,
    pub failure: Failure,
}

/// Each row a suite fails, its failure answered where it is the row failing and its arguments handed on to the host behind otherwise.
macro_rules! failing_rows {
    ($($row:ident: fn $name:ident($($param:ident: $type_:ty),*) -> $answer:ty;)*) => {
        $(
            fn $name(&self, $($param: $type_),*) -> Result<$answer, Failure> {
                match self.row {
                    HostOp::$row => Err(self.failure),
                    _ => self.behind.$name($($param),*),
                }
            }
        )*
    };
}

impl HostFront for Failing {
    type Behind = MockHost;

    fn behind(&self) -> &MockHost {
        &self.behind
    }

    failing_rows! {
        HandleRead: fn handle_read(h: Handle, n: u64) -> Option<Vec<u8>>;
        HandleWrite: fn handle_write(h: Handle, b: Vec<u8>) -> ();
        HandleFlush: fn handle_flush(h: Handle) -> ();
        HandleFinish: fn handle_finish(h: Handle) -> ();
        FileOpen: fn file_open(path: Vec<u8>, mode: Mode) -> Handle;
        FileStat: fn file_stat(path: Vec<u8>) -> FileStat;
        FileRemove: fn file_remove(path: Vec<u8>) -> ();
        FileRename: fn file_rename(from: Vec<u8>, to: Vec<u8>) -> ();
        DirList: fn dir_list(path: Vec<u8>) -> Vec<Vec<u8>>;
        DirCreate: fn dir_create(path: Vec<u8>, private: bool) -> ();
        DirRemove: fn dir_remove(path: Vec<u8>) -> ();
        ProcCwd: fn proc_cwd() -> Vec<u8>;
        TtyTake: fn tty_take(h: Handle, leave: Vec<u8>) -> ();
        TtySize: fn tty_size(h: Handle) -> TtySize;
        SerialOpen: fn serial_open(path: Vec<u8>, baud: u64, data_bits: u64, parity: SerialParity, stop_bits: u64, flow: SerialFlow) -> Handle;
        SerialList: fn serial_list() -> Vec<Vec<u8>>;
        SerialControl: fn serial_control(h: Handle, op: SerialOp, on: bool) -> ();
        SocketOpen: fn socket_open(addr: Vec<u8>) -> Handle;
        SocketConnect: fn socket_connect(h: Handle, addr: Vec<u8>) -> ();
        SocketFinishConnect: fn socket_finish_connect(h: Handle) -> ();
        TlsStart: fn tls_start(h: Handle, sni: Vec<u8>) -> ();
        SocketListen: fn socket_listen(addr: Vec<u8>) -> Handle;
        SocketAccept: fn socket_accept(h: Handle) -> Handle;
        TlsServerConfig: fn tls_server_config(cert: Vec<u8>, key: Vec<u8>) -> Handle;
        ProcSpawn: fn proc_spawn(argv: Vec<Vec<u8>>, cwd: Vec<u8>, env: Vec<Vec<u8>>, stdin: StdioMode, stdout: StdioMode, stderr: StdioMode) -> ChildHandles;
        ProcWait: fn proc_wait(child: Handle) -> ChildExit;
        ProcKill: fn proc_kill(child: Handle) -> ();
    }

    fn serial_drain(&self, h: Handle) -> Later<Result<(), Failure>> {
        match self.row {
            HostOp::SerialDrain => Later::new(future::ready(Err(self.failure))),
            _ => self.behind.serial_drain(h),
        }
    }

    fn dns_lookup(&self, host: Vec<u8>, port: u64) -> Later<Result<Vec<Vec<u8>>, Failure>> {
        match self.row {
            HostOp::DnsLookup => Later::new(future::ready(Err(self.failure))),
            _ => self.behind.dns_lookup(host, port),
        }
    }
}
