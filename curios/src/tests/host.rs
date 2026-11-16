//! The program reaching outside itself: streams, files, the terminal, serial ports, the process surface, the network, and the foreign declarations that bind to a host.
//!
//! Every case here runs against a scripted host, so what is asserted is the program's side of the contract rather than any real device — except `os`, the few programs that run against the real host to prove the scheduler's wait path against a certifier-backed descriptor.

mod boundary;
mod description;
mod file;
mod fs;
mod http;
mod net;
mod os;
mod outcome;
mod plugin;
mod proc;
mod program;
mod reader;
mod serial;
mod stream;
mod suspension;
mod tty;
