//! The program reaching outside itself: streams, files, the terminal, the process surface, the network, and the foreign declarations that bind to a host.
//!
//! Every case here runs against a scripted host, so what is asserted is the program's side of the contract rather than any real device.

mod file_tests;
mod fs_tests;
mod net_tests;
mod proc_tests;
mod stream_tests;
mod tty_tests;
