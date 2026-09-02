//! The program reaching outside itself: streams, files, the terminal, the process surface, the network, and the foreign declarations that bind to a host.
//!
//! Every case here runs against a scripted host, so what is asserted is the program's side of the contract rather than any real device — except `os_tests`, the few programs that run against the real host to prove the scheduler's wait path against a kernel-backed descriptor.

mod file_tests;
mod fs_tests;
mod net_tests;
mod os_tests;
mod proc_tests;
mod stream_tests;
mod tty_tests;
