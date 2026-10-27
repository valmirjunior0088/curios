//! What `curios wonder` does at the command line and over the wire — the two transports, exercised as their consumers reach them: a process with arguments and standard input, and a process spoken to in the language server protocol.
//!
//! The engine's own behaviour — which records a program yields — is covered beside it in `wonder/tests.rs`; these decide what the transports do with them: that an answer is stdout and exit 0 whatever it says, that a file is placed in its unit, and that the server publishes the same records where the editor is looking and clears them when they go.

mod cli_tests;
mod server_tests;
mod test_support;
