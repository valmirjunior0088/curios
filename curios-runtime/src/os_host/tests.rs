use super::*;

/// Every entry in the handle table is one [`OsResource`], so the enum's size is what a plain file or an unconnected socket costs to hold. Boxing the two TLS variants took that from 1176 bytes to 16, measured 2026-08-23 by the `size_of` calls below — a `rustls` connection carries its record buffers inline, and unboxed it set the size of every other kind.
///
/// The bound is the guard rather than the figure: unboxing either variant, or adding a third large one inline, puts a kilobyte back on every handle and fails here.
#[test]
fn a_table_entry_does_not_carry_a_tls_connection_inline() {
    assert!(
        size_of::<OsResource>() <= 64,
        "OsResource is {} bytes; the TLS streams are {} and {}",
        size_of::<OsResource>(),
        size_of::<StreamOwned<ClientConnection, Socket>>(),
        size_of::<StreamOwned<ServerConnection, Socket>>(),
    );
}

/// The standard streams are never in the handle table, so a setter that asked the table would call them closed. `Async/read`/`write` begin with `set_nonblocking`, so that verdict would fail every asynchronous use of stdio on the real host while the mock — which answers `Ok` for any handle — lets the suite pass.
#[test]
fn a_standard_stream_takes_the_socket_setters_like_a_file() {
    let host = OsHost::with_args(vec![]);

    for handle in [Handle::Stdin, Handle::Stdout, Handle::Stderr] {
        assert!(matches!(
            host.set_nonblocking(handle.clone(), 1),
            Status::Ok
        ));
        assert!(matches!(
            host.set_recv_timeout(handle.clone(), 10),
            Status::Ok
        ));
        assert!(matches!(host.set_reuseaddr(handle, 1), Status::Ok));
    }
}
