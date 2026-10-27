//! What the native host's suites share: a wait driven to its answer, a pseudo-terminal, and the loopback endpoints and pipes they read and write.

use {
    super::*,
    crate::{Failure, Handle},
    curios_abi::event,
    rustix::pty::{OpenptFlags, grantpt, openpt, ptsname, unlockpt},
};

/// What `later` comes to, filed with `host` and waited for as a run waits for it.
pub fn come<R: Send + 'static>(host: &OsHost, later: Later<R>) -> R {
    let answer: Answer = Box::pin(async move { Box::new(later.await) as Settled });
    let settled = match host.later(answer) {
        Ok(settled) => settled,
        Err(token) => loop {
            host.wait(vec![token.clone()]).expect("the wait ends");

            if let Some(settled) = host
                .settled(token.clone())
                .expect("the token names a reply")
            {
                break settled;
            }
        },
    };

    *settled.downcast::<R>().expect("a reply settles as itself")
}

/// Whether a watch of `handle`, asking to read or to write as said, is ready now.
pub fn watched(host: &OsHost, handle: &Handle, readable: bool, writable: bool) -> bool {
    let watch = host.handle_watch(handle.clone(), readable, writable);
    let ready = host.handle_ready(vec![watch.clone()]);
    host.handle_close(watch);

    ready == [event::READY]
}

/// Whether a watch of `handle`, asking to read or to write as said, is ready within five seconds: a program's wait, beside a deadline so that a handle never readied fails its test rather than holding it.
pub fn awaited(host: &OsHost, handle: &Handle, readable: bool, writable: bool) -> bool {
    let watch = host.handle_watch(handle.clone(), readable, writable);
    let now = host.clock_mono().unwrap();
    let deadline = host.clock_deadline(now.secs + 5, now.nanos);
    host.wait(vec![watch.clone(), deadline.clone()]).unwrap();
    let ready = host.handle_ready(vec![watch.clone()]);
    host.handle_close(watch);
    host.handle_close(deadline);

    ready == [event::READY]
}

/// A listener bound and listening through the host at a loopback port the kernel picked, with the canonical `ip:port` blob `dns_lookup` would answer for it.
///
/// Bound at port zero and read back, never probed and released: a released port is free only until the next probe takes it, and the two tests that need one run in parallel, so a probe-and-release helper would have the second test bind the port the first is about to.
pub fn loopback_listener(host: &OsHost) -> (Handle, Vec<u8>) {
    let listener = host.socket_listen(b"127.0.0.1:0".to_vec()).unwrap();

    let port = match host.table.lock().unwrap().get(&listener) {
        Some(OsResource::Listener(socket)) => socket
            .local_addr()
            .expect("a bound address")
            .as_socket()
            .expect("an IP address")
            .port(),
        _ => panic!("the listener is filed as one"),
    };

    (listener, format!("127.0.0.1:{port}").into_bytes())
}

/// A connected loopback pair: a listener, a client settled through a watch and `socket_finish_connect` where the kernel made it pend, and the accepted server end.
pub fn loopback_pair(host: &OsHost) -> (Handle, Handle, Handle) {
    let (listener, blob) = loopback_listener(host);

    let client = host.socket_open(blob.clone()).unwrap();
    if host.socket_connect(client.clone(), blob) == Err(Failure::WouldBlock) {
        assert!(awaited(host, &client, false, true));
        assert_eq!(host.socket_finish_connect(client.clone()), Ok(()));
    }
    assert!(awaited(host, &listener, true, false));
    let server = host.socket_accept(listener.clone()).unwrap();

    (listener, client, server)
}

/// A pseudo-terminal: its near end, and the name its far end opens by.
pub fn pseudo_terminal() -> (OwnedFd, Vec<u8>) {
    let near = openpt(OpenptFlags::RDWR | OpenptFlags::NOCTTY).expect("a pseudo-terminal");
    grantpt(&near).expect("its far end granted");
    unlockpt(&near).expect("its far end unlocked");
    let name = ptsname(&near, Vec::new()).expect("its far end's name");

    (near, name.into_bytes())
}
