//! What the scripted host's suites share: a wait driven to its answer, a watch on a handle, and a child spawned with its streams.

use {
    super::MockHost,
    crate::{Answer, ChildHandles, Handle, Later, Settled, StdioMode},
    curios_abi::{HostOps, event},
};

/// What `later` comes to, filed with `host` and waited for as a run waits for it.
pub fn come<R: Send + 'static>(host: &MockHost, later: Later<R>) -> R {
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

/// Whether a watch of `handle`, asking to read or to write as said, is ready now; the watch is closed again.
pub fn watched(host: &MockHost, handle: &Handle, readable: bool, writable: bool) -> bool {
    let watch = host.handle_watch(handle.clone(), readable, writable);
    let ready = host.handle_ready(vec![watch.clone()]);
    host.handle_close(watch);

    ready == [event::READY]
}

/// Spawn `program` with its standard output piped and the rest inherited.
pub fn spawned(host: &MockHost, program: &[u8]) -> ChildHandles {
    host.proc_spawn(
        vec![program.to_vec()],
        vec![],
        vec![],
        StdioMode::Inherit,
        StdioMode::Pipe,
        StdioMode::Inherit,
    )
    .unwrap()
}
