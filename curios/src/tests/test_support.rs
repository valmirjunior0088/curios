//! A host that answers one row with a scripted reply and every other with a [`MockHost`]'s: how one call of a row fails where the mock's script only succeeds.

use {
    curios_abi::{
        ChildExit, ChildHandles, Failure, FileStat, Handle, HostOp, HostOps, Mode, Polled, Refusal,
        SerialFlow, SerialOp, SerialParity, StdioMode, Termination, Timestamp, TtySize,
        for_each_host_op,
    },
    curios_runtime::MockHost,
    std::{any::Any, sync::Mutex},
};

/// A [`MockHost`] with one lie in it: one call to the lying row answers the scripted reply, and every other call the mock's.
pub(super) struct Lying {
    inner: MockHost,
    lie: Mutex<Option<Lie>>,
}

/// A lie not told yet: the row, how many of its calls the mock still answers first, and the reply.
struct Lie {
    op: HostOp,
    honest: usize,
    reply: Box<dyn Any + Send>,
}

impl Lying {
    /// `inner`, except that the first call to `op` answers `reply`, which must be `op`'s reply type.
    pub(super) fn new<R: Send + 'static>(inner: MockHost, op: HostOp, reply: R) -> Self {
        Self::after(inner, op, 0, reply)
    }

    /// `inner`, except that the call to `op` after its first `honest` answers `reply`, which must be `op`'s reply type.
    pub(super) fn after<R: Send + 'static>(
        inner: MockHost,
        op: HostOp,
        honest: usize,
        reply: R,
    ) -> Self {
        Self {
            inner,
            lie: Mutex::new(Some(Lie {
                op,
                honest,
                reply: Box::new(reply),
            })),
        }
    }

    /// The lie, when `op` is the lying row, its honest calls are spent and it has not been told yet.
    fn lie<R: 'static>(&self, op: HostOp) -> Option<R> {
        let mut lie = self.lie.lock().unwrap();

        match lie.take() {
            Some(told) if told.op == op && told.honest == 0 => Some(
                *told
                    .reply
                    .downcast::<R>()
                    .unwrap_or_else(|_| panic!("the lie is not {op:?}'s reply")),
            ),
            Some(mut untold) => {
                if untold.op == op {
                    untold.honest -= 1;
                }
                *lie = Some(untold);

                None
            }
            None => None,
        }
    }
}

/// Every method asks for the lie first and falls back to the mock's answer.
macro_rules! declare_lying_host {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty { $($contract:tt)* }
    )*) => {
        impl HostOps for Lying {
            $(
                fn $name(&self, $($p: $t),*) -> $r {
                    match self.lie::<$r>(HostOp::$variant) {
                        Some(reply) => reply,
                        None => self.inner.$name($($p),*),
                    }
                }
            )*
        }
    };
}

for_each_host_op!(declare_lying_host);
