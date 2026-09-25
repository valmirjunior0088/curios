//! A host that answers one row with a scripted reply and every other with a [`MockHost`]'s: how a host that breaks its contract is put in front of the adapter, since no honest host does.

use {
    curios_abi::{
        ChildExit, ChildStream, Failure, FileStat, Handle, HostOp, HostOps, Mode, Poll, Refusal,
        SerialFlow, SerialOp, SerialParity, StdioMode, Termination, Timestamp, TtySize,
        for_each_host_op,
    },
    curios_runtime::MockHost,
    std::{any::Any, sync::Mutex},
};

/// A [`MockHost`] with one lie in it: the first call to the lying row answers the scripted reply, and every other call the mock's.
pub(super) struct Lying {
    inner: MockHost,
    lie: Mutex<Option<(HostOp, Box<dyn Any + Send>)>>,
}

impl Lying {
    /// `inner`, except that the first call to `op` answers `reply`, which must be `op`'s reply type.
    pub(super) fn new<R: Send + 'static>(inner: MockHost, op: HostOp, reply: R) -> Self {
        Self {
            inner,
            lie: Mutex::new(Some((op, Box::new(reply)))),
        }
    }

    /// The lie, when `op` is the lying row and it has not been told yet.
    fn lie<R: 'static>(&self, op: HostOp) -> Option<R> {
        let mut lie = self.lie.lock().unwrap();

        match lie.take() {
            Some((lying, reply)) if lying == op => Some(
                *reply
                    .downcast::<R>()
                    .unwrap_or_else(|_| panic!("the lie is not {op:?}'s reply")),
            ),
            untold => {
                *lie = untold;

                None
            }
        }
    }
}

/// Every method asks for the lie first and falls back to the mock's answer.
macro_rules! declare_lying_host {
    ($(
        $(#[doc = $doc:literal])*
        $variant:ident: fn $name:ident($($p:ident: $t:ty),* $(,)?) -> $r:ty as $subject:ident / $label:ident { $($contract:tt)* }
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
