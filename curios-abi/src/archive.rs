//! How a validated host-import namespace is archived: as its one-byte code.
//!
//! The namespace is one of a closed pair, so archiving the string would store a length and up to three bytes to say what a single byte says exactly. The roster below is the only place the codes are assigned, and both directions read it.

use {
    curios_archive::{Proxy, Via},
    std::borrow::Borrow,
};

/// The archived form of a namespace: `sys` is 0, `ffi` is 1.
pub struct NamespaceCode;

impl Proxy<&'static str> for NamespaceCode {
    type Archivable = u8;

    /// Panics on an unknown namespace, which is not a validation step but a restatement of one: a `ForeignFunction` cannot be constructed with a namespace outside the pair, so reaching here with one means the constructor's own check was bypassed.
    fn to_archivable(namespace: &&'static str) -> impl Borrow<u8> {
        match *namespace {
            "sys" => 0,
            "ffi" => 1,
            other => panic!("foreign namespace `{other}` is validated at construction"),
        }
    }

    /// Fallible in the direction that reads bytes: an archive is a file, and a file can say 7.
    fn from_archivable(code: u8) -> Result<&'static str, String> {
        match code {
            0 => Ok("sys"),
            1 => Ok("ffi"),
            invalid => Err(format!("invalid foreign namespace code {invalid}")),
        }
    }
}

/// The field adapter: `#[archived_with(crate::Namespace)]`.
pub type Namespace = Via<NamespaceCode>;
