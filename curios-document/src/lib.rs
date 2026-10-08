//! The documentation record and its pages. [`Documentation`] is what a unit's interface looks like to its consumers — one record per exposed module, each declaration's head printed as written with every name in it resolved, and the prose attached to each — made by `curios-text` where a command asks for one, from the unit its compilation built, and carried by no unit. [`write_documentation`] is the one renderer over it: the static pages `curios document` writes, from Askama templates compiled into this crate. The README says why the two share a crate and where everything else about documentation lives.

mod record;
pub use record::*;

mod pages;
pub use pages::*;
