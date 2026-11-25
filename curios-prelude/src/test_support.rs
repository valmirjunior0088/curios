//! What the suites over the restored prelude and the bench's measurements of it share: the scope the images are certified over, and the items they hold.
//!
//! A namespace rather than a root export, for `curios-native`'s `test_support` reason: a call through `curios_prelude::test_support` says at its use site that the caller reached for scaffolding rather than product API.

use {curios_cert::Globals, curios_core::Item, curios_unit::Unit};

/// The certifier's environment for a root walked after `predecessors`: each read where it stands with the record its unit carries, as the build mounts it.
pub fn certified<'a>(predecessors: &[&'a Unit]) -> Globals<'a> {
    let mut globals = Globals::default();
    for root in predecessors {
        globals.mount(root.filed());
    }

    globals
}

/// Every item the prelude declares, across its roots — what a claim about "the prelude" is a claim about.
pub fn items<'a>(prelude: &'a [&'a Unit]) -> impl Iterator<Item = &'a Item> {
    prelude.iter().flat_map(|root| root.core().items.iter())
}
