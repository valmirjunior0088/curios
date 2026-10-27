//! What the verifier's suites share: a `Nat` atom.

use {crate::*, curios_num::Natural};

pub fn nat_atom(builder: &mut Builder, value: u32) -> Atom {
    let constant = builder.constant(Constant::Nat(Natural::from(value)));
    Atom::Constant(constant)
}
