//! What the suites of `into_cps` share: the modules they lower and the readings of what comes out.

use {crate::*, curios_num::Natural};

pub fn nat(builder: &mut Builder, value: u32) -> Atom {
    let constant = builder.constant(Constant::Nat(Natural::from(value)));
    Atom::Constant(constant)
}

/// Every completed lowering has already passed `curios_cps::Module::verify` inside `lower_to_cps`; the shape assertions on top ask the graph.
///
/// They ask the graph rather than the printed text: a spelling belongs to the printer, so an assertion over it is a claim about how a Rust variant is rendered, which a printer change can turn quietly vacuous rather than failing. The printed module is still what a failure reports, because that is what a reader needs in front of them.
pub fn lowered(module: &Module) -> curios_cps::Module {
    lower_to_cps(module)
}

pub fn has_node(
    module: &curios_cps::Module,
    predicate: impl Fn(&curios_cps::Node) -> bool,
) -> bool {
    module.nodes().iter().flatten().any(predicate)
}
