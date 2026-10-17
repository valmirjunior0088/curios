//! A unit elaborates to one module however far its declarations' binder counters had run when they started: an order the elaborator puts atoms in, and a spelling it writes from that order, read an identity by its age and never by its value.

use {
    super::test_support::{elaborated_above, mounted},
    curios_prelude::with_prelude,
    curios_text::UnitSource,
    curios_unit::Predecessors,
};

/// How far above its lowering each declaration's binders are counted from in the second elaboration.
const RAISED: u64 = 1_000;

/// The magnitude of a product, case by case: each arm's `Eq/refl()` solves the equality's implicits through the products of the two binders, spelled in the order the fold puts their factors in. `/std`'s own declarations are held so by `std_identity_census`.
///
/// Mutation-checked: with a rank read off each identity's value, the implicits are spelled apart.
#[test]
fn a_solution_through_a_product_is_spelled_alike_from_any_floor() {
    let modules = mounted(
        "lib",
        "use /std/{Int, Eq};

pub let magnitude_of_product(a: Int, b: Int) -> Eq()(Int/abs(a * b), Int/abs(a) * Int/abs(b)) =
    match Int/view(a)
    | nonneg(_) => match Int/view(b) | nonneg(_) => Eq/refl() | neg(_) => Eq/refl() end
    | neg(_) => match Int/view(b) | nonneg(_) => Eq/refl() | neg(_) => Eq/refl() end
    end;",
    );
    with_prelude(|prelude| {
        let source = UnitSource::mounted(&modules);
        let elaborated = |floor| elaborated_above(&source, Predecessors::over(prelude), floor);
        assert!(elaborated(0) == elaborated(RAISED));
    });
}
