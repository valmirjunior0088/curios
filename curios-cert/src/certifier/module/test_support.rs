//! Fixtures the module suites share: the certifier they start from, and the families, structures and names they declare against it, each written in Curios/Core.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across this module, and nothing outside it.

use {
    crate::{test_support::*, *},
    curios_analysis::test_support::*,
    curios_common::{Qualifier, fill},
    curios_core::{Global, InductDecl, StructDecl, Term},
};

pub fn certifier() -> Certifier<'static> {
    Certifier::new(100_000, SYNTAX)
}

/// The name [`family`] registers its declaration under.
pub fn fam() -> Global {
    Global::Authored(Qualifier::from(["Fam"]))
}

/// `Fam : ${sort} | mk(x : ${payload}) end`: one family of one case and no parameter.
const FAMILY: &str = r#"
induct /Fam: pub ${sort}
| mk(x: ${payload})
end
"#;

/// [`FAMILY`] at `result_sort` over `payload_type`, registered and returned for checking.
pub fn family(certifier: &mut Certifier, result_sort: &str, payload_type: &str) -> InductDecl {
    declared(
        certifier,
        &fill!(FAMILY, sort = result_sort, payload = payload_type),
    );

    certifier
        .induct_decl(&fam())
        .expect("the family is declared")
        .clone()
}

/// `(A : Prop) -> A`: a proposition at level one, since it quantifies over the ground propositions.
pub const LARGE_PROPOSITION: &str = "(A: Prop) -> A";

/// `Fam : ${outer} | mk(A : ${inner}, f : (A) -> Fam) end`, with `outer` a level above `inner`. A family holding a value of any type of `inner` — Russell's `set` at `Type`, the paradox of trees' `node` at `Prop` — takes this shape, and the two sorts a level apart are what make the *declaration* stand: the payload fits under the family. The paradox is the self-application a caller builds from it ([`HOLDING_ITSELF`]), not the declaration.
const HOLDING_A_TYPE: &str = r#"
induct /Fam: pub ${outer}
| mk(A: ${inner}, f: (x: A) -> #{node = InductType} /Fam())
end
"#;

/// [`HOLDING_A_TYPE`] at `outer` over `inner`, registered and returned for checking.
pub fn family_holding_a_type(certifier: &mut Certifier, outer: &str, inner: &str) -> InductDecl {
    declared(certifier, &fill!(HOLDING_A_TYPE, outer, inner));

    certifier
        .induct_decl(&fam())
        .expect("the family is declared")
        .clone()
}

/// `mk(Fam, (x) => x)` — the family's own type handed to the field that must sit below it. Russell's and the paradox of trees' self-application both take this shape, and the identity closes the forward field whatever the first argument's fate.
pub const HOLDING_ITSELF: &str =
    "#{node = Variant} /Fam/mk(#{node = InductType} /Fam(), (x: #{node = InductType} /Fam()) => x)";

/// A `Prop`-sorted family named `path` with no constructors, so that a field typed by it is a genuine proof rather than a proposition held as data: declared, and the text of its type.
pub fn proposition(certifier: &mut Certifier, path: &str) -> String {
    declared(certifier, &fill!("induct /${path}: pub Prop\nend", path));

    fill!("#{node = InductType} /${path}()", path)
}

/// A `Prop`-sorted structure with one field of `field_type` and no parameters, registered and returned for checking.
pub fn proposition_with_field(certifier: &mut Certifier, field_type: &str) -> StructDecl {
    struct_with_field(certifier, "Prop", field_type)
}

/// `Bad : ${sort} { value : ${field} }`: one structure of one field and no parameter.
const STRUCTURE: &str = r#"
struct /Bad: pub ${sort} {
    value: ${field},
}
"#;

/// [`STRUCTURE`] at `result_sort` with its field at `field_type`, registered and returned for checking.
pub fn struct_with_field(
    certifier: &mut Certifier,
    result_sort: &str,
    field_type: &str,
) -> StructDecl {
    declared(
        certifier,
        &fill!(STRUCTURE, sort = result_sort, field = field_type),
    );

    certifier
        .struct_decl(&Global::Authored(Qualifier::from(["Bad"])))
        .expect("the structure is declared")
        .clone()
}

/// The name the definitions below are checked under.
pub fn lemma() -> Global {
    Global::Authored(Qualifier::from(["lemma"]))
}

/// The family-type term [`family`] declares.
pub fn fam_type() -> Term {
    term("#{node = InductType} /Fam()")
}
