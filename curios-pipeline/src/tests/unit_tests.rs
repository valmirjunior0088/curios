//! Mounts and prefixes across compilation units, and the orphan rule that fires between them.

use super::test_support::*;

/// Two ordinary units mounting one prefix is refused where the registry knows both, naming the prefix.
#[test]
fn two_units_claiming_one_prefix_is_diagnosed() {
    let error = compile_with_units(
        &[
            ("dup", "pub let a : /std/Nat = 1;"),
            ("dup", "pub let b : /std/Nat = 2;"),
        ],
        "0",
    )
    .expect_err("one prefix cannot belong to two units");

    assert!(
        error.contains("dup") && error.contains("exactly one unit"),
        "unexpected error: {error}"
    );
}

/// A unit mounted at `/lib` and an entry declaring its own `mod lib` are the same collision seen from the other side, and the entry loses: the mount was there first.
///
/// This is also what keeps `ffi` import names disjoint. A row's name is its declaration's fully qualified name, so `/lib/…` from a mounted unit and `/lib/…` from an entry module are the one shape that could collide in a namespace neither owns — and it is refused here rather than at the link.
#[test]
fn an_entry_module_colliding_with_a_mount_is_diagnosed() {
    let error = compile_with_units(
        &[("lib", "pub let a : /std/Nat = 1;")],
        "mod lib\n    pub let b : /std/Nat = 2;\nend\n/std/print(/std/Nat/to_str(0))",
    )
    .expect_err("an entry cannot declare a module a unit already mounts");

    assert!(error.contains("lib"), "unexpected error: {error}");
}

/// A mount wins over the entry's empty prefix, which is the whole of what longest match decides now that a prefix is one segment: `/json/answer` is the mounted unit's, not a name the entry failed to declare.
#[test]
fn a_mount_wins_over_the_entrys_empty_prefix() {
    compile_with_units(
        &[("json", "pub let answer : /std/Nat = 42;")],
        "/std/print(/std/Nat/to_str(/json/answer))",
    )
    .expect("a mounted prefix resolves");
}

/// Two mounted units are both reachable from one entry: the compilation root holds a child per mount, so neither shadows the other and the second does not replace the first.
#[test]
fn two_mounts_are_both_reachable() {
    compile_with_units(
        &[
            ("json", "pub let a : /std/Nat = 1;"),
            ("http", "pub let b : /std/Nat = 2;"),
        ],
        "/std/print(/std/Nat/to_str(/std/Nat/add(/json/a, /http/b)))",
    )
    .expect("two mounted packages");
}

/// An entry's `mod json` beside a mount at `/json` makes `/json/a` two answers, so the claim is refused naming both claimants. A prefix is one segment, so this is the only shape a collision has — but it is still decided here rather than surfacing later as an ordinary duplicate declaration, which would name the label without naming what else claimed it.
#[test]
fn a_prefix_claimed_twice_is_refused() {
    let error = compile_with_units(
        &[("json", "pub let a : /std/Nat = 1;")],
        "mod json\n    pub let b : /std/Nat = 2;\nend\n/std/print(/std/Nat/to_str(0))",
    )
    .expect_err("an entry cannot declare a module a mount already claims");

    assert!(
        error.contains("mod json") && error.contains("/json"),
        "unexpected error: {error}"
    );
}

/// A unit's names resolve from the entry, which is the whole point of mounting one.
#[test]
fn an_entry_reaches_a_mounted_units_public_name() {
    compile_with_units(
        &[("lib", "pub let answer : /std/Nat = 42;")],
        "/std/print(/std/Nat/to_str(/lib/answer))",
    )
    .expect("a mounted unit's public name resolves from the entry");
}

/// The unit boundary **is** semantic, and this pair is what says so.
///
/// A witness declared in ordinary unit `A` for a concept declared in ordinary unit `B` over a type declared in ordinary unit `C` is an orphan: no unit involved owns the pair, and two unrelated authors could otherwise each `satisfy` it and collide unfixably once both are linked. Written as modules of one unit the same three declarations are accepted, because one unit owns all of them.
///
/// Either half alone proves nothing. The refusal could come from a malformed fixture; the acceptance could come from a rule that never runs. Only together do they say the boundary is where coherence is enforced — which is why the earlier claim that N units compile identically to N modules was exactly backwards.
#[test]
fn the_orphan_rule_fires_across_units_and_not_across_modules() {
    let concept = "pub concept Show(A: Type): pub Type {\n    show(A) -> /std/Nat,\n}";
    let type_ = "pub struct Widget: pub Type {\n    tag: /std/Nat,\n}";
    let witness = "satisfy /b/Show(/c/Widget) {\n    show = (w) => 0,\n}";

    let across = compile_with_units(
        &[("b", concept), ("c", type_), ("a", witness)],
        "/std/print(/std/Nat/to_str(0))",
    )
    .expect_err("a third unit may not satisfy another's concept at another's type");
    assert!(
        across.contains("orphan"),
        "expected an orphan refusal, got: {across}"
    );

    // The control. One unit owning all three is the sanctioned shape, and it must still compile — otherwise the refusal above would only show that the fixture is broken.
    let together = format!(
        "pub mod b\n{concept}\nend\npub mod c\n{type_}\nend\npub mod a\nsatisfy /one/b/Show(/one/c/Widget) {{\n    show = (w) => 0,\n}}\nend"
    );
    compile_with_units(&[("one", &together)], "/std/print(/std/Nat/to_str(0))")
        .expect("one unit may satisfy its own concept at its own type");
}
