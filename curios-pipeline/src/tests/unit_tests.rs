//! Mounts and prefixes across compilation units, the orphan rule that fires between them, and the bytes a unit stores whatever came before it and whichever compilation made it.

use {
    super::test_support::*,
    curios_core::{Global, Module},
    curios_prelude::with_prelude,
    curios_text::{PreparedText, RootSource, spell},
    curios_unit::Unit,
    curios_utilities::Qualifier,
    std::rc::Rc,
};

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

/// A mount wins over the entry's empty prefix, which is the whole of what longest match decides, a prefix being one segment: `/json/answer` is the mounted unit's, not a name the entry failed to declare.
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
/// Either half alone proves nothing. The refusal could come from a malformed fixture; the acceptance could come from a rule that never runs. Only together do they say the boundary is where coherence is enforced.
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

/// A value whose own initializer reads it is forced by the first declaration that reads it, whichever unit that declaration is in: the unit declaring it compiles, since nothing in it reads the value, and a unit that reads it is refused where it is compiled, at the value's own line, before any program links the two.
///
/// The control is the same pair with the read left out: a refusal that fired for it would be the declaring unit's, or the fixture's.
#[test]
fn a_unit_that_reads_another_units_self_reading_value_is_refused_where_it_is_compiled() {
    let knot = mounted("knot", "pub let xs : /std/Nat = /std/Nat/add(xs, 1);");
    let reader = mounted("reader", "pub let read() -> /std/Nat = /knot/xs;");
    let idle = mounted("reader", "pub let read() -> /std/Nat = 0;");

    let refusal = compile_each(&[&knot, &reader])
        .err()
        .expect("a unit may not read a value that reads itself");
    assert!(
        refusal.contains("evaluates itself"),
        "expected the recursion rule's refusal, got: {refusal}"
    );
    assert!(
        refusal.contains("pub let xs : /std/Nat = /std/Nat/add(xs, 1);"),
        "expected the refusal framed at the value's own line, got: {refusal}"
    );

    compile_each(&[&knot, &idle]).expect("a unit that reads no such value compiles");
}

/// A unit's stored bytes do not change with what was compiled before it.
///
/// `/lib` is compiled twice: with only the prelude beneath it, and after `/pre`, which mints binders and universe metavariables of its own. Nothing a unit mints resumes above a predecessor's count, so the lowered and the elaborated module serialize to the same bytes either way, and the lowering hands elaboration the same counts; and an item erases into a fragment of its own, so its erased forms do too. What legitimately moves is left out: the spellings a unit may write for its scope's names, which are its scope's interface.
#[test]
fn a_units_stored_bytes_do_not_depend_on_what_was_compiled_before_it() {
    let pre = mounted(
        "pre",
        "pub let pick(@A: Type, a: A) -> A = a;\npub let one: /std/Nat = pick(1);",
    );
    let lib = mounted(
        "lib",
        "pub let same(@A: Type, a: A) -> A = a;\npub let three: /std/Nat = same(3);",
    );
    let last = |sources: &[&RootSource]| {
        compile_in_order(sources)
            .pop()
            .expect("a unit was compiled")
    };
    let alone = last(&[&lib]);
    let after = last(&[&pre, &lib]);
    let bytes = |module: &Module| {
        curios_archive::to_bytes(module)
            .expect("a module serializes")
            .to_vec()
    };

    assert!(
        bytes(alone.text().core()) == bytes(after.text().core()),
        "the lowered module moved with its predecessors"
    );
    assert!(
        bytes(alone.core()) == bytes(after.core()),
        "the elaborated module moved with its predecessors"
    );
    assert_eq!(alone.text().minted(), after.text().minted());
    assert!(
        curios_archive::to_bytes(alone.erased()).expect("an erased unit serializes")[..]
            == curios_archive::to_bytes(after.erased()).expect("an erased unit serializes")[..],
        "the erased forms moved with its predecessors"
    );
}

/// The texts of the prelude's roots and then of `earlier`: what a unit compiled after them was lowered against.
fn scope_of<'a>(prelude: &[&'a Unit], earlier: &'a [Unit]) -> Vec<&'a PreparedText> {
    let prelude = prelude.iter().map(|root| root.text());

    prelude.chain(earlier.iter().map(Unit::text)).collect()
}

/// The paths a unit gives its names are listed once for the unit, and every report over it reads the one list: two units compiled after `/lib` are handed the very table the first of them to ask had made.
#[test]
fn a_units_paths_are_listed_once_for_every_report_over_it() {
    let lib = mounted("lib", "pub let x: /std/Nat = 1;");
    let one = mounted("one", "pub let a: /std/Nat = /lib/x;");
    let two = mounted("two", "pub let b: /std/Nat = /lib/x;");
    let units = compile_in_order(&[&lib, &one, &two]);

    with_prelude(|prelude| {
        let first = units[1].text().spellings(&scope_of(prelude, &units[..1]));
        let second = units[2].text().spellings(&scope_of(prelude, &units[..2]));

        for place in 0..=prelude.len() {
            assert!(
                Rc::ptr_eq(&first.units[place].paths, &second.units[place].paths),
                "the unit at {place} listed its paths again for a second reader"
            );
        }
    });
}

/// A name one unit declares and a later one exports is written by the path either gives it, read in one order whichever unit the reader stands in: the facade's own module reaches it by its label, a unit after both by the facade's path, the shorter, and a unit that may not name the facade by the declaring unit's alone.
#[test]
fn a_name_a_later_unit_exports_is_spelled_by_either_units_path() {
    let lib = mounted("lib", "pub mod deep\n    pub let x: /std/Nat = 1;\nend");
    let facade = mounted("facade", "pub use /lib/deep/{x};");
    let reader = mounted("reader", "pub let read: /std/Nat = /facade/x;");
    let units = compile_in_order(&[&lib, &facade, &reader]);
    let x = Global::Authored(Qualifier::from(["lib", "deep", "x"]));

    with_prelude(|prelude| {
        let inside = units[1].text().spellings(&scope_of(prelude, &units[..1]));
        assert_eq!(
            spell(&inside, &Qualifier::from(["facade"]), None, &x).as_deref(),
            Some("x")
        );

        let after = units[2].text().spellings(&scope_of(prelude, &units[..2]));
        assert_eq!(
            spell(&after, &Qualifier::from(["reader"]), None, &x).as_deref(),
            Some("/facade/x")
        );

        let mut closed = after.clone();
        closed.closed.push(Qualifier::from(["facade"]));
        assert_eq!(
            spell(&closed, &Qualifier::from(["reader"]), None, &x).as_deref(),
            Some("/lib/deep/x")
        );
    });
}

/// A name exported along several paths is listed the same by every compilation.
///
/// `/lib/a/x` is written four ways, its own path and three re-exports, and the lowering reaches the modules exporting it through tables it walks in a hash's order. The paths a report is handed for a name are listed its own first and then by path, so sixteen compilations hand over one list; in the order they were reached, two would agree one time in six.
#[test]
fn a_name_exported_along_several_paths_is_listed_the_same_every_time() {
    let source = "pub mod a\n    pub let x: /std/Nat = 1;\nend\npub mod b\n    pub use /lib/a/{x};\nend\npub mod c\n    pub use /lib/a/{x};\nend\npub mod d\n    pub use /lib/a/{x};\nend";

    for _ in 0..16 {
        let unit = unit_of(source);
        let spellings = with_prelude(|prelude| {
            let scope = prelude.iter().map(|root| root.text()).collect::<Vec<_>>();
            unit.text().spellings(&scope)
        });
        let own = spellings.units.last().expect("the unit's own paths");
        let written = own
            .paths
            .values()
            .find(|written| written.len() > 1)
            .expect("a name written several ways")
            .iter()
            .map(|written| written.path.join())
            .collect::<Vec<_>>();

        assert_eq!(written, ["/lib/a/x", "/lib/b/x", "/lib/c/x", "/lib/d/x"]);
    }
}
