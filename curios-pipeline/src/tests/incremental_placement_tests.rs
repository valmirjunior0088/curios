//! One unit compiled over a baseline, where its declarations stand: a new one where the order puts it, a kept one where an edit above moved it, and each name where its module holds it across a chain of recompiles.

use {
    super::test_support::{BASE, placed},
    super::test_support::{
        assert_modules_agree, assert_stored_alike, entrypoint_of, recompile_over, reuses_body,
        unit_of,
    },
    curios_elab::DEFAULT_BUDGET,
    curios_prelude::with_prelude,
    curios_text::{RootSource, SYNTAX, UnitSource},
    curios_unit::{Predecessors, Unit},
    curios_utilities::fill,
};

/// A declaration new to the unit is stored where the order puts it, the baseline's order kept around it: `alpha` and `delta` reach nothing of the unit and stand among the declarations of depth zero by name, around `beta`, and `gamma` reaches `zeta` and stands after it, one deeper.
///
/// Mutation-checked: with a new declaration stored after every other, the unit is stored in another order than the whole compile's.
#[test]
fn a_declaration_new_to_the_unit_is_stored_where_the_order_puts_it() {
    let base = "use /std/{Nat};

pub let beta: Nat = 1;

pub let zeta: Nat = beta + 1;
";
    let edited = fill!(
        "${base}\npub let alpha: Nat = 0;\n\npub let gamma: Nat = zeta + 1;\n\npub let delta: Nat = 2;\n",
        base
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A unit compiled over a unit that was itself compiled over a baseline holds each name where its module does: its places are its baseline's with the names of what it changed placed again, a declaration added among them and one respelled, and every place agrees with the module it is of. The units are stored as the whole compiles of their texts store them.
#[test]
fn a_recompile_over_a_recompile_holds_each_name_where_its_module_does() {
    let edited = BASE.replace("n + n", "n + n + 0");
    let again = fill!("${edited}\npub let zero: Nat = 0;\n", edited);
    let baseline = unit_of(BASE);
    let first = recompile_over(&edited, &baseline).unwrap();

    let second = recompile_over(&again, &first).unwrap();

    let module = second.core();
    let fresh = curios_core::Places::of(module);
    for place in module.items.places() {
        for name in module.items.names(place) {
            assert_eq!(second.places().place(name), fresh.place(name), "{name:?}");
        }
    }
    assert_stored_alike(&unit_of(&edited), &first);
    assert_stored_alike(&unit_of(&again), &second);
}

/// A lowering numbers a declaration's universe metavariables and holes from zero, so an item inserted ahead renumbers none after it: their lowerings are kept, what each asked stands, and they are reused.
#[test]
fn inserting_a_polymorphic_item_leaves_the_items_after_it_unchanged() {
    let base = "use /std/{Nat};

pub let apply(@A: Type, f: (A) -> A, a: A) -> A = f(a);

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = apply(double, 2);
";
    let inserted = base.replace(
        "pub let apply",
        "pub let id(@A: Type, a: A) -> A = a;\n\npub let apply",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&inserted, &baseline).unwrap();

    for name in ["apply", "double", "twice"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_eq!(incremental.core().items.len(), 4);
    assert_modules_agree(unit_of(&inserted).core(), incremental.core());
    assert_stored_alike(&unit_of(&inserted), &incremental);
}

/// Every name `unit` declares held where its module holds it: the places a recompile revised are the ones its module would be placed at whole, which every later walk reads a name of the unit by.
fn assert_placed(unit: &Unit) {
    let module = unit.core();
    let fresh = curios_core::Places::of(module);
    for place in module.items.places() {
        for name in module.items.names(place) {
            assert_eq!(unit.places().place(name), fresh.place(name), "{name:?}");
        }
    }
}

/// Six declarations, a family among them: `b` reads `a`, `d` matches on `T` and reads `b`, and `e` reads `d`.
const PLACED: &str = "use /std/{Nat};

pub let a: Nat = 1;

pub let b: Nat = a + 1;

pub let c: Nat = 7;

pub induct T: pub Type
| one()
| two(n: Nat)
end

pub let d(t: T) -> Nat =
    match t
    | one() => b
    | two(n) => n
    end;

pub let e: Nat = d(T/one());
";

/// A chain of recompiles, each over the one before, holds each name where its module does and is stored as the whole compile of its text: a declaration that comes to read a deeper one, one inserted beside one renamed, one that stops reading what it read, which moves the depth of what read it, a constructor added, a declaration taken out, and one that comes to read the declaration inserted.
#[test]
fn a_chain_of_recompiles_holds_each_name_where_its_module_does() {
    let edits: [&dyn Fn(&str) -> String; 6] = [
        &|text| text.replace("c: Nat = 7", "c: Nat = b + 7"),
        &|text| {
            text.replace("pub let b", "pub let a2: Nat = 2;\n\npub let b")
                .replace("pub let e:", "pub let e2:")
        },
        &|text| text.replace("b: Nat = a + 1", "b: Nat = 3"),
        &|text| {
            text.replace("| two(n: Nat)\nend", "| two(n: Nat)\n| three()\nend")
                .replace("| two(n) => n\n", "| two(n) => n\n    | three() => 3\n")
        },
        &|text| text.replace("pub let c: Nat = b + 7;\n\n", ""),
        &|text| text.replace("d(T/one());", "d(T/one()) + a2;"),
    ];
    let mut text = PLACED.to_string();
    let mut unit = unit_of(&text);
    for edit in edits {
        let edited = edit(&text);
        assert_ne!(edited, text, "every edit changes the text");
        text = edited;

        let next = recompile_over(&text, &unit).unwrap();

        assert_placed(&next);
        assert_stored_alike(&unit_of(&text), &next);
        unit = next;
    }
}

/// Declarations written one at a time between the same two, each recompiled over the unit before, until no place is left between them and the module's items are placed apart again, each kept item standing where the baseline held another or none: every name is held where its module holds it, the unit is stored as the whole compile of its text, and a program naming a declaration kept through it compiles.
#[test]
fn a_unit_whose_items_are_placed_apart_again_keeps_every_erased_form() {
    let mut text = "use /std/{Nat};\n\npub let a: Nat = 1;\n\npub let b: Nat = 2;\n".to_string();
    let mut unit = unit_of(&text);
    for step in (0..24).rev() {
        text = text.replace(
            "pub let b",
            &format!("pub let a_{step:02}: Nat = {step};\n\npub let b"),
        );

        let next = recompile_over(&text, &unit).unwrap();

        assert_placed(&next);
        assert_stored_alike(&unit_of(&text), &next);
        unit = next;
    }
    let program = entrypoint_of(
        "use /std/{Io, Nat};\nuse /lib/{a_23};\n\nlet named: Nat = a_23 + 1;\n\nIo/pure(())",
    );

    let compiled = with_prelude(|prelude| {
        let scope = prelude.iter().copied().chain([&unit]).collect::<Vec<_>>();

        crate::compile_entrypoint(
            DEFAULT_BUDGET,
            Predecessors::over(&scope),
            &SYNTAX,
            &UnitSource::entry(&program, &RootSource::none()),
            None,
            |_| {},
        )
        .map(|_| ())
        .map_err(String::from)
    });

    assert_eq!(compiled, Ok(()));
}

/// A declaration an edit above it moves is kept, and says nothing of where it was written: the terms the baseline elaborated are placed on the lines the new text holds them at, covering the text they covered.
///
/// Mutation-checked against the anchor: measured from the start of the file, the positions land on other words, above the lines they were written on.
#[test]
fn a_kept_declaration_is_placed_where_an_edit_above_moved_it() {
    let baseline = unit_of(BASE);
    let edited = BASE.replace(
        "pub let double",
        "pub let first: Nat = 1;\n\npub let double",
    );

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(reuses_body(&baseline, &incremental, "double"));
    let before = placed(&baseline, "double");
    assert!(
        before
            .iter()
            .any(|(line, text)| (*line, text.as_str()) == (3, "n + n")),
        "the body is placed on its line: {before:?}"
    );
    assert_eq!(
        placed(&incremental, "double"),
        before
            .into_iter()
            .map(|(line, text)| (line + 2, text))
            .collect::<Vec<_>>(),
    );
}

/// A declaration whose own text changed is elaborated again, whatever it lowers to, so its terms are placed in the text it now has; and what read it is reused, the declaration having come out as it was.
#[test]
fn a_declaration_whose_text_changed_is_elaborated_again_and_placed_in_its_new_text() {
    let baseline = unit_of(BASE);
    let edited = BASE.replace("= n + n;", "=\n    n + n;");

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        reuses_body(&baseline, &incremental, "twice"),
        "a respaced declaration moved the declaration that reads it"
    );
    let placed = placed(&incremental, "double");
    assert!(
        placed
            .iter()
            .any(|(line, text)| (*line, text.as_str()) == (4, "n + n")),
        "the body is placed on its new line: {placed:?}"
    );
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}
