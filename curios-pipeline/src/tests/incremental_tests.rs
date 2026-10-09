//! One unit compiled over a baseline: what is elaborated again, what is reused untouched, and that the result agrees with a whole compile of the same text — its lints included, which a reused item's credits are carried into.
//!
//! Reuse is observed by allocation identity — a reused item carries the very terms the baseline holds, which no elaboration could produce twice — and agreement by the differential predicate in `test_support`. Resource verdicts are deliberately outside the predicate: a reused declaration stands on the verdict its baseline reached, under whatever budget that was, as `documentation/design/compilation/a-stored-unit-is-a-baseline-for-an-item-level-recompile.md` states.

use {
    super::test_support::{
        assert_modules_agree, assert_stored_alike, carried_variances, compile_modules,
        keeps_lowered, mounted, read_from, recompile_after, recompile_modules, recompile_over,
        reuses_body, unit_after, unit_of, unused_binders, written, written_files,
    },
    curios_core::{Enter, Item, Variance},
    curios_unit::Unit,
    std::fs,
};

/// Three items: `twice` reaches `double`, and `unrelated` reaches neither.
const BASE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);

pub let unrelated: Nat = 7;
";

#[test]
fn an_unchanged_text_reuses_every_item() {
    let baseline = unit_of(BASE);

    let again = recompile_over(BASE, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &again, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(baseline.core(), again.core());
    assert_stored_alike(&baseline, &again);
}

/// An edited body is elaborated again, and a declaration that read its signature alone is reused: what `twice` asked of `double` is answered as it was.
#[test]
fn an_edited_body_is_elaborated_again_and_what_read_its_signature_alone_is_reused() {
    let edited = BASE.replace("n + n", "n + n + 0");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        reuses_body(&baseline, &incremental, "twice"),
        "a declaration that read the signature alone was elaborated again"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// An edited body reaches the value computed over it. `twice` is elaborated from `double`'s signature alone and stays the declaration it was; what it erased to is `double(2)` computed where the unit was erased, over the erased form of `double` whole, so it is computed again, and the unit is the one a whole compile of the edited text stores.
///
/// Mutation-checked: with an erased form an evaluation ran taken to stand whatever it is now, `twice` is stored as the `4` it computed over a body that is gone.
#[test]
fn an_edited_body_reaches_the_value_computed_over_it() {
    let edited = BASE.replace("n + n", "n + n + 1");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(
        reuses_body(&baseline, &incremental, "twice"),
        "a declaration that read the signature alone was elaborated again"
    );
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// An edited body reaches the declaration that unfolded it: `two` is checked by computing `double(1)`, so what it asked of `double`'s body is answered another way, and it is elaborated again. `four` reads `two` by its signature, which came out as it was, and is reused: the edit stops where what it changed was read.
#[test]
fn an_edited_body_reaches_the_declaration_that_unfolded_it_and_stops_there() {
    let base = "use /std/{Nat, Eq};

pub let double(n: Nat) -> Nat = n + n;

pub let two: Eq()(double(1), 2) = Eq/refl();

pub let four: Eq()(double(1), 2) = two;

pub let unrelated: Nat = 7;
";
    let edited = base.replace("n + n", "n + n + 0");
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        !reuses_body(&baseline, &incremental, "two"),
        "a declaration that unfolded the edited body was reused"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A renamed parameter is a change, to its item and to every item that read its signature: a binder's name is in what is stored, the item's own and a dependent's that was handed its signature.
///
/// Mutation-checked: with a part compared up to its binders' names, the dependents are reused and the unit is stored under the old ones.
#[test]
fn a_renamed_parameter_is_elaborated_again_with_its_dependents() {
    let edited = BASE.replace(
        "double(n: Nat) -> Nat = n + n",
        "double(m: Nat) -> Nat = m + m",
    );
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "double"));
    assert!(
        !reuses_body(&baseline, &incremental, "twice"),
        "a dependent of a renamed item is re-elaborated"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A signature handed to a dependent carries its binders' names into the dependent's own term — `same(f)` is elaborated to `same(@(base: Nat, exp: Nat) -> Nat, f)` — so the dependent is stored anew when they are renamed.
#[test]
fn a_renamed_parameter_reaches_the_dependent_that_was_handed_its_signature() {
    let base = "use /std/{Nat};

pub let f(base: Nat, exp: Nat) -> Nat = base;

pub let same(@A: Type, x: A) -> A = x;

pub let g(n: Nat) -> Nat = same(f)(n, n);
";
    let edited = base.replace(
        "f(base: Nat, exp: Nat) -> Nat = base",
        "f(b: Nat, e: Nat) -> Nat = b",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "g"));
    assert_stored_alike(&unit_of(&edited), &incremental);
}

#[test]
fn a_removed_item_is_gone_and_the_rest_is_reused() {
    let edited = BASE.replace("pub let unrelated: Nat = 7;\n", "");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 2);
    assert!(reuses_body(&baseline, &incremental, "double"));
    assert!(reuses_body(&baseline, &incremental, "twice"));
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
    assert!(
        incremental.certification() == unit_of(&edited).certification(),
        "the record keeps an entry for a declaration the text no longer holds"
    );
}

/// A declaration renamed is one removed and one added: the record a recompile joins holds an entry under the new name and none under the old.
#[test]
fn a_renamed_item_leaves_no_entry_under_its_old_name() {
    let edited = BASE.replace("pub let unrelated: Nat = 7;", "pub let apart: Nat = 7;");
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(reuses_body(&baseline, &incremental, "double"));
    assert!(
        incremental.certification() == unit_of(&edited).certification(),
        "the record differs from a whole compile's"
    );
}

/// A moved declaration changes the lowering's order and nothing about any item, so everything is reused.
#[test]
fn an_item_moved_past_another_reuses_every_item() {
    let moved = "use /std/{Nat};

pub let unrelated: Nat = 7;

pub let double(n: Nat) -> Nat = n + n;

pub let twice: Nat = double(2);
";
    let baseline = unit_of(BASE);

    let incremental = recompile_over(moved, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was re-elaborated"
        );
    }
    assert_modules_agree(unit_of(moved).core(), incremental.core());
    assert_stored_alike(&unit_of(moved), &incremental);
}

/// A witness is named by what it witnesses, so one written ahead of another renames nothing: the witness after it and the declaration that resolves through that one are reused. Named by its place among its module's witnesses, the second would be another name once one is written ahead of it, and `measured`, whose term names it, would be elaborated again.
#[test]
fn a_witness_written_ahead_of_another_leaves_its_dependents_reused() {
    let base = "use /std/{Nat};

pub concept Measured(A: Type): pub Type {
    measure(A) -> Nat,
}

satisfy Measured(Nat) {
    measure(n) = n,
}

pub let measured: Nat = Measured/measure(2);
";
    let inserted = base.replace(
        "satisfy Measured(Nat)",
        "satisfy Measured(/std/Bool) {\n    measure(_) = 1,\n}\n\nsatisfy Measured(Nat)",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&inserted, &baseline).unwrap();

    assert!(
        reuses_body(&baseline, &incremental, "measured"),
        "a declaration that resolves through a witness was elaborated again when another was written ahead of it"
    );
    assert_eq!(
        incremental.core().witnesses.len(),
        2,
        "the unit declares both witnesses"
    );
    assert_modules_agree(unit_of(&inserted).core(), incremental.core());
    assert_stored_alike(&unit_of(&inserted), &incremental);
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

/// A struct literal names its declaration in the registry rather than by a variable, so an item building one asked for the declaration's entry, and is elaborated again where the entry came out another way.
#[test]
fn an_edited_struct_recompiles_the_items_that_construct_it() {
    let base = "use /std/{Nat};

pub struct Box: pub Type { value: Nat }

pub let unbox(b: Box) -> Nat = b.value;

pub let value_of_boxed: Nat = (Box { value = 1 }).value;

pub let unrelated: Nat = 7;
";
    let sealed = base.replace("Box: pub Type", "Box: Type");
    let baseline = unit_of(base);

    let incremental = recompile_over(&sealed, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "unbox"));
    assert!(
        !reuses_body(&baseline, &incremental, "value_of_boxed"),
        "an item constructing the struct was reused"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&sealed).core(), incremental.core());
    assert_stored_alike(&unit_of(&sealed), &incremental);
}

/// A type former elaborated again that comes out as it was moves nothing: `Chain` holds itself through `Wrapped`, which strict positivity unfolds, so respelling `Wrapped`'s body elaborates `Chain` again, to the former and the entry the baseline holds, and `length`, which read its signature, is reused. A former carries its entry, and what it is declared as is compared with the entry left out, an entry being a part of its own.
///
/// Mutation-checked: with a former's kind compared entry and all, `length` is elaborated again.
#[test]
fn a_type_former_elaborated_again_to_what_it_was_keeps_what_read_it() {
    let base = "use /std/{Nat, Option};

pub let Same(T: Type) -> Type = T;

pub let Wrapped(T: Type) -> Type = T;

pub struct Chain: pub Type { next: Wrapped(Option(Chain)) }

pub let length(_chain: Chain) -> Nat = 0;
";
    let edited = base.replace(
        "Wrapped(T: Type) -> Type = T",
        "Wrapped(T: Type) -> Type = Same(T)",
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "Wrapped"));
    assert!(
        !reuses_body(&baseline, &incremental, "Chain"),
        "the control stopped holding: a former that unfolded the edited body was reused"
    );
    assert!(
        reuses_body(&baseline, &incremental, "length"),
        "a declaration that read a former elaborated again to what it was was elaborated again"
    );
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A field's mark is part of its declaration: hiding a field changes what a literal of the structure writes and what a position counts. An edit that moves the mark and nothing else therefore recompiles the items that read the structure, against the declaration as it now stands.
#[test]
fn an_edit_that_moves_a_fields_mark_alone_recompiles_the_items_that_read_the_struct() {
    let base = "use /std/{Nat};
use /std/Bool/{Holds};

pub struct Positive: pub Type { n: Nat, ok: Holds(0 < n) }

pub let n_of(p: Positive) -> Nat = p.n;

pub let unrelated: Nat = 7;
";
    let hidden = base.replace("ok: Holds", "@ok: Holds");
    let baseline = unit_of(base);

    let incremental = recompile_over(&hidden, &baseline).unwrap();

    assert!(
        !reuses_body(&baseline, &incremental, "n_of"),
        "an item reading the struct was reused"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&hidden).core(), incremental.core());
    assert_stored_alike(&unit_of(&hidden), &incremental);
}

// A family's variance composes through the families it holds, so an edit that turns one of their levels invariant moves the vector of every family that reaches it. Such a family mentions the edited declaration, so it is elaborated again with the items that read its entry, and the recompile carries the vector a whole compile does.
#[test]
fn an_edit_that_moves_a_familys_variance_recompiles_the_families_that_hold_it() {
    let base = "use /std/{Nat, Eq};

pub induct Wrap(A: Type): pub Type
| wrap(a: A)
end

pub induct Held(A: Type): pub Type
| held(w: Wrap(A))
end

pub let reader(@A: Type, h: Held(A)) -> Held(A) = h;

pub let unrelated: Nat = 7;
";
    // `Eq()(A, A)` holds `A` as a value of its own `Type`, so `A`'s level reaches a `Type` and is invariant in `Wrap`. An instance of `Held` then holds the level of the `Wrap` in its payload, which is `Held`'s to its callers beside the level its parameter is typed at: two levels where the baseline's `Held`, holding none, takes one.
    let pinned = base.replace("| wrap(a: A)", "| wrap(a: A, same: Eq()(A, A))");
    let baseline = unit_of(base);

    let incremental = recompile_over(&pinned, &baseline).unwrap();

    assert!(!reuses_body(&baseline, &incremental, "reader"));
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_eq!(carried_variances(&baseline, "Held"), [Variance::Irrelevant]);
    assert_eq!(
        carried_variances(&incremental, "Held"),
        [Variance::Irrelevant, Variance::Invariant]
    );
    assert_modules_agree(unit_of(&pinned).core(), incremental.core());
    assert_stored_alike(&unit_of(&pinned), &incremental);
}

/// A baseline from unrelated text keeps nothing, and the recompile is then a whole compile by another route.
#[test]
fn a_baseline_of_another_text_keeps_nothing_and_equals_the_whole_compile() {
    let baseline = unit_of("use /std/{Nat};\n\npub let other: Nat = 1;\n");

    let incremental = recompile_over(BASE, &baseline).unwrap();

    assert_eq!(incremental.core().items.len(), 3);
    assert_modules_agree(unit_of(BASE).core(), incremental.core());
    assert_stored_alike(&unit_of(BASE), &incremental);
}

/// An item the parser could not read withholds its dependents, which report nothing and leave no refusal behind them — so a unit compiled over a baseline holds declarations its walk made no item for. It leaves them out, exactly as a whole compile of the same text does, and answers with the parse error either way.
#[test]
fn a_broken_item_withholds_its_dependents_and_reports_as_the_whole_compile_does() {
    let baseline = unit_of(BASE);
    let (_guard, edited) = written("lib", &BASE.replace("n + n", ""));

    let Err(incremental) = recompile_modules(&edited, &baseline) else {
        panic!("a unit holding a broken item does not compile");
    };
    let Err(whole) = compile_modules(&edited) else {
        panic!("a unit holding a broken item does not compile");
    };

    assert!(
        incremental.contains("expected a term"),
        "the parse error is the answer: {incremental}"
    );
    assert_eq!(
        incremental, whole,
        "a recompile answers what the whole compile of the same text answers"
    );
}

/// A hypothesis only a proof the elaborator writes reads, in `below`, beside one nothing reads, in `idle`.
const CREDITED: &str = "use /std/{Nat};
use /std/Bool/{True, Holds};

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = True/proved();

pub let idle(i: Nat, q: Holds(i < 3)) -> Nat = i;

pub let unrelated: Nat = 7;
";

#[test]
fn a_binder_a_reused_items_proof_reads_stays_credited_as_in_a_whole_compile() {
    let baseline = unit_of(CREDITED);
    assert_eq!(
        unused_binders(&baseline),
        ["unused binder `q`; name it `_q` to keep it"],
        "`p` is read by the proof `proved` stands for, and `q` by nothing"
    );

    let edited = CREDITED.replace("= 7", "= 8");
    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(reuses_body(&baseline, &incremental, "below"));
    assert_eq!(
        unused_binders(&incremental),
        unused_binders(&unit_of(&edited))
    );
}

/// A hypothesis a proof reads in `below`, and one in `above` under it, whose statement reaches `limit`.
const CREDITED_TWICE: &str = "use /std/{Nat};
use /std/Bool/{True, Holds};

pub let limit: Nat = 7;

pub let below(i: Nat, n: Nat, p: Holds(i < n)) -> Holds(i <= n) = True/proved();

pub let above(i: Nat, q: Holds(limit < i)) -> Holds(limit <= i) = True/proved();
";

/// A unit's credited binders are stored alike whichever pass credited them.
///
/// A recompile credits the binders of what it elaborates again and then the ones the baseline's proofs read in what it reused, where a whole compile credits in reading order: `above` is elaborated again and `below`, over it in the text, is reused, so the two passes run in the other order than the text. Mutation-checked: kept as a list, the two units' text parts differ.
#[test]
fn credited_binders_are_stored_alike_whichever_pass_credited_them() {
    let baseline = unit_of(CREDITED_TWICE);
    let edited = CREDITED_TWICE.replace("= 7", "= 8");

    let incremental = recompile_over(&edited, &baseline).unwrap();
    let whole = unit_of(&edited);

    assert!(reuses_body(&baseline, &incremental, "below"));
    assert!(!reuses_body(&baseline, &incremental, "above"));
    assert!(unused_binders(&incremental).is_empty());
    assert!(
        curios_archive::to_bytes(incremental.text()).unwrap()[..]
            == curios_archive::to_bytes(whole.text()).unwrap()[..],
        "the text stage's part is stored differently over a baseline"
    );
}

/// Where the terms of the definition `unit` calls `name` were written, as `unit`'s own text places them: each position's line with the text it covers, in order and once each.
fn placed(unit: &Unit, name: &str) -> Vec<(usize, String)> {
    let definition = unit
        .core()
        .items
        .iter()
        .find_map(|item| match item {
            Item::Let(definition) if definition.name.symbol().ends_with(&format!("/{name}")) => {
                Some(definition)
            }
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} is a let item of the unit"));
    let mut extents = Vec::new();
    for term in [&definition.type_, &definition.body] {
        term.walk(
            &mut extents,
            |extents, term| {
                extents.extend(term.extent());
                Enter::Descend
            },
            |_, _, _| (),
        );
    }

    let mut placed = extents
        .iter()
        .map(|extent| {
            let span = unit
                .text()
                .anchors()
                .resolve(extent)
                .expect("a declaration of the unit is sited");

            (
                span.line_column().0,
                span.source.text[span.start..span.end].to_string(),
            )
        })
        .collect::<Vec<_>>();
    placed.sort();
    placed.dedup();

    placed
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

/// A witness declared where none stood reaches the declaration that asked: `boxed` checks a call at `Box(Nat)`, so it asked whether `Box` is a monad's head, to embed the call where it is, and was told no. It is elaborated again once the unit declares `Monad(Box)`, and a declaration that never asked is reused.
#[test]
fn a_witness_declared_where_none_stood_reaches_the_declaration_that_asked() {
    let base = "use /std/{Monad, Nat};

pub struct Box(A: Type): pub Type { value: A }

pub let make(n: Nat) -> Box(Nat) = Box { value = n };

pub let boxed: Box(Nat) = make(1);

pub let unrelated: Nat = 7;
";
    let witnessed = format!(
        "{base}\nsatisfy Monad(Box) {{ pure(@_, a) = Box {{ value = a }}, bind(@_, @_, m, f) = f(m.value) }}\n"
    );
    let baseline = unit_of(base);

    let incremental = recompile_over(&witnessed, &baseline).unwrap();

    assert!(
        !reuses_body(&baseline, &incremental, "boxed"),
        "a declaration told no witness stood at a key was reused once one did"
    );
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(unit_of(&witnessed).core(), incremental.core());
    assert_stored_alike(&unit_of(&witnessed), &incremental);
}

/// Two modules each declaring `x`, a declaration reading the one a `use` above it brings in, and one that reads neither.
const SHADOWED: &str = "pub mod a
    pub let x: /std/Nat = 1;
end

pub mod b
    pub let x: /std/Nat = 2;
end

use /lib/a/{x};

pub let y: /std/Nat = x;

pub let z: /std/Nat = 3;
";

/// A declaration's lowered form is the baseline's where its text stands and every name it resolved resolves as it did, and is lowered again where one does not: an edit to the `use` above `y` touches no character of `y`, and the `x` it names is another declaration.
///
/// Mutation-checked: kept by its text alone, `y` goes on naming `/lib/a/x`, and the unit is not the one a whole compile stores.
#[test]
fn a_declaration_is_lowered_again_where_a_name_it_wrote_resolves_elsewhere() {
    let edited = SHADOWED.replace("use /lib/a/{x};", "use /lib/b/{x};");
    let baseline = unit_of(SHADOWED);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    assert!(
        !keeps_lowered(&baseline, &incremental, "y"),
        "a declaration whose name resolves elsewhere kept the lowering that named the other"
    );
    assert!(
        keeps_lowered(&baseline, &incremental, "z"),
        "a declaration that read nothing the edit moved was lowered again"
    );
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// A declaration written between two others is lowered, and the ones around it are not: nothing a declaration lowers to says where it stands among the others.
#[test]
fn a_declaration_written_between_two_others_leaves_their_lowerings_kept() {
    let edited = BASE.replace(
        "pub let twice",
        "pub let thrice: Nat = double(3);\n\npub let twice",
    );
    let baseline = unit_of(BASE);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    for name in ["double", "twice", "unrelated"] {
        assert!(
            keeps_lowered(&baseline, &incremental, name),
            "{name} was lowered again"
        );
    }
    assert_modules_agree(unit_of(&edited).core(), incremental.core());
    assert_stored_alike(&unit_of(&edited), &incremental);
}

/// `edited` compiled over `baseline` answers what compiling it whole answers: the same unit, stored alike, or the same refusal.
fn answers_as_the_whole_compile_does(edited: &str, baseline: &Unit) {
    let whole = compile_modules(&mounted("lib", edited));
    let incremental = recompile_over(edited, baseline);

    match (whole, incremental) {
        (Ok(whole), Ok(incremental)) => {
            assert_modules_agree(whole.core(), incremental.core());
            assert_stored_alike(&whole, &incremental);
        }
        (Err(whole), Err(incremental)) => assert_eq!(
            incremental, whole,
            "a recompile refuses as the whole compile of the same text refuses"
        ),
        (whole, incremental) => panic!(
            "one compiles and the other does not: whole {:?}, over the baseline {:?}",
            whole.map(|_| ()),
            incremental.map(|_| ())
        ),
    }
}

/// A module a glob imports gains a member under a name the scope already holds: `y` wrote that name and no character of it changed, and what the recompile makes of it is what the whole compile does.
#[test]
fn a_globbed_module_gaining_a_member_is_answered_as_the_whole_compile_answers() {
    let base = "use /std/{Nat};

pub mod a
    pub let one: /std/Nat = 1;
end

pub mod b
    pub let two: /std/Nat = 2;
end

use /lib/b/{two};
use /lib/a/*;

pub let y: Nat = one + two;
";
    let edited = base.replace(
        "    pub let one: /std/Nat = 1;\n",
        "    pub let one: /std/Nat = 1;\n    pub let two: /std/Nat = 22;\n",
    );
    let baseline = unit_of(base);

    answers_as_the_whole_compile_does(&edited, &baseline);
}

/// An export moves from one module to another, and a declaration that wrote the path through the first is untouched: its lowering is not kept, and the refusal is the whole compile's.
#[test]
fn a_path_through_an_export_that_moved_is_refused_as_the_whole_compile_refuses_it() {
    let base = "pub mod a
    pub let x: /std/Nat = 1;
end

pub mod b
    pub use /lib/a/{x};

    pub let w: /std/Nat = 0;
end

pub mod c
    pub let v: /std/Nat = 0;
end

pub let y: /std/Nat = /lib/b/x;
";
    let edited = base.replace("    pub use /lib/a/{x};\n\n", "").replace(
        "    pub let v: /std/Nat = 0;\n",
        "    pub use /lib/a/{x};\n\n    pub let v: /std/Nat = 0;\n",
    );
    let baseline = unit_of(base);

    assert!(
        recompile_over(&edited, &baseline).is_err(),
        "a path through an export that is gone still resolved"
    );
    answers_as_the_whole_compile_does(&edited, &baseline);
}

/// A library of five files: `a` reads `x` through the export `b` makes of `c`'s, beside a declaration that reads nothing and an import nothing reads.
const FILES: [(&str, &str); 5] = [
    (
        "lib.crs",
        "pub mod a;\npub mod b;\npub mod c;\npub mod d;\n",
    ),
    (
        "a.crs",
        "use /std/{Nat, Bool};\n\npub let y: Nat = /lib/b/x;\n\npub let z: Nat = 3;\n",
    ),
    ("b.crs", "pub use /lib/c/{x};\n"),
    ("c.crs", "pub let x: /std/Nat = 1;\n"),
    ("d.crs", "pub let x: /std/Nat = 2;\n"),
];

/// A file no edit touched is read as the text the baseline read and is not parsed, until a declaration of it is lowered again: `y` wrote a path through an export another file moved. The baseline is a stored unit restored, which holds what each file declared in a source of its own, and the unit is stored as the whole compile of the edited files stores it — the standing file's lint with it, placed in the file as it was read.
#[test]
fn a_standing_files_declaration_is_lowered_again_where_another_files_edit_moves_a_name_it_wrote() {
    let (directory, modules) = written_files("standing-file", &FILES);
    let compiled = compile_modules(&modules).expect("the unit compiles");
    let baseline = Unit::restored(&compiled.stored().expect("a unit serializes"))
        .expect("a stored unit restores");

    fs::write(directory.join("b.crs"), "pub use /lib/d/{x};\n").expect("a fixture is rewritten");
    let incremental = recompile_modules(&read_from("lib", &directory), &baseline).unwrap();
    let whole = compile_modules(&read_from("lib", &directory)).unwrap();

    assert!(
        !keeps_lowered(&baseline, &incremental, "y"),
        "a declaration whose name resolves elsewhere kept the lowering that named the other"
    );
    assert!(
        keeps_lowered(&baseline, &incremental, "z"),
        "a declaration that read nothing the edit moved was lowered again"
    );
    let placed = incremental
        .text()
        .lints()
        .iter()
        .map(|lint| {
            let span = lint.report.span.as_ref().expect("a lint is placed");

            span.source.path.clone().expect("in a file")
        })
        .collect::<Vec<_>>();
    assert_eq!(
        placed,
        [directory.join("a.crs")],
        "the import nothing reads is reported in the file it is written in"
    );
    assert_modules_agree(whole.core(), incremental.core());
    assert_stored_alike(&whole, &incremental);
}

/// A unit `/dep` and a unit `/lib` after it: `twice` and `bound` read a signature of `/dep`, `two` unfolds `double`, and `unrelated` reads nothing of it.
const DEPENDENCY: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let limit: Nat = 4;
";

const DEPENDENT: &str = "use /std/{Nat, Eq};
use /dep/{double, limit};

pub let twice: Nat = double(2);

pub let two: Eq()(double(1), 2) = Eq/refl();

pub let bound: Nat = limit;

pub let unrelated: Nat = 7;
";

/// `/lib` compiled over its baseline after `/dep` was edited to `edited`, beside the baseline and a whole compile after the same `/dep`.
fn after_an_edited_dependency(edited: &str) -> (Unit, Unit, Unit) {
    let dependency = unit_after("dep", DEPENDENCY, &[]);
    let baseline = unit_after("lib", DEPENDENT, &[&dependency]);
    let moved = unit_after("dep", edited, &[]);

    let incremental = recompile_after("lib", DEPENDENT, &baseline, &[&moved], &[true]);
    let whole = unit_after("lib", DEPENDENT, &[&moved]);

    (baseline, incremental, whole)
}

/// A read of another unit stands where the part read is the part that was read, whatever else of the unit moved. `/dep`'s `double` is respelled under its signature, so `twice`, which read the signature, is reused; `two`, which unfolded the body, is elaborated again; and `bound` and `unrelated`, which read nothing that moved, are reused. The unit is the one a whole compile after the edited `/dep` stores, what each stage recorded of `/dep` with it.
///
/// Mutation-checked both ways: with every read of a unit that moved taken to stand, `two` is reused over a body that is gone; with none taken to stand, `twice` is elaborated again.
#[test]
fn a_dependency_respelled_under_its_signature_redoes_what_unfolded_it_and_no_other() {
    let (baseline, incremental, whole) =
        after_an_edited_dependency(&DEPENDENCY.replace("n + n", "n + n + 0"));

    assert!(
        reuses_body(&baseline, &incremental, "twice"),
        "a declaration that read a signature that stands was elaborated again"
    );
    assert!(
        !reuses_body(&baseline, &incremental, "two"),
        "a declaration that unfolded a body that moved was reused"
    );
    assert!(reuses_body(&baseline, &incremental, "bound"));
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(whole.core(), incremental.core());
    assert_stored_alike(&whole, &incremental);
}

/// A signature of another unit that moved reaches what read it: `double`'s parameter is renamed, a binder's name being in what a dependent is handed, so `twice` and `two` are elaborated again, and `bound`, which read `limit` alone, is reused.
#[test]
fn a_dependency_whose_signature_moved_redoes_what_read_it() {
    let (baseline, incremental, whole) = after_an_edited_dependency(&DEPENDENCY.replace(
        "double(n: Nat) -> Nat = n + n",
        "double(m: Nat) -> Nat = m + m",
    ));

    assert!(
        !reuses_body(&baseline, &incremental, "twice"),
        "a declaration that read a signature that moved was reused"
    );
    assert!(!reuses_body(&baseline, &incremental, "two"));
    assert!(reuses_body(&baseline, &incremental, "bound"));
    assert!(reuses_body(&baseline, &incremental, "unrelated"));
    assert_modules_agree(whole.core(), incremental.core());
    assert_stored_alike(&whole, &incremental);
}

/// A unit that moved in nothing its dependent read moves nothing of the dependent: `/dep` gains a declaration, and every declaration of `/lib` is reused.
#[test]
fn a_dependency_that_gained_a_declaration_redoes_nothing_of_its_dependent() {
    let (baseline, incremental, whole) =
        after_an_edited_dependency(&format!("{DEPENDENCY}\npub let spare: Nat = 9;\n"));

    for name in ["twice", "two", "bound", "unrelated"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was elaborated again"
        );
    }
    assert_modules_agree(whole.core(), incremental.core());
    assert_stored_alike(&whole, &incremental);
}

/// A value computed where its unit is erased ran the erased forms of what it reaches in other units too, each recorded by the digest it had. `/dep`'s `double` is given another body under its signature, so `/lib`'s `twice` is elaborated once and what it erased to is computed again over the `double` that is there now: the unit is the one a whole compile after the edited `/dep` stores.
///
/// Mutation-checked: with every erased form of a unit that moved taken to be the one an evaluation ran, `twice` is stored as the `4` it computed over the `/dep` that is gone.
#[test]
fn a_dependency_given_another_body_reaches_the_value_computed_over_it() {
    let dependency = "use /std/{Nat};\n\npub let double(n: Nat) -> Nat = n + n;\n";
    let dependent = "use /std/{Nat};\nuse /dep/{double};\n\npub let twice: Nat = double(2);\n";
    let before = unit_after("dep", dependency, &[]);
    let baseline = unit_after("lib", dependent, &[&before]);
    let moved = unit_after("dep", &dependency.replace("n + n", "n + n + 1"), &[]);

    let incremental = recompile_after("lib", dependent, &baseline, &[&moved], &[true]);

    assert!(
        reuses_body(&baseline, &incremental, "twice"),
        "a declaration that read a signature that stands was elaborated again"
    );
    assert_stored_alike(&unit_after("lib", dependent, &[&moved]), &incremental);
}
