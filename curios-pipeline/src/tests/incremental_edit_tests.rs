//! One unit compiled over a baseline, what an edit reaches: what is elaborated or lowered again, where that stops, and what is reused untouched.
//!
//! Reuse is observed by allocation identity: a reused item carries the very terms the baseline holds, which no elaboration could produce twice.

use {
    super::test_support::BASE,
    super::test_support::{
        assert_modules_agree, assert_stored_alike, carried_variances, keeps_lowered,
        recompile_over, reuses_body, unit_of,
    },
    curios_core::Variance,
    curios_utilities::fill,
};

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

/// An edited value reaches a value computed over one computed over it: `b` is computed from `a` where the unit is erased, and `c` from `b`, so `a` respelled is erased again, `b` with it, and `c`, which ran what `b` erased to, with that. Each reads the one before it by its signature alone, so each is the declaration it was.
///
/// Mutation-checked: with the items that read an item erased again marked and theirs not, `c` is stored as the value it computed over `b`'s old form.
#[test]
fn an_edited_value_reaches_a_value_computed_over_one_computed_over_it() {
    let base = "use /std/{Nat};

pub let a: Nat = 1;

pub let b: Nat = a + 1;

pub let c: Nat = b + 1;

pub let unrelated: Nat = 7;
";
    let edited = base.replace("a: Nat = 1", "a: Nat = 2");
    let baseline = unit_of(base);

    let incremental = recompile_over(&edited, &baseline).unwrap();

    for name in ["b", "c", "unrelated"] {
        assert!(
            reuses_body(&baseline, &incremental, name),
            "{name} was elaborated again"
        );
    }
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

/// A witness declared where none stood reaches the declaration that asked: `boxed` checks a call at `Box(Nat)`, so it asked whether `Box` is a monad's head, to embed the call where it is, and was told no. It is elaborated again once the unit declares `Monad(Box)`, and a declaration that never asked is reused.
#[test]
fn a_witness_declared_where_none_stood_reaches_the_declaration_that_asked() {
    let base = "use /std/{Monad, Nat};

pub struct Box(A: Type): pub Type { value: A }

pub let make(n: Nat) -> Box(Nat) = Box { value = n };

pub let boxed: Box(Nat) = make(1);

pub let unrelated: Nat = 7;
";
    let witnessed = fill!(
        "${base}\nsatisfy Monad(Box) { pure(@_, a) = Box { value = a }, bind(@_, @_, m, f) = f(m.value) }\n",
        base
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
