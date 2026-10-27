//! One unit compiled over a baseline after an edit elsewhere: another file of the unit, or a dependency respelled, moved or given another body, and what the unit records of it.

use {
    super::test_support::{
        assert_modules_agree, assert_stored_alike, compile_modules, keeps_lowered, read_from,
        recompile_after, recompile_modules, restored, reuses_body, unit_after, written_files,
    },
    curios_core::{Module, Zonked},
    curios_elab::DEFAULT_BUDGET,
    curios_prelude::with_prelude,
    curios_text::SYNTAX,
    curios_unit::{Predecessors, Unit},
    std::fs,
};

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
    let baseline = restored(&compiled);

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

/// A file no edit reached keeps every lowering it holds without asking any, and still says what its declarations read: `d` respelled moves no binding anything reads, so `y` and `z` are kept unasked, the import of `Nat` they read is used, the import of `Bool` nothing reads is the one finding, and the unit is stored as the whole compile stores it.
///
/// Mutation-checked: with what the declarations kept unasked read not marking the imports they read, `Nat`'s import is reported as unused.
#[test]
fn a_file_no_edit_reached_keeps_what_its_declarations_read_without_asking_them() {
    let (directory, modules) = written_files("unreached-file", &FILES);
    let compiled = compile_modules(&modules).expect("the unit compiles");
    let baseline = restored(&compiled);

    fs::write(directory.join("d.crs"), "pub let x: /std/Nat = 3;\n")
        .expect("a fixture is rewritten");
    let incremental = recompile_modules(&read_from("lib", &directory), &baseline).unwrap();
    let whole = compile_modules(&read_from("lib", &directory)).unwrap();

    for name in ["y", "z"] {
        assert!(
            keeps_lowered(&baseline, &incremental, name),
            "{name} was lowered again"
        );
    }
    assert_eq!(
        incremental.text().lints().len(),
        1,
        "the import nothing reads is the one finding"
    );
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

/// What a unit records of another unit's parts follows what its declarations read now: `bound` stops reading `/dep`'s `limit`, which nothing else read, and `unrelated` starts reading `double`'s body, which `two` read already, so the record of `limit` is taken out and the one of `double` stands, as a whole compile of the edited text records them.
///
/// Mutation-checked: with a part no declaration reads any more left recorded, the unit records `limit`, and is not the one a whole compile stores.
#[test]
fn what_a_unit_records_of_another_follows_what_its_declarations_read_now() {
    let dependency = unit_after("dep", DEPENDENCY, &[]);
    let baseline = unit_after("lib", DEPENDENT, &[&dependency]);
    let edited = DEPENDENT
        .replace("pub let bound: Nat = limit;", "pub let bound: Nat = 4;")
        .replace(
            "pub let unrelated: Nat = 7;",
            "pub let unrelated: Eq()(double(2), 4) = Eq/refl();",
        );

    let incremental = recompile_after("lib", &edited, &baseline, &[&dependency], &[false]);

    assert!(reuses_body(&baseline, &incremental, "twice"));
    assert_stored_alike(&unit_after("lib", &edited, &[&dependency]), &incremental);
}

/// What [`UNTAKEN`] holds of another unit: `f` names `/dep`'s `spin` in the arm no reduction takes, and `spin` is made total in `/dep`.
#[test]
fn a_definition_partial_only_through_another_units_name_in_an_untaken_arm_turns_total_with_it() {
    let dependency = "use /std/{Nat};\n\npub let spin(n: Nat) -> Nat = spin(n);\n";
    let dependent = "use /std/{Nat};
use /dep/{spin};

pub let f: Nat =
    match true
    | true => 1
    | false => spin(0)
    end;
";
    let before = unit_after("dep", dependency, &[]);
    let baseline = unit_after("lib", dependent, &[&before]);
    let moved = unit_after("dep", &dependency.replace("= spin(n);", "= n;"), &[]);

    let incremental = recompile_after("lib", dependent, &baseline, &[&moved], &[true]);

    assert_stored_alike(&unit_after("lib", dependent, &[&moved]), &incremental);
}

/// `/dep` holds `Wrap`, whose parameter is as positive as `Arrow`, which it holds, takes its own.
const ARROW: &str = "pub induct Arrow(T: Type): pub Type
| arrow(t: T)
end

pub induct Wrap(T: Type): pub Type
| wrap(a: Arrow(T))
end
";

/// `/lib` ties `Knot` through `/dep`'s `Wrap`.
const KNOT: &str = "use /dep/{Wrap};

pub induct Knot: pub Type
| knot(w: Wrap(Knot))
end
";

/// `module`, elaborated already, put to the kernel alone with the prelude and then `units` in scope.
fn certified_after(module: &Module, units: &[&Unit]) -> Vec<curios_cert::Verdict> {
    let module = Zonked::project(module).expect("an elaborated module is zonked");

    with_prelude(|prelude| {
        let scope = prelude.iter().chain(units).copied().collect::<Vec<_>>();

        let globals = crate::globals(Predecessors::over(&scope));

        curios_cert::certify_module(&module, DEFAULT_BUDGET, &globals, SYNTAX)
            .0
            .verdicts
    })
}

/// A family whose parameter turns negative only through a family it holds is composed through at the vector a whole compile files: `Arrow` respelled to take `T` to a function's domain leaves `Wrap`'s terms the baseline's, and `Knot`, tied through `Wrap` and admitted while `Arrow` held `T` as a payload, is refused by a kernel reading the recompiled `/dep`.
///
/// Mutation-checked: with every signature read taken to stand over a baseline, `Wrap` keeps the baseline's vector and the kernel admits `Knot`.
#[test]
fn a_family_turned_negative_through_one_it_holds_is_composed_through_as_a_whole_compile_files_it() {
    let negative = ARROW.replace("| arrow(t: T)", "| arrow(f: (T) -> /std/Nat)");
    let baseline = unit_after("dep", ARROW, &[]);
    let recompiled = recompile_after("dep", &negative, &baseline, &[], &[]);
    let knot = unit_after("lib", KNOT, &[&baseline]);

    assert_eq!(certified_after(knot.core(), &[&baseline]), []);
    let verdicts = certified_after(knot.core(), &[&recompiled]);
    assert!(
        matches!(
            verdicts.as_slice(),
            [verdict] if matches!(verdict.error, curios_cert::Error::NotPositive { .. })
        ),
        "a family tied through a recompiled one that turned negative was admitted: {verdicts:?}"
    );
    assert_stored_alike(&unit_after("dep", &negative, &[]), &recompiled);
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
