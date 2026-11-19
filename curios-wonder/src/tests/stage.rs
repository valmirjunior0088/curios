//! What `wonder --stage` answers: a rung the driver reached is an answer whatever stopped the program after it, and a rung it never reached is a refusal carrying what stopped it; beside `--name`, the declaration's part of a rung a unit has and nothing else.

use {
    crate::*,
    curios_common::Qualifier,
    curios_common::Source,
    curios_elab::DEFAULT_BUDGET,
    curios_module::{Overlay, RootSource},
    curios_surface::WrittenIn,
    std::path::Path,
};

/// One rung of `program`, asked for the way the one-shot transport asks: what the asker was handed, beside what the query answered.
fn rung(name: &str, text: &str) -> Result<(String, crate::Rendering), crate::Refusal> {
    let mut handed = String::new();
    let reached = crate::stage(
        DEFAULT_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                text: text.to_string(),
                written: WrittenIn::Program,
            },
            declares: None,
            package: None,
        },
        &Overlay::default(),
        None,
        name,
        |rung| handed.push_str(rung),
        &mut |_| {},
    );

    reached.map(|rendering| (handed, rendering))
}

const REFUSED_LATER: &str = "use /std/{Nat};\nlet x : Nat = true;\n/std/print(\"ok\\n\")";

/// A rung the driver emitted is an answer, and a failure *after* it does not unmake one. Asking for `core` on a program that fails to elaborate is asking what the lowering produced, which is most of the reason to ask at all.
#[test]
fn a_rung_reached_before_the_failure_is_still_answered() {
    for name in ["text", "core"] {
        let (handed, rendering) = rung(name, REFUSED_LATER)
            .unwrap_or_else(|_| panic!("{name} was observed before the refusal"));
        assert!(!handed.is_empty(), "{name} rendered nothing");
        assert!(
            !rendering.diagnostics.is_empty(),
            "{name} lost the diagnostics that stopped the compilation"
        );
    }
}

/// The other side of the same boundary: a rung the program never reached has not been answered, and stays a refusal. `anf` sits past elaboration, which is where this program stops.
#[test]
fn a_rung_the_program_never_reached_is_refused() {
    let refusal = rung("anf", REFUSED_LATER).expect_err("anf is never reached");
    assert!(
        matches!(refusal, crate::Refusal::Diagnostics(diagnostics) if !diagnostics.is_empty()),
        "expected the refusal to carry what stopped the program"
    );
}

/// A program that compiles answers with nothing beside the rendering, so the added channel stays empty on the ordinary path.
#[test]
fn a_rung_of_a_program_that_compiles_carries_no_diagnostics() {
    let (_, rendering) = rung("core", "/std/print(\"hi\\n\")").expect("core is reached");
    assert!(rendering.diagnostics.is_empty());
}

/// A library has the rungs a unit has, each reprinting what the unit holds of it, and no rung past them: from `anf` on a program's entry roots what is rendered.
#[test]
fn a_library_is_reprinted_at_a_units_rungs_and_no_other() {
    const LIBRARY: &str = "use /std/{Nat};\npub let answer: Nat = 42;\n";
    let file = Path::new("answers.crs");
    let overlay = Overlay::of([(file.to_path_buf(), LIBRARY.to_string())]);
    let rung = |name: &str| {
        let library = RootSource::supplied_module("answers", &Source::held("answers.crs", LIBRARY))
            .expect("the text is written as a module");

        crate::unit_stage(
            DEFAULT_BUDGET,
            vec![library],
            file,
            &overlay,
            None,
            name,
            &mut |_| {},
        )
    };

    for name in ["text", "core", "core-elab"] {
        let handed = rung(name).unwrap_or_else(|_| panic!("{name} is a unit's rung"));
        assert!(handed.contains("answer"), "{name} reprinted {handed:?}");
    }
    assert!(matches!(rung("anf"), Err(crate::Refusal::Unit)));
}

/// A library's `core` rung is its lowering, made over the units before it: the library is lowered and compiled nowhere, and what is printed is the lowering, its marks of what nothing has checked yet among it.
#[test]
fn a_librarys_core_rung_lowers_it_and_compiles_nothing() {
    const LIBRARY: &str = "use /std/{Nat};\npub let answer: Nat = 40 + 2;\n";
    let file = Path::new("answers.crs");
    let overlay = Overlay::of([(file.to_path_buf(), LIBRARY.to_string())]);
    let library = RootSource::supplied_module("answers", &Source::held("answers.crs", LIBRARY))
        .expect("the text is written as a module");
    let mut compiled = Vec::new();

    let printed = crate::unit_stage(
        DEFAULT_BUDGET,
        vec![library],
        file,
        &overlay,
        None,
        "core",
        &mut |progress| {
            if let curios_pipeline::Progress::Compiling(prefix) = progress {
                compiled.push(prefix.join());
            }
        },
    )
    .unwrap_or_else(|_| panic!("core is a unit's rung"));

    assert!(compiled.is_empty(), "{compiled:?}");
    assert!(printed.contains("answer"), "{printed}");
    assert!(printed.contains("#{node = Infix}"), "{printed}");
}

/// A program of two declarations, `twice` holding a lambda and `other` beside it.
const TWO: &str = "use /std/{Nat};\n\nlet twice(n: Nat) -> Nat =\n    let step = (k: Nat) => k + n;\n    step(step(n));\n\nlet other(n: Nat) -> Nat = n + 1;\n\n/std/print(\"ok\\n\")\n";

/// The declaration at `path`'s part of the rung `name` of `program`, asked the way the one-shot transport asks.
fn part(name: &str, program: &str, path: &[&str]) -> Result<String, crate::Refusal> {
    crate::named_stage(
        DEFAULT_BUDGET,
        Subject::Entry {
            units: Vec::new(),
            origin: Origin::Text {
                text: program.to_string(),
                written: WrittenIn::Program,
            },
            declares: None,
            package: None,
        },
        &Qualifier::from(path.to_vec()),
        name,
        &Overlay::default(),
        None,
        &mut |_| {},
    )
    .map(|described| described.expect("the path names a declaration").1)
}

/// At each of a unit's rungs, the program's own declaration is printed and its neighbour is not.
#[test]
fn a_programs_own_declaration_is_its_part_of_each_rung_a_unit_has() {
    for name in ["text", "core", "core-elab"] {
        let printed = part(name, TWO, &["twice"]).unwrap_or_else(|_| panic!("{name} is answered"));

        assert!(printed.contains("twice"), "{name}: {printed}");
        assert!(!printed.contains("other"), "{name}: {printed}");
    }
}

/// A library's own declaration answers as a program's does, off the library's lowering and its elaborated module.
#[test]
fn a_librarys_own_declaration_is_its_part_of_each_rung_a_unit_has() {
    const LIBRARY: &str = "use /std/{Nat};\npub let answer: Nat = 42;\npub let other: Nat = 7;\n";
    for name in ["text", "core", "core-elab"] {
        let library = RootSource::supplied_module("answers", &Source::held("answers.crs", LIBRARY))
            .expect("the text is written as a module");
        let printed = crate::named_stage(
            DEFAULT_BUDGET,
            Subject::Unit {
                units: vec![library],
            },
            &Qualifier::from(["answers", "answer"]),
            name,
            &Overlay::default(),
            None,
            &mut |_| {},
        )
        .unwrap_or_else(|_| panic!("{name} is answered"))
        .expect("the path names a declaration")
        .1;

        assert!(printed.contains("answer"), "{name}: {printed}");
        assert!(!printed.contains("other"), "{name}: {printed}");
    }
}

/// A declaration of a unit in scope is printed as that unit was elaborated, and refused at `core`, which is the subject's own lowering: a unit in scope keeps none.
#[test]
fn a_declaration_in_scope_is_its_units_elaborated_part_and_has_no_core() {
    let elaborated =
        part("core-elab", TWO, &["std", "Bool", "not"]).expect("core-elab is answered");
    assert!(elaborated.contains("/std/Bool/not"), "{elaborated}");
    assert!(!elaborated.contains("/std/Bool/and"), "{elaborated}");

    assert!(matches!(
        part("core", TWO, &["std", "Bool", "not"]),
        Err(crate::Refusal::Kept { .. })
    ));
}

/// A program reading `n` from its arguments, so nothing folds away: `count` a tail recursion the optimizer makes a loop of in its caller, and `fib` a recursion of two calls, which stays a function of its own.
const RECURSIONS: &str = "use /std/{Str, Nat, List, Option, Result, Try, Io, Proc, Fmt};

let count(k: Nat, acc: Nat) -> Nat =
    match k | 0 => acc | p + 1 => count(p, acc + p) end;

let fib(n: Nat) -> Nat =
    match n | 0 => 0 | p + 1 => match p | 0 => 1 | q + 1 => fib(p) + fib(q) end end;

let execute() -> Try(Io, Str, {}) =
    let arguments = Proc/args()!;
    let bytes = Option/to_result(List/try_get(arguments, 1), \"no input\")!;
    let s = Result/map_failure(Str/of_bytes(bytes), (_) => \"invalid utf-8\")!;
    let n = Option/to_result(Nat/of_str(Str/trim(s)), \"bad input\")!;
    let () = Fmt/print(\"%\\n\")(count(n, 0))!;
    let () = Fmt/print(\"%\\n\")(fib(n))!;
    Try/pure(());

execute()
";

/// Past a unit's rungs, a declaration's part is what the rung holds of its lineages: a function kept on every rung prints on each, and a neighbour's code does not.
#[test]
fn each_rung_past_core_prints_the_declarations_part_and_nothing_else() {
    for name in ["anf", "anf-optm", "cps", "cps-optm", "wasm"] {
        let printed =
            part(name, RECURSIONS, &["fib"]).unwrap_or_else(|_| panic!("{name} is answered"));

        assert!(printed.contains("/fib"), "{name}: {printed}");
        assert!(!printed.contains("/count"), "{name}: {printed}");
    }
}

/// A declaration the optimizer made a loop of in its caller is answered at `cps-optm` with that loop, under the header of the function it stands in, and `…` for what that function's body holds besides.
#[test]
fn a_loop_made_of_a_declaration_is_answered_under_the_function_it_stands_in() {
    let printed = part("cps-optm", RECURSIONS, &["count"]).expect("cps-optm is answered");

    assert!(printed.contains("cont ~k"), "{printed}");
    assert!(printed.contains("$/count("), "{printed}");
    assert!(printed.contains("…"), "{printed}");
}
