//! A type a goal reports reads back: written where the goal stood, it compiles. That is the promise every reader-facing rendering makes — a report spells what its reader could have written at that position — and these are the programs that hold it to that, each written the first time a rendering broke it.

use {
    crate::*, curios_core::Global, curios_text::RootSource, curios_utilities::Qualifier,
    std::fmt::Write,
};

use super::test_support::*;

/// Compile `source`, whose every `?` a goal report determines, write each goal's reported solution over the goal's own span, and compile the result. An error names the program as rewritten, so a failure shows the spelling that did not read back.
fn reads_back(source: &str) -> Result<(), String> {
    let rewritten = pasted(source)?;
    compile(&rewritten, None)
        .map(|_| ())
        .map_err(|error| format!("{rewritten}\ndoes not compile:\n{error}"))
}

/// `source` with each goal's reported solution written over the goal's own span.
fn pasted(source: &str) -> Result<String, String> {
    let reports = match compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &with_entrypoint_type(source, None),
        &RootSource::none(),
        |_| {},
    ) {
        Err(CompileError::Incomplete(reports)) => reports,
        Err(error) => return Err(format!("the program is refused:\n{error}")),
        Ok(_) => return Err("the program has no goal to read back".to_string()),
    };

    let mut answers = reports
        .iter()
        .map(|report| {
            let span = report.span.as_ref().expect("a goal report is located");
            assert_eq!(
                span.source.text, source,
                "a goal's span points into the program"
            );
            let answer = solution(&report.message)
                .unwrap_or_else(|| panic!("the goal is determined:\n{}", report.message));
            (span.start, span.end, answer)
        })
        .collect::<Vec<_>>();

    // Last first, so each splice leaves the offsets of the ones still to come where they were.
    answers.sort_by_key(|(start, _, _)| std::cmp::Reverse(*start));
    let mut rewritten = source.to_string();
    for (start, end, answer) in answers {
        rewritten.replace_range(start..end, &answer);
    }
    Ok(rewritten)
}

/// The `? = …` line of a goal report, with the lines a long solution breaks onto: those are indented past the two spaces every other line of the report takes.
fn solution(message: &str) -> Option<String> {
    let mut lines = message.lines();
    let first = lines
        .by_ref()
        .find_map(|line| line.strip_prefix("  ? = "))?;
    let rest = lines.take_while(|line| line.starts_with("   "));
    Some(
        std::iter::once(first)
            .chain(rest)
            .collect::<Vec<_>>()
            .join("\n"),
    )
}

#[test]
fn a_polymorphic_signature_over_imported_names_reads_back() {
    let source = r#"
        use /std/{List};
        pub let t: ? = List/map;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_local_function_type_reads_back() {
    let source = r#"
        use /std/{Nat};
        let double(n: Nat) -> Nat = n + n;
        pub let t: ? = double;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_tuple_type_reads_back() {
    let source = r#"
        use /std/{Nat, Bool};
        pub let t: ? = (1, true);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_applied_family_reads_back() {
    let source = r#"
        use /std/{Nat, Option};
        pub let t: ? = Option/some(1);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_proposition_over_an_implicit_parameter_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@Nat, @3);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_holding_a_lambda_reads_back() {
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = Eq/refl(@(Nat) -> Nat, @(n: Nat) => n);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_indexed_family_and_its_applications_read_back() {
    let source = r#"
        use /std/{Nat};
        induct Vec(T: Type): (n: Nat) -> Type
        | nil(): (0)
        | cons(@m: Nat, x: T, xs: Vec(T)(m)): (m + 1)
        end
        pub let family: ? = Vec;
        pub let at_nat: ? = Vec(Nat);
        pub let value: ? = Vec/cons(7, Vec/nil());
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_alias_over_a_local_witness_reads_back() {
    // `Wrap(A, use x)` under the binder `use Show(A)`: the argument is the binder resolution would find, so it is left out, and the binder, referenced nowhere else, prints unnamed.
    let source = r#"
        use /std/{Nat, Show};
        pub let Wrap(A: Type, use Show(A)) -> Type = {A};
        pub let v(@A: Type, use Show(A), x: Wrap(A)) -> Nat = 0;
        pub let t: ? = v;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_alias_over_a_global_witness_reads_back() {
    // Nothing in scope reaches `Show`, so resolution goes to the table, and coherence makes its answer the witness the report would otherwise have named by its identity.
    let source = r#"
        use /std/{Nat, Show};
        pub let Wrap(A: Type, use Show(A)) -> Type = {A};
        pub let u(x: Wrap(Nat)) -> Nat = 0;
        pub let t: ? = u;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_method_whose_type_names_an_earlier_field_reads_back() {
    // `Tot/div`'s `@ok` is typed by the concept's own `Ok`, a projection off the wrapper's witness, which prints as the call a program writes.
    let source = r#"
        pub concept Tot(A: Type): pub Type {
            Ok(A) -> Prop,
            div(a: A, b: A, @ok: Ok(b)) -> A,
        }
        pub let t: ? = Tot/div;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_law_over_its_concepts_methods_reads_back() {
    let source = r#"
        use /std/{Eq};
        pub concept Idem(A: Type): pub Type {
            op(A) -> A,
            law(x: A) -> Eq()(op(op(x)), op(x)),
        }
        pub let t: ? = Idem/law;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_lambda_naming_its_witness_reads_back() {
    // A lambda may name its witness and its type may not: the type prints `use Show(A)` unnamed.
    let source = r#"
        use /std/{Show, Str};
        pub let t: ? = (@A: Type, use s: Show(A), a: A) => Show/show(a);
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn an_operator_under_an_abstract_witness_reads_back() {
    // `a + a` elaborates to a projection off the `use Add(A)` binder, which prints as the operator.
    let source = r#"
        use /std/{Eq};
        use /std/ops/{Add};
        pub let f(@A: Type, use Add(A), a: A) -> Eq()(a + a, a + a) = Eq/refl();
        pub let t: ? = f;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_standard_alias_taking_a_witness_reads_back() {
    // `/std/Parse`'s `Parse(I, use Input(I), A)`: the `use` argument is the function type's own binder.
    let source = r#"
        use /std/{Parse};
        pub let t: ? = Parse/pure;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_type_over_names_nothing_imported_reads_back() {
    // Nothing is imported, so `List` resolves nowhere, however unambiguous a suffix it is: every name is written by a path that reaches it.
    let source = r#"
        pub let t: ? = /std/List/map;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_nested_modules_goal_is_spelled_by_what_that_module_imports() {
    // `use` binds to the end of the module it is written in and a nested module starts with nothing, so the root's `Nat` import does not reach `Inner`, and a root declaration is reached from there by its absolute path.
    let source = r#"
        use /std/{Nat};
        pub let double(n: Nat) -> Nat = n + n;
        pub mod Inner
            pub let t: ? = /double;
        end
        pub let u: ? = double;
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_nested_modules_goal_reaches_a_root_declaration_by_its_absolute_path() {
    // The root's `Colour` is a bare label only in the root: inside `Paint` a bare `Colour` resolves nowhere.
    let source = r#"
        pub induct Colour: pub Type
        | red()
        end
        pub mod Paint
            pub let t: ? = /Colour/red();
        end
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_module_reaches_its_own_declarations_by_their_labels() {
    let source = r#"
        pub mod Shapes
            pub induct Shape: pub Type
            | dot()
            end
            pub let t: ? = Shape/dot();
        end
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

#[test]
fn a_built_in_operation_reads_back() {
    // A built-in operation is spelled as the `/std` declaration that re-exports it resolves from the reader, like any other name.
    let source = r#"
        use /std/{Nat, Eq};
        pub let t: ? = (a: Nat, b: Nat) => Eq/refl(@Nat, @Nat/shl(a, b));
        /std/print("")
    "#;

    reads_back(source).unwrap();
}

/// Every public declaration of `/std` the entry root can reach, each by the shortest absolute path reaching it — one per declaration, whichever re-export that is.
fn standard_declarations() -> Vec<Qualifier> {
    let standard = Qualifier::from(["std"]);
    Fold::new(DEFAULT_STEP_BUDGET, &[], None)
        .units(
            |_| {},
            |prelude, _| {
                let unit = prelude.last().expect("the prelude has roots");
                Ok(unit
                    .text()
                    .spellings()
                    .paths
                    .iter()
                    .filter(|(global, _)| matches!(global, Global::Authored(_)))
                    .filter_map(|(_, paths)| {
                        paths
                            .iter()
                            .filter(|written| {
                                written.path.is_within(&standard)
                                    && written.audience.iter().any(Qualifier::is_root)
                            })
                            .map(|written| written.path.clone())
                            .min_by_key(|path| (path.segments().len(), path.join().len()))
                    })
                    .collect())
            },
        )
        .unwrap()
}

/// A program declaring one goal per `/std` declaration, each written by `spell`, after `header` and before `tail`.
fn sweep(header: &str, spell: impl Fn(&Qualifier) -> String, tail: &str) -> String {
    let mut source = format!("{header}\n");
    for (index, path) in standard_declarations().iter().enumerate() {
        writeln!(source, "pub let t{index}: ? = {};", spell(path)).unwrap();
    }
    source.push_str(tail);
    source
}

#[test]
fn every_public_standard_declaration_reads_back_by_its_absolute_path() {
    let source = sweep("", Qualifier::join, "/std/print(\"\")");

    compile(&pasted(&source).unwrap(), None).unwrap();
}

#[test]
fn every_public_standard_declaration_reads_back_under_a_glob_import() {
    // `use /std/*` imports every top-level module and value, so each declaration is written through its module and every name in its type may be.
    let source = sweep(
        "use /std/*;",
        |path| path.segments()[1..].join("/"),
        "print(\"\")",
    );

    compile(&pasted(&source).unwrap(), None).unwrap();
}
