//! A type a goal reports reads back in the module it stands in: spelled by what that module imports, by a root declaration's absolute path, and for every public standard declaration.

use {
    super::test_support::*, crate::*, curios_core::Global, curios_utilities::Qualifier,
    std::fmt::Write,
};

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
fn an_answer_naming_another_modules_own_declaration_is_declined() {
    // Unification unfolds `shown(n)` to `hidden(n) + 1` and solves the goal to `hidden(n)`, which only `Inner` names.
    let source = r#"
        mod Inner
            use /std/{Nat};
            let hidden(n: Nat) -> Nat = match n | 0 => 0 | k + 1 => hidden(k) end;
            pub let shown(n: Nat) -> Nat = hidden(n) + 1;
        end
        use /std/{Nat, Eq};
        let below(@m: Nat, _e: Eq()(m + 1, m + 1)) -> Nat = m;
        pub let t(n: Nat) -> Nat = below(@?, Eq/refl(@Nat, @/Inner/shown(n)));
        /std/print("")
    "#;

    assert!(declined(source));
}

/// Every public declaration of `/std` the entry root can reach, each by the shortest absolute path reaching it — one per declaration, whichever re-export that is.
fn standard_declarations() -> Vec<Qualifier> {
    let standard = Qualifier::from(["std"]);
    Fold::new(DEFAULT_BUDGET, &[], None)
        .units(
            |_| {},
            |prelude, _| {
                let (unit, earlier) = prelude.split_last().expect("the prelude has roots");
                let scope = earlier.iter().map(|root| root.text()).collect::<Vec<_>>();
                let spellings = unit.text().spellings(&scope);
                let own = spellings.units.last().expect("the unit's own paths");
                Ok(own
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
                            .map(|written| written.path)
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

    compile(&pasted(&source).unwrap()).unwrap();
}

#[test]
fn every_public_standard_declaration_reads_back_under_a_glob_import() {
    // `use /std/*` imports every top-level module and value, so each declaration is written through its module and every name in its type may be.
    let source = sweep(
        "use /std/*;",
        |path| path.segments()[1..].join("/"),
        "print(\"\")",
    );

    compile(&pasted(&source).unwrap()).unwrap();
}
