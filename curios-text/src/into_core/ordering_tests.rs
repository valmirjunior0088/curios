//! Declaration order the lowering derives, and the modules it reads from a loader.

use {
    super::test_support::*,
    crate::{Entrypoint, Error, RootSource},
    curios_utilities::{Qualifier, RootKind},
};

// A.f references B.g and B.h references A.e, with e and g independent — no cycle, but no contiguous source order binds both references. The reorder must produce a valid binding order, leaving the lowered term with no free name.
#[test]
fn orders_acyclic_bidirectional_value_graph() {
    assert!(
        run(r#"
            pub mod A
                pub let e : Type = Type;
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = Type;
                pub let h : Type = /A/e;
            end
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// A dependency through a type annotation is as much a binding-order constraint as one through a value: `f : T` declared before `T` must still order `T` first.
#[test]
fn orders_dependency_through_type_annotation() {
    assert!(
        run(r#"
            let f : T = Type;
            let T : Type = Type;
            f
        "#)
        .free_vars()
        .is_empty()
    );
}

// A genuine value cycle between separate items cannot be ordered, and it is not the lowering's to repair: cross-declaration recursion is declared with `and`, so a cycle the source did not declare is refused by name, with the way out.
#[test]
fn an_undeclared_value_cycle_is_refused_by_name() {
    let report = run_err(
        r#"
        pub mod A
            pub let f : Type = /B/g;
        end
        pub mod B
            pub let g : Type = /A/f;
        end
        Type
    "#,
    );
    assert!(
        report.contains("reference each other") && report.contains("`and`"),
        "expected the cycle refused with its way out: {report}"
    );
}

// A definition that names itself is the recursive group of one the kernel needs it to be — read off its body, never declared — so the reference is bound rather than left free.
#[test]
fn a_definition_that_names_itself_lowers_to_a_bound_group_of_one() {
    assert!(
        run(r#"
            let f : (n : Type) -> Type = (n) => f(n);
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

// A declared group binds every member's name in every member, whichever module each reference spells.
#[test]
fn a_declared_group_binds_its_members_in_one_item() {
    assert!(
        run(r#"
            pub let f : Type = g
            and g : Type = f;
            Type
        "#)
        .free_vars()
        .is_empty()
    );
}

/// The entry is a header like any other, so its own modules live in its stem directory: `main.crs` declaring `mod A` reads `main/A.crs`, never a sibling `A.crs`.
#[test]
fn the_entry_reads_its_modules_from_its_stem_directory() {
    let base = temp_dir("stem-directory");
    write_module(
        &base,
        "main/A.crs",
        r#"
            use /B/{x};
            pub let y : Type = x;
        "#,
    );
    write_module(&base, "main/B.crs", "pub let x : Type = Type;");
    // A sibling of the entry, which nothing may resolve to, since one rule governs every file.
    write_module(&base, "A.crs", "pub let wrong : Type = Type;");

    let entrypoint = r#"
            pub mod A;
            pub mod B;
            A/y
        "#
    .parse::<Entrypoint>()
    .unwrap();
    let loader = RootSource::entry(&base.join("main.crs"));

    super::into_core(&entrypoint, &loader, syntax()).unwrap();
}

#[test]
fn file_backed_module_missing_from_loader_is_module_not_found() {
    let entrypoint = r#"
            pub mod A;
            Type
        "#
    .parse::<Entrypoint>()
    .unwrap();

    assert!(matches!(
        super::into_core(&entrypoint, &RootSource::none(), syntax()).unwrap_err(),
        Error::Located { error, .. }
            if matches!(error.as_ref(), Error::ModuleNotFound { path } if path == "/A")
    ));
}

/// A source that is not a directory resolves, and resolves to the same unit one that is does.
///
/// The whole resolver contract is a qualifier in and a module out, and nothing above it may assume a filesystem: `curios-js` supplies every body inline and compiles with none at all, and a package fetched from anywhere arrives as bytes somebody else placed. So the two bases are written here against one another rather than each against itself — one of them being wrong is the interesting failure, not either of them being broken.
#[test]
fn a_supplied_source_and_a_directory_resolve_alike() {
    const HEADER: &str = "pub mod Inner;";
    const INNER: &str = "pub let x : Type = Type;";

    let mut supplied = RootSource::supplied();
    supplied.insert_root("pkg", RootKind::Ordinary, HEADER.parse().unwrap());
    supplied.insert_module(Qualifier::from(["pkg", "Inner"]), INNER.parse().unwrap());

    let base = temp_dir("supplied-versus-disk");
    write_module(&base, "pkg.crs", HEADER);
    write_module(&base, "pkg/Inner.crs", INNER);
    let disk = RootSource::mounted(
        "pkg",
        RootKind::Ordinary,
        base.join("pkg.crs"),
        base.join("pkg"),
    );

    let names = |source: &RootSource| {
        super::prepare_prelude(source, &[], syntax())
            .expect("a mounted unit lowers")
            .core()
            .items
            .iter()
            .map(curios_core::Item::describe)
            .collect::<Vec<_>>()
    };

    assert_eq!(names(&supplied), vec!["/pkg/Inner/x".to_string()]);
    assert_eq!(names(&supplied), names(&disk));
}

/// A derived witness lowers to a `Derive` transient, which carries no `Var` — so the scheduler cannot see the renderers and method wrapper the written body would have named, and `derived_vocabulary` supplies them as hard edges instead. This is what those edges buy: the vocabulary is declared *after* the witness here and must still be emitted before it.
///
/// The names come from the fixture registry rather than from `/std`, so this suite is what makes those spellings load-bearing: a row whose concept does not match what the source declares yields no edges at all, and the assertion below is what says so.
#[test]
fn a_derived_spell_witness_orders_its_vocabulary_first() {
    let module = lowered_module(
        r#"
        pub induct Colour : pub Type | red() end
        satisfy /std/Spell/Spell(Colour) { .. }
        pub mod std
            pub mod Spell
                pub concept Spell(A : Type) : pub Type {
                    spell(A) -> Type,
                }
                pub let call : Type = Type;
                pub let record : Type = Type;
            end
            pub mod Str
                pub let Str : Type = Type;
            end
            pub mod Bool
                pub mod True
                    pub let qed : Type = Type;
                end
            end
        end
        Type
        "#,
    );

    let names = module
        .items
        .iter()
        .map(curios_core::Item::describe)
        .collect::<Vec<_>>();
    let at = |needle: &str| {
        names
            .iter()
            .position(|name| name.contains(needle))
            .unwrap_or_else(|| panic!("{needle} is not among {names:?}"))
    };

    let witness = at("witness");
    assert!(at("/std/Spell/Spell/spell") < witness, "{names:?}");
    assert!(at("/std/Spell/call") < witness, "{names:?}");
    assert!(at("/std/Spell/record") < witness, "{names:?}");

    // The rendered pieces are string literals, so the carrier and the proof that closes its `Valid` field are as much a part of what the body writes as the renderers are.
    assert!(at("/std/Str/Str") < witness, "{names:?}");
    assert!(at("/std/Bool/True/qed") < witness, "{names:?}");
}

/// The equality derivation applies its concept's own method and nothing else — no renderer, and no string machinery, since it builds a `Bool` rather than text.
#[test]
fn a_derived_equality_witness_orders_its_method_first() {
    let module = lowered_module(
        r#"
        pub induct Colour : pub Type | red() end
        satisfy /std/Equal/Equal(Colour) { .. }
        pub mod std
            pub mod Equal
                pub concept Equal(A : Type) : pub Type {
                    eql(A, A) -> Type,
                    neq(A, A) -> Type,
                }
            end
        end
        Type
        "#,
    );

    let names = module
        .items
        .iter()
        .map(curios_core::Item::describe)
        .collect::<Vec<_>>();
    let at = |needle: &str| {
        names
            .iter()
            .position(|name| name.contains(needle))
            .unwrap_or_else(|| panic!("{needle} is not among {names:?}"))
    };

    assert!(at("/std/Equal/Equal/eql") < at("witness"), "{names:?}");
}

// A witness row registered into a concept an earlier unit declared is dispatched through that unit's wrappers, which are no item of this one — so no name here can order a use after the row, and the edge is read off the earlier unit's module. `user` waits on nothing but `Colour` by name, and without the edge is emitted ahead of the row its reference dispatches through.
#[test]
fn a_reference_to_an_earlier_units_wrapper_follows_the_row_it_dispatches_through() {
    let module = lowered_module_over_prelude(
        r#"
        pub induct Colour : pub Type | red() end
        pub let user : Type = /std/Shape/Shape/area(Colour/red(), Colour/red());
        pub let method : Type = Type;
        satisfy /std/Shape/Shape(Colour) {
            area = method,
        }
        Type
        "#,
    );

    let names = module
        .items
        .iter()
        .map(curios_core::Item::describe)
        .collect::<Vec<_>>();
    let at = |needle: &str| {
        names
            .iter()
            .position(|name| name.contains(needle))
            .unwrap_or_else(|| panic!("{needle} is not among {names:?}"))
    };

    assert!(at("witness") < at("/user"), "{names:?}");
}

/// The procedure that proves a bound from the facts in scope writes its vocabulary into proofs no item spells, so where that vocabulary is declared beside its users — `/std` — every item it does not itself need is ordered after it, whatever the source order, and what it needs stays ahead of it.
#[test]
fn every_item_the_proof_vocabulary_does_not_need_follows_it() {
    let module = lowered_module(
        r#"
        pub let user : Type = Type;
        pub mod std
            pub mod Nat
                pub mod le
                    pub let add : Type = helper;
                    pub let helper : Type = Type;
                end
            end
        end
        Type
        "#,
    );

    let names = module
        .items
        .iter()
        .map(curios_core::Item::describe)
        .collect::<Vec<_>>();
    let at = |needle: &str| {
        names
            .iter()
            .position(|name| name.contains(needle))
            .unwrap_or_else(|| panic!("{needle} is not among {names:?}"))
    };

    assert!(at("/std/Nat/le/add") < at("/user"), "{names:?}");
    assert!(
        at("/std/Nat/le/helper") < at("/std/Nat/le/add"),
        "{names:?}"
    );
}
