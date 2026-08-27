//! Declaration order the lowering derives, and the modules it reads from a loader.

use crate::{Entrypoint, Error, RootSource};
use curios_utilities::{Qualifier, RootKind};
use std::fs;

use super::test_support::*;

// Phase 5: A.f references B.g and B.h references A.e, with e and g independent — no cycle, but no contiguous source order binds both references. The reorder must produce a valid binding order, leaving the lowered term with no free name.
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

// A genuine non-atomic value cycle cannot be ordered; phase 5 emits it anyway and leaves one reference as a free name, which core rejects as unbound. There is nothing to repair — cross-declaration value recursion is unexpressible.
#[test]
fn genuine_value_cycle_leaves_unbound_name() {
    assert!(
        !run(r#"
            pub mod A
                pub let f : Type = /B/g;
            end
            pub mod B
                pub let g : Type = /A/f;
            end
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
    // A sibling of the entry, which nothing may resolve to now that one rule governs every file.
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

    fs::remove_dir_all(base).unwrap();
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
        super::prepare_prelude(source, syntax())
            .expect("a mounted unit lowers")
            .core()
            .items
            .iter()
            .map(curios_core::Item::describe)
            .collect::<Vec<_>>()
    };

    assert_eq!(names(&supplied), vec!["/pkg/Inner/x".to_string()]);
    assert_eq!(names(&supplied), names(&disk));

    fs::remove_dir_all(base).unwrap();
}
