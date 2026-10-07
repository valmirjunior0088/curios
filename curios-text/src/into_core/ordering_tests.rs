//! The order items leave the lowering in, the groups it binds, and the modules it reads from a loader.

use {
    super::test_support::*,
    crate::{Entrypoint, Error, RootSource},
    curios_utilities::{Qualifier, RootKind},
};

/// Items leave the lowering as they are written, whatever each names: `f` is written before the `T` its annotation names and the `g` another module declares, and no item is moved for another's sake. Which elaborates first is elaboration's to say, and the order they are stored in is the module's.
#[test]
fn items_leave_the_lowering_as_they_are_written() {
    let module = lowered_module(
        r#"
        let f : T = /B/g;
        let T : Type = Type;
        pub mod B
            pub let g : Type = Type;
        end
        f
        "#,
    );

    assert_eq!(
        module
            .items
            .iter()
            .map(curios_core::Item::describe)
            .collect::<Vec<_>>(),
        ["/f", "/T", "/B/g"],
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
