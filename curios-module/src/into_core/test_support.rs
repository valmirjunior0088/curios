//! Lowering a source to core and reading the result back: the harness every case in these suites asserts through.

use {
    super::lower_unit,
    crate::*,
    curios_abi::ForeignStore,
    curios_common::{Mount, Qualifier, SyntaxRegistry, test_support::*},
    curios_core::test_support::*,
    curios_surface::Entrypoint,
    std::{
        collections::{BTreeSet, HashSet},
        fs,
        path::Path,
        rc::Rc,
    },
};

/// The registry the lowering's suites lower through ([`crate::test_support::SYNTAX`]).
pub fn syntax() -> &'static SyntaxRegistry {
    &crate::test_support::SYNTAX
}

/// A top-level definition's identity, from the path a test writes. Fixture-only — production code carries the `Qualifier` from resolution instead of recovering it from a spelling.
pub fn global_name(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(Qualifier::from(path.trim_start_matches('/').split('/')))
}

/// What a fixture's entry resolves through: no filesystem, and the entry tested, so its tests are lowered as a question lowers a subject's. What a build passes over is held by a test of its own.
pub fn fixture_loader() -> RootSource {
    RootSource::none().tested()
}

/// The lints of `src` lowered alone, each as `kind: message`, in reading order: the lowering's own — an import nothing resolved through, a declaration nothing reaches — and what the sites it found no written name reads come to where no proof read one and no site's class is known, as a declaration nothing elaborated would be filed with.
pub fn lints(src: &str) -> Vec<String> {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let text = into_core_unit(
        &UnitSource::entry(&entrypoint, &fixture_loader()),
        &[],
        syntax(),
    )
    .unwrap();
    // A declaration's names share what it minted, so each entry is read once.
    let minted = text.minted();
    let mut read = HashSet::new();
    let unread = minted
        .each()
        .filter(|(_, mints)| read.insert(std::ptr::from_ref(*mints)))
        .flat_map(|(_, mints)| &mints.unread)
        .chain(&minted.entry.unread);
    let binders = curios_elab::binder_lints(unread, |_| false, |_| None);

    curios_core::Lint::ordered(
        text.lints().iter().cloned().chain(binders).collect(),
        text.anchors(),
    )
    .iter()
    .map(|lint| format!("{}: {}", lint.kind.name(), lint.report(&[]).message()))
    .collect()
}

/// The entry's body as the lowering writes it, the module it closes over left aside.
pub fn run(src: &str) -> curios_core::Term {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();

    program.entrypoint.body
}

/// What `src` lowers to as a suite reads it: each definition's name, type and body in the module's order, beside the entry's body.
pub fn lowered(src: &str) -> (Vec<Lowered>, curios_core::Term) {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    let definitions = program
        .definitions
        .items
        .iter()
        .flat_map(curios_core::Item::definitions)
        .map(|definition| (definition.name, definition.type_, definition.body))
        .collect();

    (definitions, program.entrypoint.body)
}

/// A definition as [`lowered`] reads it: its name, its type and its body.
pub type Lowered = (curios_core::Global, curios_core::Term, curios_core::Term);

pub fn lowered_definitions(src: &str) -> curios_core::Definitions {
    let (program, _, _, _) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();

    program.definitions
}

/// The members of the one item `src` lowers to, where it is a recursive group whose bodies name no declaration but its own members: `None` where it is anything else.
pub fn closed_group(src: &str) -> Option<Vec<String>> {
    let definitions = lowered_definitions(src);
    let [curios_core::Item::Rec(group)] = &definitions.items.iter().collect::<Vec<_>>()[..] else {
        return None;
    };
    let definitions = group.definitions();
    let members = definitions
        .iter()
        .map(|definition| definition.name)
        .collect::<BTreeSet<_>>();

    definitions
        .iter()
        .all(|definition| {
            definition.body.mentions().iter().all(|name| {
                name.as_declaration()
                    .is_some_and(|name| members.contains(name))
            })
        })
        .then(|| {
            definitions
                .iter()
                .map(|definition| definition.name.faithful().to_string())
                .collect()
        })
}

pub fn written_type(id: usize) -> curios_core::Term {
    curios_core::Term::type_at(curios_core::Level::meta(curios_core::UniverseMetaId(id)))
}

/// `src` lowered and elaborated as a program against `established`, its entry inferred.
fn elaborate_program(
    src: &str,
    established: &curios_elab::Established<'_>,
) -> curios_core::Program {
    let (program, minted, _, mounts) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    let (definitions, entry) = curios_elab::elaborate_and_zonk_program(
        &mut curios_elab::Context::with_default_budget(*syntax()),
        established,
        &program.definitions,
        &mounts,
        &minted,
        curios_elab::Tail::Entry(&program.entrypoint),
    )
    .unwrap();

    curios_core::Program {
        definitions,
        entrypoint: entry.expect("nothing here fails to parse, so nothing withholds the entry"),
    }
}

/// `src` elaborated with nothing in scope, and the term it closes with.
pub fn elaborate_source_program(src: &str) -> curios_core::Program {
    elaborate_program(src, &curios_elab::Established::nothing())
}

/// The module `src` elaborates to with nothing in scope.
pub fn elaborate_source(src: &str) -> curios_core::Definitions {
    elaborate_source_program(src).definitions
}

pub fn elaboration_paths(src: &str) -> (curios_core::Program, curios_core::Program) {
    let (lowered, minted, _, mounts) = into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap();
    assert!(lowered.definitions.items.len() >= 2);

    let mut lowered_prefix = lowered.definitions.clone();
    lowered_prefix.items = lowered_prefix.items.iter().take(1).cloned().collect();
    let nothing = curios_elab::Established::nothing();
    let (prelude, traces) = curios_elab::elaborate_and_zonk_unit(
        &mut curios_elab::Context::with_default_budget(*syntax()),
        &nothing,
        &lowered_prefix,
        &mounts,
        &minted,
    )
    .unwrap();

    let full = elaborate_program(src, &nothing);
    let cached = elaborate_program(
        src,
        &curios_elab::Established::over(&[(&prelude, &mounts, &traces)]),
    );
    (full, cached)
}

/// What stops `src` compiling, as its lowering knows it: the refusal it raised, or, where it raised none, every name it filed as resolving to nothing, each as elaboration reports it of the declaration that wrote it. A name written in a declaration refuses that declaration and not the lowering, so a fixture about a name reads both.
pub fn run_err(src: &str) -> String {
    let entrypoint = src.parse::<Entrypoint>().unwrap();
    let loader = fixture_loader();

    match super::into_core_unit(
        &super::UnitSource::entry(&entrypoint, &loader),
        &[],
        syntax(),
    ) {
        Err(error) => error.to_string(),
        Ok(unit) => unresolved(unit.minted()).expect_err("the source lowers, every name resolved"),
    }
}

// A refusal the lowering raised, rendered as the reader sees it: the block, with its pane where the error was placed.
pub fn run_err_report(src: &str) -> String {
    into_core(
        &src.parse::<Entrypoint>().unwrap(),
        &fixture_loader(),
        syntax(),
    )
    .unwrap_err()
    .format()
}

/// A directory of its own, removed however the test that holds it ends.
pub fn temp_dir(name: &str) -> Temporary {
    Temporary::new("into-core", name)
}

pub fn write_module(base: &Path, path: &str, source: &str) {
    let path = base.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, source).unwrap();
}

pub fn universe_parameters(definitions: &curios_core::Definitions, name: &str) -> usize {
    definitions
        .items
        .iter()
        .find_map(|item| match item {
            curios_core::Item::Let(definition)
                if definition.name.faithful().to_string() == name =>
            {
                Some(definition.universe_context.parameter_count)
            }
            // An inductive and its constructors are one recursive group, so a lookup restricted to `Let` would miss every one of them.
            curios_core::Item::Rec(rec) => rec
                .definitions()
                .iter()
                .find(|definition| definition.name.faithful().to_string() == name)
                .map(|definition| definition.universe_context.parameter_count),
            _ => None,
        })
        .unwrap_or_else(|| panic!("{name} is declared"))
}

/// Lower a whole [`Entrypoint`] with nothing in scope, as `curios-module`'s own stage tests do: the program, the counters its lowering minted from, its `foreign` rows and the prefixes its unit claims.
pub fn into_core(
    entrypoint: &Entrypoint,
    loader: &RootSource,
    syntax: &SyntaxRegistry,
) -> Result<
    (
        curios_core::Program,
        Rc<curios_core::Minted>,
        ForeignStore,
        Vec<Mount>,
    ),
    Error,
> {
    let (unit, entry) = lower_unit(&UnitSource::entry(entrypoint, loader), &[], syntax, None)?;

    Ok((
        curios_core::Program {
            definitions: unit.core,
            entrypoint: entry.expect("an entry source's parse holds its final term"),
        },
        unit.minted,
        unit.foreigns,
        unit.mounts,
    ))
}
