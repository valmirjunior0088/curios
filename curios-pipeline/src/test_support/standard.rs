//! The standard library as its own package, and what its suites and the bench's measurements of it compile it over: the edits measured, and a module's declarations.

use {
    crate::*,
    curios_common::{Qualifier, RootKind},
    curios_core::{Definitions, Global},
    curios_elab::{
        Context, DEFAULT_BUDGET, Error, Established, Recompile, Recompiled,
        elaborate_and_zonk_unit_over,
    },
    curios_module::{PreparedText, RootSource, SYNTAX, UnitSource},
    curios_prelude_build::std_directory,
    curios_unit::{Predecessors, Unit},
    std::collections::BTreeSet,
};

/// The standard library's own tree, as the package claiming `/std` from it.
pub fn std_from_its_tree() -> RootSource {
    let directory = std_directory();

    RootSource::mounted(
        "std",
        RootKind::Ordinary,
        directory.join("lib.crs"),
        directory,
    )
    .declaring([Qualifier::from(["sys"])])
}

/// One edit the census measures: what it is, the file it touches, and the text it leaves there.
pub struct Edit {
    pub label: &'static str,
    pub file: &'static str,
    pub edit: fn(&str) -> String,
}

/// The two edits the standard library is measured over: `leaf`, a declaration added to `/std/Nat`, which nothing reads, and `hub`, the body of `/std/Bool/not` respelled, which most of the library reaches.
pub fn edits() -> [Edit; 2] {
    [
        Edit {
            label: "leaf",
            file: "Nat.crs",
            edit: |text| format!("{text}\npub let _census_probe(a: Nat) -> Nat =\n    a;\n"),
        },
        Edit {
            label: "hub",
            file: "Bool.crs",
            edit: |text| text.replacen("xor(b, true)", "xor(true, b)", 1),
        },
    ]
}

/// The names of each declaration of `module`, in item order: each item's, a family's constructors among its own.
pub fn declarations(definitions: &Definitions) -> Vec<BTreeSet<Global>> {
    definitions
        .items
        .iter()
        .map(|item| item.declared_names().into_iter().collect())
        .collect()
}

/// `source`, the standard library with an edit in it, compiled over `baseline`, its unit before the edit, as the product's cache compiles a unit no record agrees with: under `budget`, after `predecessors`, whose one unit has not moved since the baseline was compiled, with no candidates offered and nothing told.
pub fn recompiled_over(
    budget: u64,
    predecessors: Predecessors<'_>,
    source: &UnitSource<'_>,
    baseline: &Unit,
) -> Result<Unit, CompileError> {
    compile_unit_over(
        budget,
        predecessors,
        &SYNTAX,
        source,
        Over {
            baseline,
            moved: &[false],
            candidates: None,
        },
        &|_| {},
        Telling::default(),
    )
}

/// `lowered`, the library's sources lowered over the archived unit `std`, elaborated over it: every name of `relowered` is offered to the walk as one the lowering made anew, and the walk keeps each declaration whose reads stand.
pub fn elaborated_over(
    predecessors: Predecessors<'_>,
    std: &Unit,
    lowered: &PreparedText,
    relowered: &BTreeSet<Global>,
) -> Result<Recompiled, Error> {
    let established = Established::placed(&predecessors.placed()).keeping(
        std.core(),
        std.places(),
        std.classifications(),
    );
    let mut context = Context::new(DEFAULT_BUDGET, SYNTAX);
    context.set_imports(lowered.imports().clone());
    context.set_broken(lowered.broken_names());
    // A unit lowered whole drops what the archived unit's lowering declares and it does not.
    let dropped = lowered.dropped().cloned().unwrap_or_else(|| {
        let items = &std
            .text()
            .expect("a program's unit holds its lowering")
            .core()
            .items;
        items
            .places()
            .flat_map(|place| items.names(place))
            .filter(|name| lowered.declared_as(name).is_none())
            .collect()
    });

    elaborate_and_zonk_unit_over(
        &mut context,
        &established,
        Recompile {
            baseline: std.core(),
            places: std.places(),
            traces: std.traces(),
            readers: std.readers(),
            declarations: lowered.declarations(),
            relowered,
            dropped: &dropped,
            candidates: None,
        },
        lowered.core(),
        lowered.mounts(),
        lowered.minted(),
    )
}
