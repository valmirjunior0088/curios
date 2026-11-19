//! What a compile tells a reader of its subject's declarations as elaboration established them: the unit's module with the certifier's record where the unit was made, and where it was refused, the declarations that elaborated.

use {
    super::test_support::*,
    crate::*,
    curios_core::{Certification, Definitions},
    curios_elab::{DEFAULT_BUDGET, Traces},
    curios_module::{PreparedText, RootSource, SYNTAX, UnitSource},
    curios_prelude::with_prelude,
    curios_unit::{Predecessors, Unit},
    std::{cell::RefCell, collections::BTreeSet},
};

const PROGRAM: &str = "use /std/{Nat};\nlet seven: Nat = 7;\nlet eight: Nat = 8;\nlet double(n: Nat) -> Nat = n + n;\nlet quad(n: Nat) -> Nat = double(double(n));\n/std/print(\"\")\n";

/// What compiling `source` through `cache` tells of its declarations as elaboration established them: the path of each name they declare, and whether the certifier's record came with them. None where it told nothing.
fn told(source: &str, cache: Option<&dyn Cache>) -> Option<(BTreeSet<String>, bool)> {
    let entrypoint = entrypoint_of(source);
    let told = RefCell::new(None);
    let elaborated = |_: &[&Unit],
                      _: &PreparedText,
                      definitions: &Definitions,
                      _: &Traces,
                      certification: Option<&Certification>| {
        let names = definitions
            .items
            .iter()
            .flat_map(|item| item.declared_names())
            .map(|name| name.path())
            .collect();
        *told.borrow_mut() = Some((names, certification.is_some()));
    };

    // The compile's own verdict is beside the point: what it told is.
    let _ = with_prelude(|prelude| {
        compile_entrypoint(
            DEFAULT_BUDGET,
            Predecessors::over(prelude),
            &SYNTAX,
            &UnitSource::entry(&entrypoint, &RootSource::none()),
            cache,
            EntryTail::Program,
            Told {
                observe: &mut |_: Stage<'_>| {},
                read: None,
                elaborated: Some(&elaborated),
                progress: &mut |_| {},
            },
        )
    });

    told.take()
}

/// The paths `paths` name.
fn paths(paths: &[&str]) -> BTreeSet<String> {
    paths.iter().map(|path| path.to_string()).collect()
}

/// A program its checks accept tells its declarations' module, with the record the certifier filed of it.
#[test]
fn an_accepted_program_tells_its_module_with_the_certifiers_record() {
    assert_eq!(
        told(PROGRAM, None),
        Some((paths(&["/seven", "/eight", "/double", "/quad"]), true))
    );
}

/// A program elaboration refuses tells the declarations that elaborated, with no record: the refused one stays out, and so does the one reaching it.
#[test]
fn a_refused_program_tells_the_declarations_that_elaborated() {
    let refused = PROGRAM.replace("n + n", "true");

    assert_eq!(
        told(&refused, None),
        Some((paths(&["/seven", "/eight"]), false))
    );
}

/// Over the unit a cache kept, a refused edit tells what its walk made and what it kept: `eight` is elaborated again, `seven` is kept, and neither the refused `double` nor `quad`, which reaches it, is told, though the baseline holds both.
///
/// Mutation-checked: with every item the baseline holds told but a dropped one's, `double` and `quad` come back as the baseline holds them.
#[test]
fn a_refused_recompile_tells_what_its_walk_made_and_kept() {
    let cache = Keeping::default();
    told(PROGRAM, Some(&cache));
    let refused = PROGRAM.replace("n + n", "true").replace("= 8;", "= 9;");

    assert_eq!(
        told(&refused, Some(&cache)),
        Some((paths(&["/seven", "/eight"]), false))
    );
}
