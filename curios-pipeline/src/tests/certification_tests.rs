//! The certifier's record a compiled unit files beside its definitions: what it covers, what it says, what it read, what an item-level recompile keeps of the baseline's, and the termination flags it marks on a sealed program.

use {
    super::test_support::{entrypoint_of, recompile_over, reuses_body, unit_of},
    crate::{DEFAULT_STEP_BUDGET, Stage, compile_with_prelude},
    curios_core::{Enter, Free, Global, Reads, Subterm, Term, Totality},
    curios_prelude::with_prelude,
    curios_text::RootSource,
    curios_unit::Unit,
    std::collections::{BTreeMap, BTreeSet, HashSet},
};

/// A total function, one that recurses without descending — a single-member `rec` group, since its body names itself — and a value reaching the first.
const SOURCE: &str = "use /std/{Nat};

pub let double(n: Nat) -> Nat = n + n;

pub let spin(n: Nat) -> Nat = spin(n);

pub let twice: Nat = double(2);
";

/// A type named by a value typed at it, a value that names neither, and a family whose one constructor takes it.
const READING: &str = "use /std/{Nat};

pub let Count: Type = Nat;

pub let three: Count = 3;

pub let four: Nat = 4;

pub induct Box: pub Type
| mk(Count)
end
";

/// The name of the definition `unit` calls `symbol`.
fn named(unit: &Unit, symbol: &str) -> Global {
    unit.core()
        .items
        .iter()
        .flat_map(|item| item.definitions())
        .map(|definition| definition.name)
        .find(|name| name.symbol().ends_with(&format!("/{symbol}")))
        .unwrap_or_else(|| panic!("the unit defines no `{symbol}`"))
}

/// The certifier's classification of the definition `unit` calls `symbol`.
fn classified(unit: &Unit, symbol: &str) -> Option<Totality> {
    unit.certification().totality(&named(unit, symbol))
}

/// The top-level names `term` mentions outside a nominal value's parameters, each node visited once. The kernel counts a record literal's or a constructor application's parameters and never types them — the value's type is taken from them as written, and conversion compares them — so a name standing only there is neither typed nor unfolded.
fn typed_mentions(term: &Term, visited: &mut HashSet<Term>, into: &mut BTreeSet<Global>) {
    term.walk(
        &mut (),
        |_, term| {
            if !visited.insert(term.clone()) {
                return Enter::Skip(());
            }
            match &**term {
                Subterm::Struct(value) => {
                    for field in &value.fields {
                        typed_mentions(field, visited, into);
                    }
                    Enter::Skip(())
                }
                Subterm::Variant(value) => {
                    for payload in &value.payload {
                        typed_mentions(payload, visited, into);
                    }
                    Enter::Skip(())
                }
                Subterm::Var(var) => {
                    if let Some(global) = var.as_free().and_then(Free::as_global) {
                        into.insert(*global);
                    }
                    Enter::Skip(())
                }
                _ => Enter::Descend,
            }
        },
        |_, _, _| (),
    );
}

/// What judging the definition `unit` calls `symbol` read of other items.
fn read_by<'a>(unit: &'a Unit, symbol: &str) -> &'a Reads {
    unit.certification()
        .reads(&named(unit, symbol))
        .unwrap_or_else(|| panic!("the record does not cover `{symbol}`"))
}

/// A unit the pipeline compiled carries the kernel's record of every definition it holds, each classified as the kernel's own closure found it.
#[test]
fn a_compiled_unit_files_the_certifiers_record_of_every_definition() {
    let unit = unit_of(SOURCE);

    assert!(unit.certification().covers(unit.core()));
    assert_eq!(classified(&unit, "double"), Some(Totality::Total));
    assert_eq!(classified(&unit, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&unit, "twice"), Some(Totality::Total));
}

/// An item-level recompile walks only the closure the edit reached, so its record joins the baseline's classifications for what it reused to its own walk's — covering the unit whole, with a reused item classified as the walk that filed the baseline classified it.
#[test]
fn an_item_level_recompile_keeps_the_baselines_record_for_what_it_reuses() {
    let baseline = unit_of(SOURCE);
    let edited = SOURCE.replace("double(2)", "double(3)");

    let recompiled = recompile_over(&edited, &baseline).expect("the edit compiles");

    assert!(reuses_body(&baseline, &recompiled, "double"));
    assert!(!reuses_body(&baseline, &recompiled, "twice"));
    assert!(recompiled.certification().covers(recompiled.core()));
    assert_eq!(classified(&recompiled, "double"), Some(Totality::Total));
    assert_eq!(classified(&recompiled, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&recompiled, "twice"), Some(Totality::Total));
    assert_eq!(read_by(&recompiled, "double"), read_by(&baseline, "double"));
    assert!(
        read_by(&recompiled, "twice")
            .signatures
            .contains(&named(&recompiled, "double"))
    );
}

/// Each definition's record holds what judging it read, and nothing another judgment read: `3` is a `Count` only once `Count` unfolds to `Nat`, so `three` reads its body, while `four` reads nothing of it. A declaration's registry entry is accepted as part of its type former, so `Box`'s record holds what accepting `mk(Count)` read — typing the former alone names no `Count`. Mutation-checked: filing an entry's reads nowhere fails the last assertion and no other.
#[test]
fn a_compiled_unit_files_what_judging_each_definition_read() {
    let unit = unit_of(READING);
    let count = named(&unit, "Count");

    assert!(read_by(&unit, "three").bodies.contains(&count));

    let four = read_by(&unit, "four");
    assert!(!four.signatures.contains(&count));
    assert!(!four.bodies.contains(&count));

    let boxed = read_by(&unit, "Box");
    assert!(boxed.signatures.contains(&count) || boxed.bodies.contains(&count));
}

/// The kernel reads along the graph elaboration built, over the whole prelude. Every name a definition mentions where the kernel types is read — its signature where the name is typed, its body where a type naming it is typed by reducing it first — a group's own members excepted, which it types as the locals it opens, and a nominal value's parameters excepted, which it never types ([`typed_mentions`]); and nothing is read that the definition's item does not [reach](curios_core::Module::reaches) transitively — the graph an item-level recompile invalidates along, a declaring item reaching what its registry entry names. The first half is what catches a way of consulting the environment that records nothing, and the second what makes the record no coarser than the graph it refines. Mutation-checked: recording no body read leaves the first half with 1 511 unread mentions.
#[test]
fn the_prelude_reads_along_the_graph_its_definitions_reach() {
    with_prelude(|prelude| {
        let mut graph: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
        for unit in prelude {
            for item in &unit.core().items {
                let reaches = unit.core().reaches(item);
                for name in item.declared_names() {
                    graph.insert(*name, reaches.clone());
                }
            }
        }
        let reached = |name: &Global| {
            let mut reached = BTreeSet::new();
            let mut frontier = vec![*name];
            while let Some(name) = frontier.pop() {
                for next in graph.get(&name).into_iter().flatten() {
                    if reached.insert(*next) {
                        frontier.push(*next);
                    }
                }
            }
            reached
        };

        let mut unread = Vec::new();
        let mut unreached = Vec::new();
        for unit in prelude {
            for item in &unit.core().items {
                let own = item
                    .declared_names()
                    .into_iter()
                    .cloned()
                    .collect::<BTreeSet<_>>();
                for definition in item.definitions() {
                    let reads = unit
                        .certification()
                        .reads(&definition.name)
                        .expect("the prelude's record covers it");
                    let mut mentions = BTreeSet::new();
                    let mut visited = HashSet::new();
                    typed_mentions(&definition.type_, &mut visited, &mut mentions);
                    typed_mentions(&definition.body, &mut visited, &mut mentions);
                    unread.extend(
                        mentions
                            .difference(&own)
                            .filter(|name| {
                                !reads.signatures.contains(*name) && !reads.bodies.contains(*name)
                            })
                            .map(|name| (definition.name.symbol(), name.symbol())),
                    );
                    let reached = reached(&definition.name);
                    unreached.extend(
                        reads
                            .signatures
                            .iter()
                            .chain(&reads.bodies)
                            .filter(|name| !reached.contains(*name))
                            .map(|name| (definition.name.symbol(), name.symbol())),
                    );
                }
            }
        }

        assert!(
            unread.is_empty(),
            "{} mentions were never read, first {:?}",
            unread.len(),
            &unread[..unread.len().min(10)]
        );
        assert!(
            unreached.is_empty(),
            "{} reads fall outside the graph, first {:?}",
            unreached.len(),
            &unreached[..unreached.len().min(10)]
        );
    });
}

/// The fixed prelude is lent with the record its build's certification filed, and that record covers each root whole — so a walk with the prelude in scope reads the certifier's verdict on every `/sys` and `/std` definition and classifies none of them for itself.
#[test]
fn the_restored_prelude_carries_a_record_covering_every_definition() {
    with_prelude(|prelude| {
        for unit in prelude {
            assert!(unit.certification().covers(unit.core()));
        }
    });
}

/// A sealed program's functions are marked total from the certifier's records of everything it was erased from — the prelude's and the entry's own — and from nothing else: erasure marks nothing, so every mark here is one a record justified.
///
/// Matched by debug name, which erasure derives from the definition's qualified symbol: the symbol itself, or the symbol and a path below it where the definition's value is a function inside it — a single-method witness collapses to its method, named for the field. A function minted inside a body shares that derivation and is never marked, so the check runs from the marks outward: each marked function's definition a record classifies `Total`, none a record classifies `Partial` marked, the prelude contributing marks at all, and the entry's own two definitions marked exactly as its walk classified them. Mutation-checked: marking the sealed arena from no unit's record in scope fails the prelude's half.
#[test]
fn a_sealed_program_marks_its_functions_from_the_certifiers_records() {
    let source = "use /std/{Nat};

let double(n: Nat) -> Nat = n + n;

let spin(n: Nat) -> Nat = spin(n);

let subject : /std/Nat = double(2) + spin(0);
/std/Io/pure(())
";
    let mut functions = Vec::new();
    compile_with_prelude(
        DEFAULT_STEP_BUDGET,
        &entrypoint_of(source),
        &RootSource::none(),
        |stage| {
            if let Stage::Ersd(module) = stage {
                functions = module
                    .functions()
                    .iter()
                    .flatten()
                    .map(|function| (function.debug_name.clone(), function.total))
                    .collect();
            }
        },
    )
    .expect("the program compiles");
    let prelude = with_prelude(|prelude| {
        prelude
            .iter()
            .flat_map(|unit| unit.certification().iter())
            .map(|(name, totality)| (name.symbol(), totality))
            .collect::<BTreeMap<_, _>>()
    });

    let mut marked_from_the_prelude = 0;
    for (name, total) in &functions {
        let Some(name) = name else {
            assert!(!total, "a function with no definition behind it is marked");
            continue;
        };
        // The definition a function's name derives from: the longest recorded symbol that is the name or a `/`-separated prefix of it.
        let mut definition = Some(name.as_str());
        let from_the_prelude = loop {
            match definition {
                Some(symbol) if prelude.contains_key(symbol) => break Some(prelude[symbol]),
                Some(symbol) => definition = symbol.rsplit_once('/').map(|(above, _)| above),
                None => break None,
            }
        };
        let recorded = match from_the_prelude {
            Some(totality) => Some(totality),
            None if name.ends_with("/double") => Some(Totality::Total),
            None if name.ends_with("/spin") => Some(Totality::Partial),
            None => None,
        };
        if *total {
            assert_eq!(
                recorded,
                Some(Totality::Total),
                "{name} is marked without a record classifying it total"
            );
            marked_from_the_prelude += usize::from(from_the_prelude.is_some());
        }
    }
    assert!(
        marked_from_the_prelude > 0,
        "no prelude function was marked"
    );

    let entry = |suffix: &str| {
        functions
            .iter()
            .filter(|(name, _)| name.as_deref().is_some_and(|name| name.ends_with(suffix)))
            .any(|(_, total)| *total)
    };
    assert!(
        entry("/double"),
        "the entry's total definition is not marked"
    );
    assert!(!entry("/spin"), "the entry's partial definition is marked");
}
