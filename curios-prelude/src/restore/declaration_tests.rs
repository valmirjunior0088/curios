//! What the restored prelude declares: no tests, monomorphic literal machinery, every root module classified and none pure reaching a host call, each item's erased form and registry key, the syntax targets and concept methods, the marks its binders bear, one universe parameter per `/sys` former, and how far each term's tree outgrows its graph.

use {
    super::*,
    crate::test_support::*,
    curios_core::{
        Bound, Cases, Enter, Global, Item, Match, Subterm, Telescope, Term, Visit, method_labels,
    },
    curios_module::SYNTAX,
    curios_prelude_build::std_directory,
    curios_surface::TopItem,
    std::{
        cell::{Cell, RefCell},
        collections::{BTreeMap, BTreeSet, HashMap, HashSet},
        fs,
        ops::ControlFlow,
        rc::Rc,
    },
};

/// A prelude test would ride into every compilation and surface in every downstream `curios test` run, so the standard library shipping none is a contract, not an accident of today's sources.
#[test]
fn the_stored_prelude_declares_no_tests() {
    with_prelude(|prelude| {
        assert!(prelude.iter().all(|root| root.core().tests().is_empty()));
    });
}

/// The declarations a literal expands into, and those its proof is checked by running, are monomorphic, so no occurrence mints universe metavariables in the data's length.
///
/// A literal's type is `Str` at a single level, and its proof is `True/qed()` against a decided `Valid` — once, whatever the length, and `True` has no level to mint. What stays pinned is what checking that proof reduces: each carrier's `Valid`, the `Valid/from` a string's unfolds to, and the `scan_from` fold under it, on which a level would be minted once per literal or once per byte, and a declaration's level count would grow with literal *length* — which is what makes `long_str_literal_compiles_on_the_default_test_stack` a test.
#[test]
fn string_literal_machinery_is_monomorphic() {
    // `Str` and `Char` are the carriers a literal builds; the rest is what its proof is discharged by running. None of the reduced names is in `curios-module/src/registry.rs`: nothing in Rust emits them, they are reached through the carriers' field types.
    let pinned = [
        "/std/Str/Str",
        "/std/Str/Valid/Valid",
        "/std/Str/Valid/from",
        "/std/Str/utf8/scan_from",
        "/std/Char/Char",
        "/std/Char/Valid/Valid",
    ];

    with_prelude(|prelude| {
        let mut parameters = std::collections::BTreeMap::new();
        for item in items(prelude) {
            match item {
                Item::Let(definition) => {
                    parameters.insert(
                        definition.name.faithful().to_string(),
                        definition.universe_context.parameter_count,
                    );
                }
                Item::Rec(rec) => {
                    for definition in rec.definitions() {
                        parameters.insert(
                            definition.name.faithful().to_string(),
                            definition.universe_context.parameter_count,
                        );
                    }
                }
                Item::Nominal(nominals) => {
                    for (name, _) in nominals.entries() {
                        parameters.insert(
                            name.faithful().to_string(),
                            nominals.universe_context().parameter_count,
                        );
                    }
                }
            }
        }

        let mut checked = 0;
        for target in pinned {
            let Some(count) = parameters.get(target) else {
                continue;
            };
            checked += 1;
            assert_eq!(
                *count, 0,
                "{target} is universe-polymorphic; every literal byte will mint levels"
            );
        }
        // Without this the test passes vacuously if a registered name is renamed.
        assert!(
            checked == pinned.len(),
            "found only {checked} of the pinned registered names; \
             the names this pins have moved"
        );
    });
}

/// Whether a root module of `/std` performs anything. `curios-prelude/README.md`'s "`/std` is laid out by purity, subject and kind" gives the rule.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Purity {
    /// It performs nothing: no declaration of it reaches a host operation, so a type or a proof may run any of it.
    Pure,
    /// It describes work the host does.
    Performs,
}

/// Whether each root module `/std` declares performs anything: the one statement of which ones do not.
const PURITY: &[(&str, Purity)] = &[
    ("tests", Purity::Pure),
    ("ops", Purity::Pure),
    ("Eq", Purity::Pure),
    ("Bool", Purity::Pure),
    ("Ord", Purity::Pure),
    ("Hash", Purity::Pure),
    ("Show", Purity::Pure),
    ("Spell", Purity::Pure),
    ("Monad", Purity::Pure),
    ("Tuple", Purity::Pure),
    ("Byte", Purity::Pure),
    ("Nat", Purity::Pure),
    ("Int", Purity::Pure),
    ("Flt", Purity::Pure),
    ("Bytes", Purity::Pure),
    ("Char", Purity::Pure),
    ("Str", Purity::Pure),
    ("List", Purity::Pure),
    ("Vec", Purity::Pure),
    ("Option", Purity::Pure),
    ("Result", Purity::Pure),
    ("State", Purity::Pure),
    ("Map", Purity::Pure),
    ("Set", Purity::Pure),
    ("Path", Purity::Pure),
    ("Parse", Purity::Pure),
    ("internal", Purity::Performs),
    ("Io", Purity::Performs),
    ("Channel", Purity::Performs),
    ("Box", Purity::Performs),
    ("Try", Purity::Performs),
    ("Program", Purity::Performs),
    ("Test", Purity::Performs),
    ("Fmt", Purity::Performs),
    ("time", Purity::Performs),
    ("rand", Purity::Performs),
    ("Uuid", Purity::Performs),
    ("Proc", Purity::Performs),
    ("fs", Purity::Performs),
    ("File", Purity::Performs),
    ("Tty", Purity::Performs),
    ("Serial", Purity::Performs),
    ("tcp", Purity::Performs),
    ("Json", Purity::Pure),
    ("Toml", Purity::Pure),
    ("Html", Purity::Pure),
    ("http", Purity::Performs),
    ("Cli", Purity::Performs),
    ("Tui", Purity::Performs),
];

/// Whether the root module `root` performs anything.
fn purity(root: &str) -> Purity {
    PURITY
        .iter()
        .find(|(name, _)| *name == root)
        .map(|(_, purity)| *purity)
        .unwrap_or_else(|| panic!("/std/{root} is not classified"))
}

/// The root modules `/std`'s header declares.
fn declared_root_modules() -> Vec<String> {
    let header = fs::read_to_string(std_directory().join("lib.crs"))
        .expect("the standard library's header reads");
    let header = header
        .parse::<curios_surface::Module>()
        .unwrap_or_else(|error| panic!("the standard library's header parses: {error:?}"));

    header
        .items
        .iter()
        .filter_map(|item| match item {
            TopItem::Mod(declared) => Some(declared.label.as_str().to_string()),
            _ => None,
        })
        .collect()
}

/// Every root module the header declares is classified, and the roster names no module the header does not declare.
#[test]
fn every_root_module_of_std_is_classified() {
    let mut declared = declared_root_modules();
    declared.sort();
    let mut rostered = PURITY
        .iter()
        .map(|(name, _)| name.to_string())
        .collect::<Vec<_>>();
    rostered.sort();

    assert_eq!(declared, rostered);
}

/// Whether `term` holds a host call.
fn holds_a_host_call(term: &Term) -> bool {
    let mut seen = HashSet::new();

    term.try_walk(
        &mut seen,
        |seen: &mut HashSet<Term>, term| match &**term {
            Subterm::Foreign(_) => ControlFlow::Break(()),
            _ if !seen.insert(term.clone()) => ControlFlow::Continue(Enter::Skip(())),
            _ => ControlFlow::Continue(Enter::Descend),
        },
        |_, _, _| (),
    )
    .is_break()
}

/// A declaration of a module that performs nothing reaches no host operation, through anything it names, the items of a recursive group and the registry entries a declaration's elaboration consumed included, across `/sys` and `/std`: a `foreign` declaration is the one place a host call stands.
#[test]
fn a_declaration_of_a_pure_module_reaches_no_foreign_declaration() {
    let [(_, sys), (_, std)] = restore_archives();
    let modules = [sys.core(), std.core()];

    // Each declaration's reach read the other way, and the declarations a host call stands in.
    let mut reached_by = HashMap::<Global, Vec<Global>>::new();
    let mut performing = HashMap::<Global, Option<Global>>::new();
    let mut queue = Vec::new();
    for definitions in modules {
        for item in &definitions.items {
            let host = item
                .definitions()
                .iter()
                .any(|definition| holds_a_host_call(&definition.body));
            let reached = definitions.reaches(item);
            for name in item.declared_names() {
                for target in &reached {
                    reached_by.entry(*target).or_default().push(name);
                }
                if host && performing.insert(name, None).is_none() {
                    queue.push(name);
                }
            }
        }
    }
    // Every declaration that reaches one, beside the declaration it reaches it through.
    while let Some(name) = queue.pop() {
        for reacher in reached_by.get(&name).into_iter().flatten() {
            if !performing.contains_key(reacher) {
                performing.insert(*reacher, Some(name));
                queue.push(*reacher);
            }
        }
    }

    let mut crossings = Vec::new();
    for item in &std.core().items {
        let pure = item.definitions().first().is_some_and(|definition| {
            matches!(
                definition.island.segments(),
                [std, root, ..] if std == "std" && purity(root) == Purity::Pure
            )
        });
        if !pure {
            continue;
        }
        for name in item.declared_names() {
            let mut chain = vec![name.faithful().to_string()];
            let mut at = performing.get(&name);
            while let Some(Some(next)) = at {
                chain.push(next.faithful().to_string());
                at = performing.get(next);
            }
            if performing.contains_key(&name) {
                crossings.push(chain.join(" -> "));
            }
        }
    }

    assert!(crossings.is_empty(), "{crossings:#?}");
}

/// Every root's, because an erased form is its item's: no root holds another's, so each has its own to show.
#[test]
fn every_root_holds_its_items_erased_forms() {
    with_prelude(|prelude| {
        for root in prelude {
            assert!(!root.erased().is_empty());
        }
    });
}

/// A registry entry is keyed by a name an item of its root declares, its type former. The row of a structure or an inductive is that item's to register where it is erased (`curios-erase`), so an entry no item declared would have no erased form to hold its row.
#[test]
fn every_registry_entry_is_keyed_by_a_name_an_item_of_its_root_declares() {
    with_prelude(|prelude| {
        for root in prelude {
            let core = root.core();
            let declared = core
                .items
                .iter()
                .flat_map(Item::declared_names)
                .collect::<BTreeSet<_>>();
            for name in core
                .induct_decls()
                .keys()
                .chain(core.struct_decls().keys())
                .chain(core.concepts().keys())
            {
                assert!(
                    declared.contains(name),
                    "no item declares {}",
                    name.faithful()
                );
            }
        }
    });
}

/// A name under `/test`, which a test compilation alone links, is held where `/test` is compiled (`curios-pipeline`'s `standard::tests`).
#[test]
fn every_syntax_target_under_a_root_is_present_after_restore() {
    with_prelude(|prelude| {
        let names = items(prelude)
            .flat_map(Item::declared_names)
            .collect::<BTreeSet<_>>();
        let roots = root_prefixes();
        for target in SYNTAX
            .targets()
            .filter(|target| roots.iter().any(|root| target.qualifier().is_within(root)))
        {
            assert!(
                names.contains(&Global::Authored(target.qualifier())),
                "missing syntax target {}",
                target.symbol()
            );
        }
    });
}

#[test]
fn every_registered_concept_declares_its_method_after_restore() {
    with_prelude(|prelude| {
        for target in SYNTAX.concept_fields() {
            let name = Global::Authored(target.concept.qualifier());
            // A concept's methods are named by its record's fields.
            let record = prelude
                .iter()
                .find_map(|root| {
                    root.core().concept(root.places(), &name)?;
                    root.core().struct_decl(root.places(), &name)
                })
                .unwrap_or_else(|| panic!("missing concept {}", target.concept.symbol()));
            assert!(
                method_labels(record).contains(&target.field),
                "concept {} does not declare {}",
                target.concept.symbol(),
                target.field
            );
        }
    });
}

/// Every arm of the restored standard library states one mark per payload binder. A function's, a function type's and a constructor signature's marks are their telescope's entries' own, so they have nothing to drift from; an arm's are the one vector left beside its binders. The pairing is a construction invariant — `InductArm::new` asserts it — and the archive restores exactly the constructor-built value its build wrote, so this is not a defense the compiler runs but the one place the claim is checked against the real image, once per test run.
#[test]
fn the_restored_prelude_pairs_every_arm_mark_with_its_binder() {
    with_prelude(|prelude| {
        let drifted = Rc::new(RefCell::new(Vec::new()));
        let inspected = Rc::new(Cell::new(0usize));
        let (sink, counter) = (Rc::clone(&drifted), Rc::clone(&inspected));
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                if let Subterm::Match(Match {
                    cases: Cases::Induct { cases, .. },
                    ..
                }) = &**term
                {
                    counter.set(counter.get() + 1);
                    if !cases
                        .iter()
                        .all(|(_, arm)| arm.plicities().len() == arm.arity())
                    {
                        sink.borrow_mut().push(term.clone());
                    }
                }
                None
            }),
        );

        for definition in items(prelude).flat_map(Item::definitions) {
            definition.type_.traverse(&mut visit);
            definition.body.traverse(&mut visit);
        }

        assert!(inspected.get() > 0, "the walk reached no match");
        assert!(
            drifted.borrow().is_empty(),
            "drifted mark vectors survived into the archive: {:?}",
            drifted.borrow()
        );
    });
}

/// The function literals of the restored standard library state which of their binders bind a proof, and none says so of a binder whose type is written as a sort. A mark lives in what a scope remembers of its binder, which is no part of a term, so a pass that rebuilt a scope from its body alone, or a cabinet that kept a term without its labels, would leave every binder unmarked and change no verdict: the closed machine would evaluate each proof it binds, and only a floor would say so. A binder at a sort binds a type, which the machine evaluates, and is the one kind this can name without the classifier.
#[test]
fn the_restored_prelude_marks_binders_and_none_at_a_sort() {
    with_prelude(|prelude| {
        let at_a_sort = Rc::new(RefCell::new(Vec::new()));
        let marked = Rc::new(Cell::new(0usize));
        let (sink, counter) = (Rc::clone(&at_a_sort), Rc::clone(&marked));
        let mut visit = Visit::rewriting(
            |_, _| None,
            Box::new(move |_, term: &Term| {
                if let Subterm::Func(function) = &**term {
                    let mut telescope = &function.telescope;
                    while let Telescope::Cons(_, type_, rest, ..) = telescope {
                        if rest.binds_proof(0) {
                            counter.set(counter.get() + 1);
                            if matches!(&**type_, Subterm::Sort(_)) {
                                sink.borrow_mut().push(term.clone());
                            }
                        }
                        telescope = rest.body();
                    }
                }
                None
            }),
        );

        for definition in items(prelude).flat_map(Item::definitions) {
            definition.type_.traverse(&mut visit);
            definition.body.traverse(&mut visit);
        }

        assert!(
            marked.get() > 0,
            "no binder of the library says it binds a proof"
        );
        assert!(
            at_a_sort.borrow().is_empty(),
            "{} binders at a sort say they bind a proof, the first under: {:?}",
            at_a_sort.borrow().len(),
            at_a_sort.borrow().first()
        );
    });
}

/// A `/sys` former is levelled by its one argument alone. `List`, `Io` and `Box` each take `T: Type u` to `Type u`, and the level its body's binder is written at, bounded only by `u`, would be a second parameter every occurrence of the former mints. `UniverseSolver::identify_bounded_choices` identifies a chosen level bounded only by one other chosen level with it, so each takes one.
#[test]
fn every_sys_former_takes_one_universe_parameter() {
    with_prelude(|prelude| {
        let counts = items(prelude)
            .flat_map(|item| item.definitions())
            .map(|definition| (definition.name.faithful().to_string(), definition))
            .filter(|(name, _)| SYS_FORMERS.contains(&name.as_str()))
            .map(|(name, definition)| (name, definition.universe_context.parameter_count))
            .collect::<BTreeMap<_, _>>();

        assert_eq!(
            counts.len(),
            SYS_FORMERS.len(),
            "a former is missing: {counts:?}"
        );
        assert!(counts.values().all(|&count| count == 1), "{counts:?}");
    });
}

// The `/sys` formers reduction turns into an intrinsic former, which carries no level.
const SYS_FORMERS: [&str; 3] = ["/sys/List/List", "/sys/Io/Io", "/sys/Box/Box"];
