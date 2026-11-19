//! The certifier's record a compiled unit files beside its definitions: what it covers, what it says, what it read, what an item-level recompile keeps of the baseline's, and the termination flags it marks on a sealed program; and that what the certifier alone refuses stops the fold's products.

use {
    super::test_support::*,
    crate::*,
    curios_common::Qualifier,
    curios_core::{Enter, Global, Instance, Reads, Subterm, Term, Totality},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_prelude::with_prelude,
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

/// The name of the declaration `unit` calls `symbol`.
fn named(unit: &Unit, symbol: &str) -> Global {
    unit.core()
        .items
        .iter()
        .flat_map(|item| item.declared_names())
        .find(|name| name.faithful().to_string().ends_with(&format!("/{symbol}")))
        .unwrap_or_else(|| panic!("the unit defines no `{symbol}`"))
}

/// The certifier's classification of the definition `unit` calls `symbol`.
fn classified(unit: &Unit, symbol: &str) -> Option<Totality> {
    unit.certification().totality(&named(unit, symbol))
}

/// The top-level names `term` mentions outside a nominal value's parameters, each node visited once. The certifier counts a record literal's or a constructor application's parameters and never types them — the value's type is taken from them as written, and conversion compares them — so a name standing only there is neither typed nor unfolded.
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
                Subterm::Instance(Instance { name, .. }) => {
                    into.insert(*name);
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

/// Whether `unit`'s record answers for every name its module holds, as the certifier reads a mounted one per name.
fn answers_for_every_name(unit: &Unit) -> bool {
    let core = unit.core();
    core.items.places().all(|place| {
        core.items
            .names(place)
            .into_iter()
            .all(|name| unit.certification().totality(&name).is_some())
    })
}

/// A unit the pipeline compiled carries the certifier's record of every definition it holds, each classified as the certifier's own closure found it.
#[test]
fn a_compiled_unit_files_the_certifiers_record_of_every_definition() {
    let unit = unit_of(SOURCE);

    assert!(answers_for_every_name(&unit));
    assert_eq!(classified(&unit, "double"), Some(Totality::Total));
    assert_eq!(classified(&unit, "spin"), Some(Totality::Partial));
    assert_eq!(classified(&unit, "twice"), Some(Totality::Total));
}

/// An item-level recompile's walk is handed the baseline whole and adopts the verdicts that stand, so its record covers the unit — a declaration the edit did not reach classified, and read, as the walk that filed the baseline left it, and one that read the edit judged again.
#[test]
fn an_item_level_recompile_keeps_the_baselines_record_for_what_it_reuses() {
    let baseline = unit_of(SOURCE);
    let edited = SOURCE.replace("double(2)", "double(3)");

    let recompiled = recompile_over(&edited, &baseline).expect("the edit compiles");

    assert!(reuses_body(&baseline, &recompiled, "double"));
    assert!(!reuses_body(&baseline, &recompiled, "twice"));
    assert!(answers_for_every_name(&recompiled));
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

/// The certifier reads along the graph elaboration built, over the whole prelude. Every name a definition mentions where the certifier types is read — its signature where the name is typed, its body where a type naming it is typed by reducing it first — a group's own members excepted, which it types as the locals it opens, and a nominal value's parameters excepted, which it never types ([`typed_mentions`]); and nothing is read that the definition's item does not [reach](curios_core::Definitions::reaches) transitively — the graph an item-level recompile invalidates along, a declaring item reaching what its registry entry names. The first half is what catches a way of consulting the environment that records nothing, and the second what makes the record no coarser than the graph it refines. An arm no reduction takes is typed by no rule either: a name mentioned there alone is read only as the classification reads it, its signature, and this walk does not tell such an arm from one that is typed, so the arm is left out. Mutation-checked: recording no body read leaves the first half with 1 511 unread mentions.
#[test]
fn the_prelude_reads_along_the_graph_its_definitions_reach() {
    with_prelude(|prelude| {
        let mut graph: BTreeMap<Global, BTreeSet<Global>> = BTreeMap::new();
        for unit in prelude {
            for item in &unit.core().items {
                let reaches = unit.core().reaches(item);
                for name in item.declared_names() {
                    graph.insert(name, reaches.clone());
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
                let own = item.declared_names().into_iter().collect::<BTreeSet<_>>();
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
                            .map(|name| {
                                (
                                    definition.name.faithful().to_string(),
                                    name.faithful().to_string(),
                                )
                            }),
                    );
                    let reached = reached(&definition.name);
                    unreached.extend(
                        reads
                            .signatures
                            .iter()
                            .chain(&reads.bodies)
                            .filter(|name| !reached.contains(*name))
                            .map(|name| {
                                (
                                    definition.name.faithful().to_string(),
                                    name.faithful().to_string(),
                                )
                            }),
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

/// The fixed prelude is lent with the record its build's certification filed, and that record answers for every name each root holds — so a walk with the prelude in scope reads the certifier's verdict on every `/sys` and `/std` definition.
#[test]
fn the_restored_prelude_carries_a_record_covering_every_definition() {
    with_prelude(|prelude| {
        for unit in prelude {
            assert!(answers_for_every_name(unit));
        }
    });
}

/// A sealed program's functions are marked total from the certifier's records of everything it was erased from — the prelude's and the entry's own — and from nothing else: erasure marks nothing, so every mark here is one a record justified.
///
/// A record speaks of an item's evaluation, the functions nested in it included, and of no performance and no machinery ([`curios_anf::Role`]), so the check runs by role and by name: no function that performs a description or runs one is marked, though the crossing `Proc/args` reaches puts machinery in an item the prelude classifies total; the entry's total definition is marked with the loop it defines, and its partial one with its loop is not; and the prelude contributes marks at all. Mutation-checked: marking from no unit's record in scope fails the prelude's half, marking only the functions a name is bound to fails the nested loop's, and marking whatever the role fails the machinery's.
#[test]
fn a_sealed_program_marks_its_functions_from_the_certifiers_records() {
    let source = "use /std/{Nat, List, Proc};

let double(n: Nat) -> Nat =
    let halve(k: Nat, acc: Nat) -> Nat = match k | 0 => acc | j + 1 => halve(j, acc + 2) end;
    halve(n, 0);

let spin(n: Nat) -> Nat =
    let whirl(k: Nat) -> Nat = whirl(k);
    whirl(n);

let subject : /std/Nat = double(2) + spin(0);

let entry() -> /std/Io({}) =
    let _ = List/len(Proc/args()!);
    /std/Io/pure(());

entry()
";
    let mut functions = Vec::new();
    compile_with_prelude(
        DEFAULT_BUDGET,
        &entrypoint_of(source),
        &RootSource::none(),
        |stage| {
            if let Stage::Anf(module) = stage {
                functions = module
                    .functions()
                    .iter()
                    .flatten()
                    .map(|function| {
                        (
                            function.debug_name.clone().unwrap_or_default(),
                            function.role,
                            function.total,
                        )
                    })
                    .collect();
            }
        },
    )
    .expect("the program compiles");

    for (name, role, total) in &functions {
        assert!(
            !total || *role == curios_anf::Role::Evaluation,
            "{name}, a {role:?}, is marked total"
        );
    }
    assert!(
        functions
            .iter()
            .any(|(_, role, _)| *role == curios_anf::Role::Machinery),
        "the program runs no machinery to hold unmarked"
    );
    assert!(
        functions
            .iter()
            .any(|(name, _, total)| *total
                && (name.starts_with("/std/") || name.starts_with("/sys/"))),
        "no prelude function was marked"
    );

    let marked = |suffix: &str| {
        let named = functions
            .iter()
            .filter(|(name, _, _)| name.ends_with(suffix))
            .collect::<Vec<_>>();
        assert!(!named.is_empty(), "the program holds no {suffix}");
        named.iter().all(|(_, _, total)| *total)
    };
    assert!(
        marked("/double"),
        "the entry's total definition is not marked"
    );
    assert!(
        marked("halve"),
        "the loop the total definition defines is not marked"
    );
    assert!(!marked("/spin"), "the entry's partial definition is marked");
    assert!(
        !marked("whirl"),
        "the loop the partial definition defines is marked"
    );
}

/// What the declarations below open with: `Code`'s one constructor fixes its index at `Bool`, so an arm matching on `Code(X)` reads `x` as a `Bool`, which `x + 0` adds to nothing.
const CODE: &str = "induct Code: (Type) -> Type
| big(): (/std/Bool)
end

";

/// A refused declaration stops the compile the browser and every embedder run, though nothing the program runs reaches it, and so does a refused witness a declaration resolves: the unit is judged whole.
#[test]
fn a_refused_declaration_stops_the_compile_with_the_prelude() {
    for declarations in [
        "pub let f(X: Type, c: Code(X), x: X) -> X =
    match c
    | big() => x + 0
    end;",
        "concept Pick(A: Type): Type {
    pick(X: Type, c: Code(X), x: X) -> X,
}

satisfy Pick(/std/Nat) {
    pick(X, c, x) = match c | big() => x + 0 end,
}

pub let chosen(c: Code(/std/Nat)) -> /std/Nat = Pick/pick(@/std/Nat, /std/Nat, c, 1);",
    ] {
        let source = format!("{CODE}{declarations}\n\n/std/print(\"\")\n");

        let refusal = compile(&source).expect_err("the declaration is refused");

        assert!(
            refusal.contains("wanted: `/std/ops/Add(/std/Bool)`"),
            "{refusal}"
        );
    }
}

/// A refused test stops the test program of its unit whether or not the selection schedules it: the unit compiled tested holds every test it declares, so each is judged with the unit.
#[test]
fn a_refused_test_stops_its_test_program_whatever_is_selected() {
    let source = format!(
        "{CODE}test refused =
    let _g = (X: Type, c: Code(X), x: X) =>
        match c
        | big() => x + 0
        end;
    /std/Test/assert(\"it holds\", true);

test held = /std/Test/assert(\"it holds\", true);

/std/print(\"\")
"
    );
    let entrypoint = entrypoint_of(&source);
    let tested = |under: Qualifier| {
        Fold::new(DEFAULT_BUDGET, &[], None)
            .tests(
                &entrypoint,
                &RootSource::none().tested(),
                vec![under],
                |_| {},
                |_| {},
            )
            .map(|_| ())
            .expect_err("`refused` is refused")
            .to_string()
    };

    for under in [Qualifier::empty(), Qualifier::from(["held"])] {
        let refusal = tested(under);
        assert!(
            refusal.contains("wanted: `/std/ops/Add(/std/Bool)`"),
            "{refusal}"
        );
    }
}
