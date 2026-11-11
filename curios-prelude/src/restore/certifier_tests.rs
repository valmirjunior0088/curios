//! The certifier over the restored prelude: where it disagrees with the elaborator, its memos changing no verdict, a declared type typed at each value of a parameter, and the inventories of the prelude's universe parameters and their variances.

use {
    super::{test_support::*, *},
    curios_cert::{Error, certify_definitions, infer, test_support::*},
    curios_core::{
        Exhaustion, Free, FuncType, Global, Intrinsic, Item, Nat, Quality, Reducer, Subterm,
        Telescope, Term, Variance, Zonked, instantiate_qualities,
    },
    curios_elab::DEFAULT_BUDGET,
    curios_module::SYNTAX,
    curios_surface::Spellable,
    std::collections::BTreeMap,
};

/// A term's printed head, clipped — enough to tell one refusal's shape from another's without pasting a standard-library type into a tally.
fn head(term: &Term) -> String {
    let rendered = term.faithful().to_string();
    let rendered = rendered.split_whitespace().collect::<Vec<_>>().join(" ");

    match rendered.char_indices().nth(44) {
        Some((cut, _)) => format!("{}…", &rendered[..cut]),
        None => rendered,
    }
}

/// The class a refusal is tallied under.
///
/// Deliberately mechanical: the variant, plus for a mismatch the two sides' printed heads. Naming classes like "index inversion" here would be inventing categories from a heuristic, which is how this project's wrong answers get made — the point of the tally is to let the categories fall out of it.
fn class(error: &Error) -> String {
    match error {
        Error::Mismatch { inferred, expected } => {
            format!("Mismatch  {}  vs  {}", head(inferred), head(expected))
        }
        other => {
            let rendered = format!("{other:?}");

            rendered
                .split(['(', ' ', '{'])
                .next()
                .unwrap_or("?")
                .to_string()
        }
    }
}

/// Every item the certifier refuses across the whole fixed prelude, tallied by class.
///
/// Not an assertion — a measurement, run on demand. A walk that stopped at the first refusal would say nothing about what lies past it; this one walks to the end with each verdict independent of the others (see `curios_cert::certify_definitions`), which is what makes the classes countable rather than discovered one build at a time.
///
/// The restored image is the subject, named directly: a compiled module carries only its own items, so no fixture reaches the standard library by compiling. What asserts the prelude is acceptable is `curios-prelude`'s own build script, which runs this same walk and panics on the first refusal; this is the inventory beside it.
///
/// An abort rather than a tally is a finding, not noise: nothing here is wrapped in a catch, because a certifier that aborts is a certifier to fix, and raising `RUST_MIN_STACK` would conceal one rather than fix it.
#[test]
#[ignore = "inventory: measures where the certifier disagrees rather than asserting"]
fn certifier_disagreements() {
    with_prelude(|prelude| {
        let mut verdicts = Vec::new();

        // Each root against the roots before it, which is the environment it was elaborated in: walking `/std` from an empty one would tally a refusal per intrinsic carrier it wraps and say nothing about the certifier.
        for (index, root) in prelude.iter().enumerate() {
            let zonked = Zonked::project(root.core()).expect("a restored prelude root is zonked");
            let globals = certified(&prelude[..index]);
            let (rechecked, _) = certify_definitions(&zonked, DEFAULT_BUDGET, &globals, SYNTAX);
            verdicts.extend(rechecked.verdicts);
        }

        let mut tally: BTreeMap<String, usize> = BTreeMap::new();
        for verdict in &verdicts {
            *tally.entry(class(&verdict.error)).or_default() += 1;
        }

        println!(
            "\n=== {} refusals over {} prelude items ===",
            verdicts.len(),
            items(prelude).count()
        );
        for (class, count) in &tally {
            println!("  {count:>4}  {class}");
        }
        for verdict in &verdicts {
            let name = match &verdict.name {
                Some(name) => name.faithful().to_string(),
                None => "<entrypoint>".to_string(),
            };
            println!("        {name}  —  {}", class(&verdict.error));
        }
    });
}

/// Memoization is an evaluation strategy exactly as long as switching it off changes no *semantic* verdict. This runs the whole-prelude walk both ways and requires the verdict lists identical.
///
/// **The semantic half is what this is for, rather than what it happens to cover.** A term-keyed memo hit spends nothing and mints nothing, so an uncached walk spends at least as much as a cached one, the two may reach different *exhaustion* points, and a binder opened after a hit takes another identity in each; `curios-cert`'s `spend` module argues why that is the whole of what the design gives up. Comparing verdicts at one budget, over a corpus where nothing exhausts, is therefore exactly the surviving property — acceptance, and refusals that are not exhaustion, are budget-independent and must agree. Asserting equal exhaustion points would assert what the design gives up.
///
/// The prelude is the subject because the property needs a large body of real terms to mean anything, and this is the only one the workspace has. It runs with the suite, since the memo decision rests on it and nothing else holds it: walking the prelude twice takes one test thread what a part of the library's read-back does.
#[test]
fn certifier_memo_parity() {
    with_prelude(|prelude| {
        for (index, root) in prelude.iter().enumerate() {
            let zonked = Zonked::project(root.core()).expect("a restored prelude root is zonked");
            let globals = certified(&prelude[..index]);
            assert_eq!(
                certify_definitions(&zonked, DEFAULT_BUDGET, &globals, SYNTAX)
                    .0
                    .verdicts,
                recheck_definitions_verdicts_uncached(&zonked, DEFAULT_BUDGET, &globals, SYNTAX),
            );
        }
    });
}

/// The closed values a parameter at `domain`, reduced, is put at: both cases of a `Bool` and the first two of a `Nat`, each as its literal and as a spelling that computes to it and is no value as it stands. None for a parameter at any other type.
fn values_at(domain: &Term) -> Vec<Term> {
    let spelled = |literal: Term, identity: fn(Term) -> Intrinsic| {
        [literal.clone(), Term::intrinsic(identity(literal))]
    };

    match &**domain {
        Subterm::Intrinsic(Intrinsic::BoolType) => [false, true]
            .into_iter()
            .flat_map(|case| {
                spelled(Term::intrinsic(Intrinsic::Bool(case)), |literal| {
                    Intrinsic::BoolXor(literal, Term::intrinsic(Intrinsic::Bool(false)))
                })
            })
            .collect(),
        Subterm::Intrinsic(Intrinsic::NatType) => [0usize, 1]
            .into_iter()
            .flat_map(|case| {
                spelled(Term::intrinsic(Intrinsic::Nat(Nat::new(case))), |literal| {
                    Intrinsic::NatMul(literal, Term::intrinsic(Intrinsic::Nat(Nat::new(1usize))))
                })
            })
            .collect(),
        _ => Vec::new(),
    }
}

/// Each parameter's type as `telescope` states it, the parameters before it standing as binders of this walk's own.
fn domains(telescope: &Telescope<Term>) -> Vec<Term> {
    let mut cursor = telescope.cursor();
    let mut domains = Vec::new();
    while let Some((hint, domain)) = cursor.entry() {
        let index = domains.len() as u64;
        domains.push(domain);
        cursor.advance(Term::free_var(&Free::local(index, hint)));
    }

    domains
}

/// The function type `telescope` states with its parameter at `position` standing at `value` and that binder gone: every other parameter under its mark, typed as the parameters before it and `value` leave it.
fn at_a_value(telescope: &Telescope<Term>, position: usize, value: &Term) -> Term {
    let mut cursor = telescope.cursor();
    let mut kept = Vec::new();
    let mut kept_proofs = Vec::new();

    for (index, proof) in telescope.proofs().into_iter().enumerate() {
        let (hint, domain) = cursor.entry().expect("a telescope has an entry a mark");
        let mark = cursor.mark().expect("a telescope has a mark an entry");
        if index == position {
            cursor.advance(value.clone());
            continue;
        }
        // Bound again where the type is rebuilt, so the index is this walk's alone.
        let binder = Free::local(index as u64, hint);
        cursor.advance(Term::free_var(&binder));
        kept.push((mark, binder, domain));
        kept_proofs.push(proof);
    }
    let result = cursor.body().expect("a cursor past every entry");

    Subterm::FuncType(FuncType {
        telescope: Telescope::build_marked(kept, result).with_proofs(&kept_proofs),
    })
    .into()
}

/// What the certifier types at a variable it types at every value put in that variable's place: every declared type of the prelude, at each closed value of each of its `Bool` and `Nat` parameters ([`values_at`]), is typed by the certifier that accepted its root.
///
/// The elaborator writes such a type wherever it derives one — a `let` it leaves unannotated, a lambda's domain, a match's goal — and types none of them, so a declared type the certifier refuses at a value is a program it refuses after the elaborator accepted it. A refusal for the budget is no violation: the instance is one a compilation affords or does not.
///
/// **Over the prelude it holds what is written later, and shows nothing of the rule.** With the certifier typing every arm, one under a scrutinee that already is another case among them, it reports no instance either: no declared type of the prelude holds an arm that rests on a parameter's case. The programs known to break the property are `curios`'s `tests::matching::instance_tests`, and the arm rule's own fixtures are `curios-cert`'s.
#[test]
fn the_certifier_types_a_declared_type_at_each_value_of_a_parameter() {
    with_prelude(|prelude| {
        let mut typed = 0usize;
        let mut refused = Vec::new();

        for (index, root) in prelude.iter().enumerate() {
            let zonked = Zonked::project(root.core()).expect("a restored prelude root is zonked");
            let globals = certified(&prelude[..index]);
            let (rechecked, mut certifier) =
                certify_definitions(&zonked, DEFAULT_BUDGET, &globals, SYNTAX);
            assert!(rechecked.verdicts.is_empty(), "{:?}", rechecked.verdicts);

            for definition in root.core().items.iter().flat_map(Item::definitions) {
                let Subterm::FuncType(FuncType { telescope }) = &*definition.type_ else {
                    continue;
                };
                for (position, domain) in domains(telescope).into_iter().enumerate() {
                    certifier.restore_budget();
                    let Ok(domain) = certifier.reduce(domain) else {
                        continue;
                    };
                    for value in values_at(&domain) {
                        let instance = at_a_value(telescope, position, &value);
                        // At each sort the declaration leaves to its caller, as the walk that certified it judges it.
                        let universes = &definition.universe_context;
                        for sorts in Quality::assignments(universes.quality_count) {
                            certifier.restore_budget();
                            certifier.assume_universes(universes);
                            typed += 1;
                            match infer(&mut certifier, &instantiate_qualities(&instance, &sorts)) {
                                Ok(_) => {}
                                Err(error) if error.is_exhausted() => {}
                                Err(error) => refused.push(format!(
                                    "{} at {value:?} in place of parameter {}: {error:?}",
                                    definition.name.faithful(),
                                    position + 1,
                                )),
                            }
                        }
                    }
                }
            }
        }

        assert!(
            refused.is_empty(),
            "{} of {typed} instances refused:\n{}",
            refused.len(),
            refused.join("\n"),
        );
    });
}

/// Every prelude definition's finalized universe scheme, printed rather than asserted: its parameter count, then its constraints and its type. A change to how conversion identifies levels, or to where a level settles, moves a signature without refusing anything, and the archive build reports only refusals. A count alone hides a move that keeps it, two parameters merged where a third appears, so the lines of two commits are compared by name.
#[test]
#[ignore = "inventory: measures the prelude's universe polymorphism rather than asserting it"]
fn universe_parameter_census() {
    with_prelude(|prelude| {
        for item in items(prelude) {
            for definition in item.definitions() {
                let context = &definition.universe_context;
                let constraints = context
                    .constraints
                    .iter()
                    .map(|constraint| {
                        format!(
                            "{} ≤ {}",
                            constraint.lower.faithful(),
                            constraint.upper.faithful()
                        )
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                let type_ = definition.type_.faithful().to_string().replace('\n', " ");

                println!(
                    "{}\t{}\t{constraints}\t{type_}",
                    definition.name.faithful(),
                    context.parameter_count
                );
            }
        }
    });
}

/// Which universe parameters of the prelude's families are irrelevant, printed rather than asserted: one line per family that takes one, `*` for an irrelevant level and `=` for an invariant one, as the archive carries the vector `curios-analysis`'s `variance_vectors` computed. Counted.
#[test]
#[ignore = "inventory: measures which universe parameters of the prelude's families are irrelevant rather than asserting it"]
fn family_variance_inventory() {
    with_prelude(|prelude| {
        let families = prelude
            .iter()
            .flat_map(|root| {
                let core = root.core();
                let inducts = core.induct_decls().into_iter().map(|(name, declaration)| {
                    let count = declaration.universe_context.parameter_count;
                    (name, (0..count).map(|i| declaration.variance(i)).collect())
                });
                let structs = core.struct_decls().into_iter().map(|(name, declaration)| {
                    let count = declaration.universe_context.parameter_count;
                    (name, (0..count).map(|i| declaration.variance(i)).collect())
                });
                inducts.chain(structs).collect::<Vec<_>>()
            })
            .collect::<BTreeMap<Global, Vec<Variance>>>();

        for (name, vector) in families.iter().filter(|(_, vector)| !vector.is_empty()) {
            let spelled = vector
                .iter()
                .map(|variance| match variance {
                    Variance::Irrelevant => '*',
                    Variance::Invariant => '=',
                })
                .collect::<String>();
            println!("{}\t{spelled}", name.faithful());
        }

        let parameters = families.values().map(Vec::len).sum::<usize>();
        let irrelevant = families
            .values()
            .flatten()
            .filter(|variance| **variance == Variance::Irrelevant)
            .count();
        println!(
            "\n=== {} families, {} with no universe parameter; the other {} carry {parameters} ===",
            families.len(),
            families.values().filter(|vector| vector.is_empty()).count(),
            families
                .values()
                .filter(|vector| !vector.is_empty())
                .count(),
        );
        println!("  {irrelevant:>4}  irrelevant");
        println!("  {:>4}  invariant", parameters - irrelevant);
    });
}

/// Which universe parameters of the prelude's definitions their values hold, printed rather than asserted: one line per non-recursive definition that takes one, `*` for an irrelevant level and `=` for an invariant one, as its unit files the vector `curios-elab`'s `definition_variances` read where the definition's levels closed — a vector holding no irrelevant level filed as none, and spelled as every level invariant. A recursive member files none and is counted apart. Counted.
#[test]
#[ignore = "inventory: measures which universe parameters of the prelude's definitions their values hold rather than asserting it"]
fn definition_variance_inventory() {
    with_prelude(|prelude| {
        let mut definitions = 0;
        let mut polymorphic = 0;
        let mut recursive = 0;
        let mut parameters = 0;
        let mut irrelevant = 0;
        let filed = prelude
            .iter()
            .flat_map(|root| root.core().items.iter().map(move |item| (root, item)));
        for (root, item) in filed {
            let Item::Let(definition) = item else {
                recursive += item
                    .definitions()
                    .iter()
                    .filter(|definition| definition.universe_context.parameter_count > 0)
                    .count();
                continue;
            };
            definitions += 1;
            let count = definition.universe_context.parameter_count;
            if count == 0 {
                continue;
            }
            polymorphic += 1;
            parameters += count;
            let spelled = match root.traces().classified(&definition.name) {
                Some(classified) if !classified.vectors.variances.is_empty() => classified
                    .vectors
                    .variances
                    .iter()
                    .map(|variance| match variance {
                        Variance::Irrelevant => '*',
                        Variance::Invariant => '=',
                    })
                    .collect::<String>(),
                _ => "=".repeat(count),
            };
            irrelevant += spelled.chars().filter(|level| *level == '*').count();
            println!("{}\t{spelled}", definition.name.faithful());
        }

        println!(
            "\n=== {definitions} definitions outside a group, {polymorphic} of them carrying {parameters} universe parameters; {recursive} recursive members take one and carry no vector ==="
        );
        println!("  {irrelevant:>4}  irrelevant");
        println!("  {:>4}  invariant", parameters - irrelevant);
    });
}
