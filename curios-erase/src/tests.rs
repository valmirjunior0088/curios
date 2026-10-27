use {
    crate::*,
    curios_abi::{ForeignFunction, WireReturn, WireSignature, WireType},
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module, certify_program},
    curios_core::Zonkable,
    curios_elab::DEFAULT_BUDGET,
    curios_utilities::{Plicity, Qualifier, grown},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> curios_core::Global {
    curios_core::Global::Authored(Qualifier::from([path]))
}

/// A binder no fixture spells, minted far above where the kernel mints: a process-wide counter keeps two in one test apart.
fn fresh(hint: Option<&str>) -> curios_core::Free {
    static MINTED: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(1_000_000);

    curios_core::Free::local(
        MINTED.fetch_add(1, std::sync::atomic::Ordering::Relaxed),
        hint,
    )
}

/// `unit` certified with nothing in scope and erased through the kernel that certified it, as the driver erases a unit it compiles.
fn erase_alone(unit: &curios_core::Zonked<curios_core::Module>) -> Result<ErasedUnit, Error> {
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_unit(&mut kernel, Erasures::nothing(), unit)
}

/// [`erase_alone`] over `baseline`.
fn erase_over(
    unit: &curios_core::Zonked<curios_core::Module>,
    baseline: ErasedBaseline<'_>,
) -> Result<ErasedUnit, Error> {
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_unit_over(&mut kernel, Erasures::nothing(), unit, baseline)
}

/// A top-level definition's identity, from the path a test writes — the same name [`definition`] declares it under. Fixture-only.
fn global(path: &str) -> curios_core::Free {
    curios_core::Free::global(Qualifier::from([path]))
}

fn nat_lit(n: usize) -> curios_core::Term {
    curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(curios_core::Nat::new(n)))
}

fn definition(name: &str, type_: curios_core::Term, body: curios_core::Term) -> curios_core::Item {
    curios_core::Item::Let(Box::new(curios_core::Definition {
        name: curios_core::Global::Authored(Qualifier::from([name])),
        kind: curios_core::DefinitionKind::Authored,
        universe_context: curios_core::UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: curios_core::Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        value_sort: None,
        type_,
        body,
    }))
}

/// A program of `items` closing with `body`, at no stated type yet — see [`stating`].
fn module(items: Vec<curios_core::Item>, body: curios_core::Term) -> curios_core::Program {
    curios_core::Program {
        module: curios_core::Module {
            mounts: Vec::new(),
            items: items.into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        entry: curios_core::Entrypoint { body, type_: None },
    }
}

fn zonked<T: Zonkable>(value: &T) -> curios_core::Zonked<T> {
    curios_core::Zonked::project(value).expect("the fixture is zonked")
}

/// `program` with its entry stating `type_`, which is what the entry's body is sealed at.
fn stating(program: &curios_core::Program, type_: curios_core::Term) -> curios_core::Program {
    let mut program = program.clone();
    program.entry.type_ = Some(type_);
    program
}

/// `program` erased with nothing in scope, its entry stating `type_`.
fn try_erase(
    program: &curios_core::Program,
    type_: curios_core::Term,
) -> Result<curios_anf::Module, Error> {
    let program = zonked(&stating(program, type_));
    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_program(&program, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(
        rechecked.verdicts.is_empty(),
        "the fixture is certified: {:?}",
        rechecked.verdicts
    );

    erase_program(&mut kernel, Erasures::nothing(), &program).map(ErasedProgram::into_module)
}

fn erase(program: &curios_core::Program, type_: curios_core::Term) -> curios_anf::Module {
    try_erase(program, type_).expect("the program erases")
}

/// The recorded payload row — field hint and carrier shape — of the one constructor of the single-constructor family whose debug name ends in `name`. The arena is what erasure wrote; the header the printer renders from it is a second spelling, and a test that scraped that spelling would pass by matching nothing if it moved.
fn payload(module: &curios_anf::Module, name: &str) -> Vec<(&'static str, curios_anf::FieldShape)> {
    let family = module
        .families()
        .iter()
        .find(|family| {
            family
                .debug_name
                .as_deref()
                .is_some_and(|debug_name| debug_name.ends_with(name))
        })
        .unwrap_or_else(|| panic!("the fixture registers {name}"));
    let [constructor] = family.constructors[..] else {
        panic!("{name} has exactly one constructor");
    };

    module
        .constructor(constructor)
        .expect("the constructor is registered")
        .fields
        .iter()
        .map(|field| {
            let hint = match field.debug_name.as_deref() {
                Some("x") => "x",
                other => panic!("unexpected field hint {other:?}"),
            };
            (hint, field.shape)
        })
        .collect()
}

/// Every `Rhs` the erased module binds, in statement order. A test whose subject is what erasure *built* asks this rather than the printout: a spelling belongs to the printer, and an anchor on one can silently match nothing, where a missing shape here is a loud failure.
fn bound(module: &curios_anf::Module) -> impl Iterator<Item = &curios_anf::Rhs> {
    module
        .statements()
        .iter()
        .flatten()
        .filter_map(|statement| match statement {
            curios_anf::Statement::Let { rhs, .. } => Some(rhs),
            _ => None,
        })
}

#[test]
fn a_scalar_expression_erases_in_evaluation_order() {
    let x_binder = fresh(Some("x"));
    // let x = 2; x + 3
    let body = curios_core::Term::let_(
        &x_binder,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(2),
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
            curios_core::Term::free_var(&x_binder),
            nat_lit(3),
        )),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0 = Operation NatAdd [Nat(2), Nat(3)]
  Return ~v0
"
    );
}

#[test]
fn bool_and_byte_keep_their_shapes() {
    let b = fresh(Some("b"));
    let (held, p, proof) = (fresh(Some("b")), fresh(Some("P")), fresh(Some("p")));
    // The bound a narrowing carries is `Holds(7 < 256)`, so the registry's `Holds` is declared, as a proposition its proof inhabits whatever it holds of.
    let true_ = curios_core::Term::func_type(
        [
            (p, curios_core::Term::prop()),
            (proof, curios_core::Term::free_var(&p)),
        ],
        curios_core::Term::free_var(&p),
    );
    let holds = curios_core::Item::Let(Box::new(curios_core::Definition {
        name: curios_core::Global::Authored(Qualifier::from(["std", "Bool", "Holds"])),
        ..match definition(
            "Holds",
            curios_core::Term::func_type(
                [(
                    held,
                    curios_core::Term::intrinsic(curios_core::Intrinsic::BoolType),
                )],
                // Quantifying over `Prop` lands a level up.
                curios_core::Term::prop_at(curios_core::Level::constant(1)),
            ),
            curios_core::Term::func(
                [(
                    held,
                    curios_core::Term::intrinsic(curios_core::Intrinsic::BoolType),
                )],
                true_,
            ),
        ) {
            curios_core::Item::Let(definition) => *definition,
            curios_core::Item::Rec(_) => unreachable!("a fixture's definition is a `let`"),
        }
    }));
    let qed = curios_core::Term::func(
        [
            (p, curios_core::Term::prop()),
            (proof, curios_core::Term::free_var(&p)),
        ],
        curios_core::Term::free_var(&proof),
    );
    // Bool stays Bool-shaped and Byte stays Byte-shaped: no Nat carrier appears anywhere in the erased output.
    let body = curios_core::Term::let_(
        &b,
        curios_core::Term::intrinsic(curios_core::Intrinsic::BoolType),
        curios_core::Term::intrinsic(curios_core::Intrinsic::BoolAnd(
            curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(true)),
            curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(false)),
        )),
        // Erasure drops the narrowing's bound, which is the half this asserts.
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_to_byte(
            curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(curios_core::Nat::new(
                7usize,
            ))),
            qed,
        )),
    );
    let erased = erase(
        &module(vec![holds], body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::ByteType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Functions
    function ~f0$/std/Bool/Holds(~v0$b)
      Return Unit
entry
  Let ~v1$b = Operation BoolAnd [Bool(true), Bool(false)]
  Let ~v2 = Operation NatToByte [Nat(7)]
  Return ~v2
"
    );
}

#[test]
fn a_nat_spine_over_a_variable_erases_to_one_addition() {
    let items = vec![definition(
        "x",
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = curios_core::Term::intrinsic(curios_core::Intrinsic::Nat(curios_core::Nat::Succ(
        3u32.into(),
        curios_core::Term::free_var(&global("x")),
    )));
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0 = Operation NatAdd [Nat(3), Nat(5)]
  Return ~v0
"
    );
}

/// Erasure takes a module's items as they are stored, every item after the items it reaches: `a` is written before the `b` it names, and the order a module is stored in puts `b` first.
#[test]
fn items_erase_in_the_order_the_module_stores_them() {
    let items = vec![
        definition(
            "a",
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                curios_core::Term::free_var(&global("b")),
                nat_lit(1),
            )),
        ),
        definition(
            "b",
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            nat_lit(2),
        ),
    ];
    let mut program = module(items, curios_core::Term::free_var(&global("a")));
    program.module = program.module.ordered();
    let erased = erase(
        &program,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Let ~v0$/a = Operation NatAdd [Nat(2), Nat(1)]
entry
  Return ~v0$/a
"
    );
}

#[test]
fn an_exit_seals_the_thunk_that_describes_it() {
    let dead = fresh(Some("dead"));
    // let dead = /std/Process/exit(3); 7 — the trailing computation is *not* dead, and that is the point. `exit` returns an `Io`, so binding it builds a description and performs nothing; the entry goes on to return 7. What the exit still does is seal the block it is written in — the thunk's, which ends on the halting terminator with no return after it.
    let body = curios_core::Term::let_(
        &dead,
        curios_core::Term::intrinsic(curios_core::Intrinsic::io_type(
            curios_core::Term::tuple_type_unit(),
        )),
        curios_core::Term::foreign(
            Arc::new(ForeignFunction {
                name: "/exit".to_string(),
                signature: WireSignature {
                    params: vec![WireType::Byte],
                    returns: WireReturn::Never,
                },
            }),
            curios_core::Global::Authored(Qualifier::from(["exit"])),
            vec![
                curios_core::Term::tuple_type_unit(),
                curios_core::Term::intrinsic(curios_core::Intrinsic::Byte(3)),
            ],
        ),
        nat_lit(7),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Functions
    function ~f0$dead()
      Halt ~x0 [Byte(3)]
  Return Nat(7)
"
    );
}

/// Where [`a_bound_call`] binds its call: a local `let` opening the entry, or a top-level item.
#[derive(Clone, Copy)]
enum Binder {
    Let,
    Item,
}

/// `callee(4)` bound and never used, before an entry that answers `7`: `callee` is a function of one `Nat`, returning a type where `returns_a_type` and a `Nat` otherwise, so the two readings differ only in whether the binding's type is a sort.
fn a_bound_call(binder: Binder, returns_a_type: bool) -> curios_anf::Module {
    let n = fresh(Some("n"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let (result_type, result) = match returns_a_type {
        true => (curios_core::Term::type_ground(), nat.clone()),
        false => (
            nat.clone(),
            curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                curios_core::Term::free_var(&n),
                nat_lit(1),
            )),
        ),
    };
    let mut items = vec![definition(
        "callee",
        curios_core::Term::func_type([(n, nat.clone())], result_type.clone()),
        curios_core::Term::func([(n, nat.clone())], result),
    )];
    let call =
        curios_core::Term::apply(curios_core::Term::free_var(&global("callee")), [nat_lit(4)]);
    let body = match binder {
        Binder::Let => {
            let bound = fresh(Some("bound"));
            curios_core::Term::let_(&bound, result_type, call, nat_lit(7))
        }
        Binder::Item => {
            items.push(definition("bound", result_type, call));
            nat_lit(7)
        }
    };
    erase(&module(items, body), nat)
}

fn calls(erased: &curios_anf::Module) -> usize {
    bound(erased)
        .filter(|rhs| matches!(rhs, curios_anf::Rhs::Apply { .. }))
        .count()
}

/// `identity : (Nat) -> Nat`, stamped `stamp` by elaboration, and an entry applying it — erased through the path production takes, as the program it links to.
fn identity_arena(stamp: curios_core::Totality) -> ErasedProgram {
    let x = fresh(Some("x"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let mut items = vec![definition(
        "identity",
        curios_core::Term::func_type([(x, nat.clone())], nat.clone()),
        curios_core::Term::func([(x, nat.clone())], curios_core::Term::free_var(&x)),
    )];
    let curios_core::Item::Let(declared) = &mut items[0] else {
        unreachable!("the fixture declares a definition");
    };
    declared.totality = stamp;
    let body = curios_core::Term::apply(
        curios_core::Term::free_var(&global("identity")),
        [nat_lit(4)],
    );

    let program = zonked(&stating(&module(items, body), nat));
    let globals = Globals::default();
    let (_, mut kernel) = certify_program(&program, DEFAULT_BUDGET, &globals, SYNTAX);
    erase_program(&mut kernel, Erasures::nothing(), &program).expect("the program erases")
}

/// Every erased function's termination flag.
fn totals(erased: &curios_anf::Module) -> Vec<bool> {
    erased
        .functions()
        .iter()
        .flatten()
        .map(|function| function.total)
        .collect()
}

/// Erasure marks nothing total, whatever elaboration stamped: the flag is the certifier's record's to set, where the sealed program meets it, and a stamp is read by nothing below Core.
#[test]
fn erasure_marks_no_function_total_whatever_its_stamp() {
    for stamp in [curios_core::Totality::Total, curios_core::Totality::Partial] {
        let erased = identity_arena(stamp).into_module();

        assert_eq!(totals(&erased), [false], "stamped {stamp:?}");
    }
}

/// A record marks exactly the functions its `Total` definitions erased to — found through the binding erasure made — and nothing it classifies otherwise or does not name, whatever the stamp says: a `Partial` stamp does not stop a `Total` record, and a `Total` stamp does not stand in for one.
#[test]
fn a_record_marks_the_functions_its_total_definitions_erased_to() {
    let identity = nominal("identity");
    let cases = [
        (
            curios_core::Totality::Partial,
            curios_core::Certification::of(
                [(
                    identity,
                    curios_core::Certified {
                        totality: curios_core::Totality::Total,
                        ..curios_core::Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            true,
        ),
        (
            curios_core::Totality::Total,
            curios_core::Certification::of(
                [(
                    identity,
                    curios_core::Certified {
                        totality: curios_core::Totality::Partial,
                        ..curios_core::Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            false,
        ),
        (
            curios_core::Totality::Total,
            curios_core::Certification::default(),
            false,
        ),
    ];

    for (stamp, record, marked) in cases {
        let mut arena = identity_arena(stamp);
        arena.mark_total(&record);

        assert_eq!(
            totals(&arena.into_module()),
            [marked],
            "stamped {stamp:?}, recorded {record:?}"
        );
    }
}

#[test]
fn a_bound_type_is_not_computed() {
    // A binding is a kept slot and a type fills it with a stand-in, so the call that would have computed the type is never emitted — the direct call `erase_apply` keeps at an erasable type is not reached at all.
    for binder in [Binder::Let, Binder::Item] {
        let erased = a_bound_call(binder, true);
        assert_eq!(
            calls(&erased),
            0,
            "{}",
            curios_anf::test_support::shape(&erased)
        );
    }
}

#[test]
fn a_bound_value_is_computed_where_it_is_written() {
    // The control: the same binding at a `Nat` is evaluated under call-by-value, used or not. A local one is the call it was written as; a top-level one is what the call computes, computed where its item is erased.
    let erased = a_bound_call(Binder::Let, false);
    assert_eq!(
        calls(&erased),
        1,
        "{}",
        curios_anf::test_support::shape(&erased)
    );

    let erased = a_bound_call(Binder::Item, false);
    assert_eq!(
        calls(&erased),
        0,
        "{}",
        curios_anf::test_support::shape(&erased)
    );
    assert!(
        bound(&erased).any(|rhs| matches!(rhs, curios_anf::Rhs::Alias(_))),
        "{}",
        curios_anf::test_support::shape(&erased)
    );
}

/// A unit of `step`, a function adding `by` to its argument, and `stepped`, which is `step(4)`, with the unit erased whole.
fn stepping(by: usize) -> (curios_core::Zonked<curios_core::Module>, ErasedUnit) {
    let n = fresh(Some("n"));
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition(
                    "step",
                    curios_core::Term::func_type([(n, nat.clone())], nat.clone()),
                    curios_core::Term::func(
                        [(n, nat.clone())],
                        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                            curios_core::Term::free_var(&n),
                            nat_lit(by),
                        )),
                    ),
                ),
                definition(
                    "stepped",
                    nat,
                    curios_core::Term::apply(
                        curios_core::Term::free_var(&global("step")),
                        [nat_lit(4)],
                    ),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_alone(&unit).expect("the unit erases whole");

    (unit, erased)
}

/// A top-level value is computed where its unit is erased: `stepped` is `step(4)`, and its erased form is the `5` that comes to, naming nothing of `step`. So the unit holds no item that computes when a program starts, and a program links `stepped` where it names it and `step` where it applies it.
#[test]
fn a_top_level_value_is_computed_where_its_unit_is_erased() {
    let (_, unit) = stepping(1);
    let stepped = unit
        .find(&nominal("stepped"), Sorts::NONE)
        .expect("the unit declares it");

    assert_eq!(unit.computing().count(), 0);
    assert!(stepped.functions().is_empty() && stepped.values().is_empty());
    assert_eq!(
        curios_anf::test_support::shape(stepped.module()),
        "\
items
  Let ~v0$/stepped = Alias Nat(5)
no entry
"
    );
}

/// A value computed where it is erased ran the erased form of what it applied, whole, so it is computed again where that form is another: `step` is elaborated again to add `2`, under the signature and the interface `stepped` read it by, and `stepped`, whose declaration is the baseline's, holds the `6` a whole erasure computes.
///
/// Mutation-checked against the question about a form whole: with every form an evaluation ran taken to stand, `stepped` keeps the baseline's `5`.
#[test]
fn a_value_is_computed_again_where_a_form_its_evaluation_ran_is_another() {
    let (before, baseline) = stepping(1);
    let (edited, whole) = stepping(2);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            erased: &baseline,
            made: &BTreeSet::from([nominal("step")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_ne!(form(&whole, "stepped"), form(&baseline, "stepped"));
    assert_eq!(form(&over, "stepped"), form(&whole, "stepped"));
}

/// A value computed where it is erased is kept where every form its evaluation ran is the one it ran: `step` is elaborated again to what it was, so its erased form is stored as the bytes it was, and `stepped` holds the baseline's own erased form.
#[test]
fn a_computed_value_is_kept_where_what_it_ran_was_erased_again_to_what_it_was() {
    let (before, baseline) = stepping(1);
    let (edited, _) = stepping(1);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            erased: &baseline,
            made: &BTreeSet::from([nominal("step")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(
            unit.find(&nominal("stepped"), Sorts::NONE)
                .expect("the unit declares it"),
        )
    };

    assert_eq!(held(&over), held(&baseline), "`stepped` was erased again");
}

#[test]
fn sequences_transcribe_without_carrier_choices() {
    let list = fresh(Some("list"));
    let body = curios_core::Term::let_(
        &list,
        curios_core::Term::intrinsic(curios_core::Intrinsic::ListType(
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )),
        curios_core::Term::intrinsic(curios_core::Intrinsic::List {
            element: curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            items: vec![nat_lit(1), nat_lit(2)],
        }),
        curios_core::Term::intrinsic(curios_core::Intrinsic::ListLen {
            element: curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            list: curios_core::Term::free_var(&list),
        }),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0$list = Sequence ListBuild [Nat(1), Nat(2)]
  Let ~v1 = Sequence ListLen [~v0$list]
  Return ~v1
"
    );
}

#[test]
fn erasure_is_deterministic() {
    let x = fresh(Some("x"));
    // Both runs erase the *same* term, each through a reader made for it, so any difference would be erasure's own, not a difference in the binders handed to it.
    let build = || {
        let body = curios_core::Term::let_(
            &x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            nat_lit(2),
            curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                curios_core::Term::free_var(&x),
                nat_lit(3),
            )),
        );
        let erased = erase(
            &module(Vec::new(), body),
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        );
        curios_anf::test_support::shape(&erased)
    };
    assert_eq!(build(), build());
}

/// What an item's erasure hands on holds no binder it opened. A binder's identity counts the binders its context had minted before it, so two erasures of one unit would part on every binder they kept, and an item that read the erased form would hold an identity it mints again.
#[test]
fn an_erased_form_keeps_no_binder_its_erasure_opened() {
    let erased = |opened: usize| {
        let x = fresh(Some("x"));
        let y = fresh(Some("y"));
        let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
        // One constant: the module indexes its constants by hash, and two would print in an order of the table's own.
        let body = curios_core::Term::func(
            [(y, nat.clone())],
            curios_core::Term::let_(
                &x,
                nat.clone(),
                nat_lit(2),
                curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                    curios_core::Term::free_var(&x),
                    curios_core::Term::free_var(&y),
                )),
            ),
        );
        let unit = module(
            vec![definition(
                "bump",
                curios_core::Term::func_type([(y, nat.clone())], nat.clone()),
                body,
            )],
            nat_lit(0),
        )
        .module;

        let unit = zonked(&unit);
        let globals = Globals::default();
        let (_, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
        let places = curios_core::Places::of(unit.as_module());
        kernel.read(|reader| {
            for _ in 0..opened {
                reader.fresh(None);
            }

            crate::erase_items(
                reader,
                Erasures::nothing(),
                (unit.as_module(), &places),
                None,
            )
            .expect("the unit erases")
        })
    };
    let first = erased(0);
    let second = erased(8);

    assert_eq!(format!("{first:?}"), format!("{second:?}"));
}

/// A unit of two definitions, `callee` and `caller`, which is `callee` by name, and the same unit erased whole.
fn calling(callee: usize) -> (curios_core::Zonked<curios_core::Module>, ErasedUnit) {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition("callee", nat.clone(), nat_lit(callee)),
                definition(
                    "caller",
                    nat,
                    curios_core::Term::free_var(&global("callee")),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_alone(&unit).expect("the unit erases whole");

    (unit, erased)
}

/// How a unit holds the item declaring `name`, as its printed erased form.
fn form(unit: &ErasedUnit, name: &str) -> String {
    format!(
        "{:?}",
        unit.find(&nominal(name), Sorts::NONE)
            .expect("the unit declares it")
    )
}

/// A unit erased over a baseline erases the items the elaborator made again and keeps the others. `kept` is given another body since the baseline was erased and is not named among the made, so what the unit holds for it is still what the baseline erased it to: an item erased again would show the new body.
#[test]
fn a_unit_erased_over_a_baseline_erases_what_was_elaborated_again_and_no_other() {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let unit = |kept: usize, redone: usize| {
        zonked(
            &module(
                vec![
                    definition("kept", nat.clone(), nat_lit(kept)),
                    definition("redone", nat.clone(), nat_lit(redone)),
                ],
                nat_lit(0),
            )
            .module,
        )
    };

    let before = unit(1, 2);
    let baseline = erase_alone(&before).expect("the baseline erases");
    let edited = unit(3, 4);
    let whole = erase_alone(&edited).expect("the unit erases whole");
    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            erased: &baseline,
            made: &BTreeSet::from([nominal("redone")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_eq!(form(&over, "kept"), form(&baseline, "kept"));
    assert_ne!(form(&over, "kept"), form(&whole, "kept"));
    assert_eq!(form(&over, "redone"), form(&whole, "redone"));
}

/// An erased form the elaborator kept is erased again where a name it uses is bound another way: `caller` reads `callee` as the constant it is bound to, so with `callee` elaborated again to another constant, `caller`'s erased form is the one a whole erasure makes, though its declaration is the baseline's.
///
/// Mutation-checked against the question about a binding: with no erased form's reads noted, `caller` keeps the baseline's, which holds the old constant.
#[test]
fn an_erased_form_is_erased_again_where_a_name_it_uses_is_bound_another_way() {
    let (before, baseline) = calling(1);
    let (edited, whole) = calling(2);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            erased: &baseline,
            made: &BTreeSet::from([nominal("callee")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");

    assert_ne!(form(&whole, "caller"), form(&baseline, "caller"));
    assert_eq!(form(&over, "caller"), form(&whole, "caller"));
}

/// An erased form the elaborator kept is kept where everything its erasure asked is answered as it was, whatever was elaborated again beside it: `callee` is elaborated again to the constant it was, so `caller`, which read that binding, holds the baseline's own erased form.
#[test]
fn an_erased_form_is_kept_where_what_it_read_came_out_as_it_was() {
    let (before, baseline) = calling(1);
    let (edited, _) = calling(1);

    let over = erase_over(
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &curios_core::Places::of(before.as_module()),
            erased: &baseline,
            made: &BTreeSet::from([nominal("callee")]),
            dropped: &BTreeSet::new(),
            now: &curios_core::Places::of(edited.as_module()),
            candidates: None,
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(
            unit.find(&nominal("caller"), Sorts::NONE)
                .expect("the unit declares it"),
        )
    };

    assert_eq!(held(&over), held(&baseline), "`caller` was erased again");
}

#[test]
fn a_function_erases_with_a_parameter_for_each_binder_and_no_captures() {
    let type_param = fresh(Some("A"));
    let x = fresh(Some("x"));
    // (A : Type, x : A) => x — the type parameter is a parameter like the other, filled with the unit constant where a type is passed; the function stores no captures.
    let func_type = curios_core::Term::func_type(
        [
            (type_param, curios_core::Term::type_ground()),
            (x, curios_core::Term::free_var(&type_param)),
        ],
        curios_core::Term::free_var(&type_param),
    );
    let items = vec![definition(
        "id",
        func_type,
        curios_core::Term::func(
            [
                (type_param, curios_core::Term::type_ground()),
                (x, curios_core::Term::free_var(&type_param)),
            ],
            curios_core::Term::free_var(&x),
        ),
    )];
    let body = curios_core::Term::apply(
        curios_core::Term::free_var(&global("id")),
        [
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            nat_lit(4),
        ],
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Functions
    function ~f0$/id(~v0$A, ~v1$x)
      Return ~v1$x
entry
  Let ~v2 = Apply ~f0$/id [Unit, Nat(4)]
  Return ~v2
"
    );
}

#[test]
fn a_capturing_closure_stores_no_capture_list() {
    let x = fresh(Some("x"));
    let y_binder = fresh(Some("y"));
    // (y : Nat) => (x : Nat) => x + y — the inner closure references the outer parameter freely; analysis derives it, nothing is stored.
    let inner_type = curios_core::Term::func_type(
        [(
            x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )],
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let outer_type = curios_core::Term::func_type(
        [(
            y_binder,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )],
        inner_type,
    );
    let items = vec![definition(
        "make",
        outer_type,
        curios_core::Term::func(
            [(
                y_binder,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::func(
                [(
                    x,
                    curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                )],
                curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
                    curios_core::Term::free_var(&x),
                    curios_core::Term::free_var(&y_binder),
                )),
            ),
        ),
    )];
    let expected = curios_core::Term::func_type(
        [(
            y_binder,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )],
        curios_core::Term::func_type(
            [(
                x,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
    );
    let erased = erase(
        &module(items, curios_core::Term::free_var(&global("make"))),
        expected,
    );

    assert!(
        bound(&erased).any(|rhs| matches!(
            rhs,
            curios_anf::Rhs::Operation {
                operation: curios_anf::Operation::NatAdd,
                ..
            }
        )),
        "the inner body adds"
    );
    // The inner closure's capture of `y` is derived, never stored: the outer parameter is the inner function's one free value.
    let analysis = curios_anf::Analysis::analyze(&erased);
    let mut functions = erased.function_ids();
    let outer = functions.next().expect("the outer function");
    let inner = functions.next().expect("the inner function");
    let y = erased.function(outer).expect("live").params[0];
    assert_eq!(
        analysis
            .free_values(inner)
            .iter()
            .copied()
            .collect::<Vec<_>>(),
        vec![y]
    );
}

fn opt_type() -> curios_core::Term {
    curios_core::Term::induct_type(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        Vec::<curios_core::Term>::new(),
    )
}

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some` tag 1 (registry-sorted). Registered on the module so erasure seeds it.
fn opt_induct() -> curios_core::InductDecl {
    let x = fresh(Some("x"));
    curios_core::InductDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::done(())),
        constructors: Vec::from([
            (
                curios_core::Atom::from("none"),
                curios_core::InductParam::new(curios_core::Telescope::done(Vec::new()), vec![]),
            ),
            (
                curios_core::Atom::from("some"),
                curios_core::InductParam::new(
                    curios_core::Telescope::build(
                        [(
                            x,
                            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                        )],
                        Vec::new(),
                    ),
                    vec![Plicity::Explicit],
                ),
            ),
        ])
        .into(),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

#[test]
fn a_variant_constructs_with_its_registered_schema() {
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let body = curios_core::Term::variant(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("some"),
        [nat_lit(6)],
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, opt_type());
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Rec
    value ~v0$/Opt
      Return Unit
entry
  Let ~v1 = Construct ~t1 [Nat(6)]
  Return ~v1
"
    );
    // The family the construction registered against, asked of the arena the header renders from: a nullary `none` and a `some` carrying one `Nat`, with `~t1` — the constructed one — the second.
    let [family] = erased.families() else {
        panic!("the fixture registers one family");
    };
    let [none, some] = family.constructors[..] else {
        panic!("Opt has two constructors");
    };
    assert!(
        erased
            .constructor(none)
            .expect("none is registered")
            .fields
            .is_empty()
    );
    assert_eq!(
        erased
            .constructor(some)
            .expect("some is registered")
            .fields
            .iter()
            .map(|field| field.shape)
            .collect::<Vec<_>>(),
        vec![curios_anf::FieldShape::Number]
    );
}

/// A row is its declaration's. The item declaring `Opt` registers the family, each field's shape with it; an item that constructs an `Opt` imports what it reads of the family, its count and the one constructor it builds, at its position and width and saying nothing of a field; and a program holds one family, the declaration's.
#[test]
fn a_row_is_registered_by_the_item_that_declares_it() {
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let former = curios_core::Item::Let(Box::new(curios_core::Definition {
        name: nominal("Opt"),
        kind: curios_core::DefinitionKind::Authored,
        universe_context: curios_core::UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: curios_core::Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        value_sort: None,
        type_: curios_core::Term::type_ground(),
        body: opt_type(),
    }));
    let six = definition(
        "six",
        opt_type(),
        curios_core::Term::variant(
            nominal("Opt"),
            Vec::<curios_core::Term>::new(),
            curios_core::Atom::from("some"),
            [nat_lit(6)],
        ),
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: vec![former, six].into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint {
            body: curios_core::Term::free_var(&global("six")),
            type_: None,
        },
    };
    // The shapes of `some`'s payload in the one family `module` holds, the constructor at its second position.
    let payload = |module: &curios_anf::Module| {
        let [family] = module.families() else {
            panic!("the module holds one family");
        };
        let some = family
            .constructors
            .iter()
            .copied()
            .find(|&constructor| module.constructor_position(constructor) == Some(1))
            .expect("the module holds some");

        module
            .constructor(some)
            .expect("some is registered")
            .fields
            .iter()
            .map(|field| field.shape)
            .collect::<Vec<_>>()
    };

    let unit = erase_alone(&zonked(&fixture.module)).expect("the unit erases");
    let fragment = |name: &str| {
        unit.find(&nominal(name), Sorts::NONE)
            .expect("the unit declares it")
            .module()
    };
    assert_eq!(
        payload(fragment("Opt")),
        vec![curios_anf::FieldShape::Number]
    );
    assert_eq!(
        payload(fragment("six")),
        vec![curios_anf::FieldShape::Opaque]
    );
    let imported = fragment("six");
    assert_eq!(imported.families()[0].constructors.len(), 1);
    assert_eq!(
        imported.imports().families().values().collect::<Vec<_>>(),
        [&2]
    );

    assert_eq!(
        payload(&erase(&fixture, opt_type())),
        vec![curios_anf::FieldShape::Number]
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let a = fresh(Some("a"));
    let b = fresh(Some("b"));
    let pair = fresh(Some("pair"));
    let tuple_type = curios_core::Term::tuple_type([
        (
            a,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        (
            b,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
    ]);
    let body = curios_core::Term::let_(
        &pair,
        tuple_type.clone(),
        curios_core::Term::tuple([nat_lit(1), nat_lit(2)]),
        curios_core::Term::proj(curios_core::Term::free_var(&pair), 1),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0$pair = Product ~p0 [Nat(1), Nat(2)]
  Let ~v1 = Project ~p0.1 ~v0$pair
  Return ~v1
"
    );
    // The width row itself — that it is the interned shared one, which is the test's subject.
    let [schema] = erased.products() else {
        panic!("the fixture registers one schema");
    };
    assert!(schema.is_shared());
    assert_eq!(schema.width(), 2);
}

#[test]
fn an_anonymous_tuple_keeps_a_field_for_its_proof() {
    let x = fresh(Some("x"));
    let w = fresh(Some("w"));
    let sub = fresh(Some("sub"));
    let p = fresh(Some("P"));
    // { x : Nat, w : Prop-valued } is two fields, the proof's holding the unit constant: an anonymous tuple has no declaration to say which it keeps, and a reader that knows `w`'s type as a variable reads two.
    let subset_type = curios_core::Term::tuple_type([
        (
            x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        // Quantifying over `Prop` lands a level up.
        (
            w,
            curios_core::Term::prop_at(curios_core::Level::constant(1)),
        ),
    ]);
    let body = curios_core::Term::let_(
        &sub,
        subset_type.clone(),
        curios_core::Term::tuple([
            nat_lit(9),
            curios_core::Term::func_type(
                [(p, curios_core::Term::prop())],
                curios_core::Term::free_var(&p),
            ),
        ]),
        curios_core::Term::proj(curios_core::Term::free_var(&sub), 0),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0$sub = Product ~p0 [Nat(9), Unit]
  Let ~v1 = Project ~p0.0 ~v0$sub
  Return ~v1
"
    );
}

#[test]
fn a_bool_match_erases_to_a_switch_bool() {
    let body = curios_core::Term::bool_match(
        curios_core::Term::intrinsic(curios_core::Intrinsic::Bool(true)),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(10),
        nat_lit(20),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v0 = SwitchBool Bool(true)
    false
      Return Nat(10)
    true
      Return Nat(20)
  Return ~v0
"
    );
}

#[test]
fn a_dead_hypothesis_nat_match_peels_to_a_dispatch() {
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // match n | 0 => 0 | succ pred (ih dead) => pred — a case split, not a fold.
    let items = vec![definition(
        "n",
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = curios_core::Term::nat_match(
        curios_core::Term::free_var(&global("n")),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::free_var(&pred),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v1 = SwitchNat Nat(5)
    case 0
      Return Nat(0)
    default
      Let ~v0$pred = Operation NatSub [Nat(5), Nat(1)]
      Return ~v0$pred
  Return ~v1
"
    );
}

#[test]
fn a_live_hypothesis_nat_match_erases_to_a_fold() {
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // match n | 0 => 0 | succ pred ih => ih + 2 — genuine induction.
    let items = vec![definition(
        "n",
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = curios_core::Term::nat_match(
        curios_core::Term::free_var(&global("n")),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
            curios_core::Term::free_var(&ih),
            nat_lit(2),
        )),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Let ~v3 = FoldNat Nat(5)
    zero
      Return Nat(0)
    step(~v0$pred, ~v1$ih)
      Let ~v2 = Operation NatAdd [~v1$ih, Nat(2)]
      Return ~v2
  Return ~v3
"
    );
}

#[test]
fn a_live_hypothesis_list_match_erases_to_a_sequence_fold() {
    let h = fresh(Some("h"));
    let t = fresh(Some("t"));
    let ih = fresh(Some("ih"));
    // match xs | [] => 0 | h :: t (ih) => ih + 1 — a length fold over a list.
    let list_ty = curios_core::Term::intrinsic(curios_core::Intrinsic::ListType(
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    ));
    let items = vec![definition(
        "xs",
        list_ty.clone(),
        curios_core::Term::intrinsic(curios_core::Intrinsic::List {
            element: curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            items: vec![nat_lit(1)],
        }),
    )];
    let body = curios_core::Term::list_match(
        curios_core::Term::free_var(&global("xs")),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &[&h, &t, &ih],
        curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(
            curios_core::Term::free_var(&ih),
            nat_lit(1),
        )),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Let ~v0$/xs = Sequence ListBuild [Nat(1)]
entry
  Let ~v5 = FoldSequence List ~v0$/xs
    empty
      Return Nat(0)
    step(~v1$h, ~v2$t, ~v3$ih)
      Let ~v4 = Operation NatAdd [~v3$ih, Nat(1)]
      Return ~v4
  Return ~v5
"
    );
}

#[test]
fn a_variant_match_binds_payload_without_projections() {
    let x = fresh(Some("x"));
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let scrutinee = curios_core::Term::variant(
        nominal("Opt"),
        Vec::<curios_core::Term>::new(),
        curios_core::Atom::from("some"),
        [nat_lit(6)],
    );
    let body = curios_core::Term::induct_match(
        scrutinee,
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        [
            ("none", Vec::<curios_core::Free>::new(), nat_lit(0)),
            ("some", vec![x], curios_core::Term::free_var(&x)),
        ],
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(
        &fixture,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Rec
    value ~v0$/Opt
      Return Unit
entry
  Let ~v1$scrutinee = Construct ~t1 [Nat(6)]
  Let ~v3 = MatchVariant ~d0 ~v1$scrutinee
    arm ~t0()
      Return Nat(0)
    arm ~t1(~v2$x)
      Return ~v2$x
  Return ~v3
"
    );
}

#[test]
fn an_effectful_scrutinee_is_erased_once() {
    let x = fresh(Some("x"));
    let pred = fresh(Some("pred"));
    let ih = fresh(Some("ih"));
    // The peel path re-derives Core terms from the head (`n - 1`); an effectful compound head must still evaluate exactly once, through the alias.
    let io_read =
        curios_core::Term::apply(curios_core::Term::free_var(&global("read")), [nat_lit(0)]);
    let items = vec![definition(
        "read",
        curios_core::Term::func_type(
            [(
                x,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
        curios_core::Term::func(
            [(
                x,
                curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
            )],
            curios_core::Term::free_var(&x),
        ),
    )];
    let body = curios_core::Term::nat_match(
        io_read,
        None,
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        curios_core::Term::free_var(&pred),
    );
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let read = erased.function_ids().next().expect("the read function");
    let applications = bound(&erased)
        .filter(|rhs| {
            matches!(
                rhs,
                curios_anf::Rhs::Apply {
                    callee: curios_anf::Atom::Function(callee),
                    ..
                } if *callee == read
            )
        })
        .count();
    assert_eq!(
        applications, 1,
        "the compound scrutinee is applied exactly once"
    );
}

#[test]
fn a_recursive_function_group_erases_to_functions() {
    let x = fresh(Some("x"));
    let f = fresh(Some("f"));
    // rec f(x) = f(x); body f(3)
    let func_type = curios_core::Term::func_type(
        [(
            x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )],
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let body = curios_core::Term::rec(
        vec![(
            f,
            func_type.clone(),
            curios_core::Term::func(
                [(
                    x,
                    curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                )],
                curios_core::Term::apply(
                    curios_core::Term::free_var(&f),
                    [curios_core::Term::free_var(&x)],
                ),
            ),
        )],
        curios_core::Term::apply(curios_core::Term::free_var(&f), [nat_lit(3)]),
    );
    let erased = erase(
        &module(Vec::new(), body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Functions
    function ~f0$f(~v0$x)
      Let ~v1 = Apply ~f0$f [~v0$x]
      Return ~v1
  Let ~v2 = Apply ~f0$f [Nat(3)]
  Return ~v2
"
    );
}

#[test]
fn a_mixed_recursive_group_erases_to_a_rec_group() {
    let u = fresh(Some("u"));
    let produce = fresh(Some("produce"));
    let consume = fresh(Some("consume"));
    // rec { produce() = consume; consume = produce } — a dormant knot: the computed member's initializer references the function, and the function body references the computed member (dormant until applied).
    let produce_type = curios_core::Term::func_type(
        [(
            u,
            curios_core::Term::tuple_type(Vec::<(curios_core::Free, curios_core::Term)>::new()),
        )],
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let body = curios_core::Term::rec(
        vec![
            (
                produce,
                produce_type.clone(),
                curios_core::Term::func([(u, curios_core::Term::tuple_type_unit())], nat_lit(5)),
            ),
            (
                consume,
                produce_type.clone(),
                curios_core::Term::free_var(&produce),
            ),
        ],
        curios_core::Term::free_var(&consume),
    );
    let erased = erase(&module(Vec::new(), body), produce_type);
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Rec
    function ~f0$produce(~v1$u)
      Return Nat(5)
    value ~v0$consume
      Return ~f0$produce
  Return ~v0$consume
"
    );
}

#[test]
fn a_computed_only_evaluation_cycle_is_rejected_as_an_error() {
    let a = fresh(Some("a"));
    let b = fresh(Some("b"));
    // rec a = b; b = a — a mutual value-level cycle no forcing order satisfies. The verifier rejects it; erasure surfaces the diagnostic as an error, never a panic. (A *self*-knot `rec loop = loop` is admitted only while nothing outside its initializer reads it, which is when the lowering drops it.)
    let type_ = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let body = curios_core::Term::rec(
        vec![
            (a, type_.clone(), curios_core::Term::free_var(&b)),
            (b, type_.clone(), curios_core::Term::free_var(&a)),
        ],
        curios_core::Term::free_var(&a),
    );
    let error =
        try_erase(&module(Vec::new(), body), type_).expect_err("the value-level cycle is rejected");
    assert!(matches!(error, Error::EvaluationCycle { .. }), "{error:?}");
}

#[test]
fn top_level_recursive_items_erase_through_the_item_chain() {
    let x = fresh(Some("x"));
    let func_type = curios_core::Term::func_type(
        [(
            x,
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        )],
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    let items = vec![curios_core::Item::Rec(curios_core::RecItem::new(vec![
        curios_core::Definition {
            name: curios_core::Global::Authored(Qualifier::from(["go"])),
            kind: curios_core::DefinitionKind::Authored,
            universe_context: curios_core::UniverseContext::empty(),
            island: Qualifier::empty(),
            totality: curios_core::Totality::default(),
            proof: false,
            variances: Vec::new(),
            field_variances: Vec::new(),
            parameter_field: None,
            value_sort: None,
            type_: func_type.clone(),
            body: curios_core::Term::func(
                [(
                    x,
                    curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                )],
                curios_core::Term::apply(
                    curios_core::Term::free_var(&global("go")),
                    [curios_core::Term::free_var(&x)],
                ),
            ),
        },
    ]))];
    let body = curios_core::Term::apply(curios_core::Term::free_var(&global("go")), [nat_lit(1)]);
    let erased = erase(
        &module(items, body),
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
    );
    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
items
  Functions
    function ~f0$/go(~v0$x)
      Let ~v1 = Apply ~f0$/go [~v0$x]
      Return ~v1
entry
  Let ~v2 = Apply ~f0$/go [Nat(1)]
  Return ~v2
"
    );
}

/// A no-parameter single-field struct declaration — the classifier fixtures' newtype link.
fn newtype_struct(field: &str, type_: curios_core::Term) -> curios_core::StructDecl {
    let f = fresh(Some(field));
    curios_core::StructDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::build([(f, type_)], ())),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

fn struct_type_of(name: &str) -> curios_core::Term {
    curios_core::Term::from(curios_core::Subterm::StructType(curios_core::StructType {
        name: nominal(name),
        universes: curios_core::Universes::default(),
        params: Vec::new(),
    }))
}

/// A no-parameter family with the single unary constructor `mk(x: payload)`.
fn unary_induct(payload: curios_core::Term) -> curios_core::InductDecl {
    let x = fresh(Some("x"));
    curios_core::InductDecl {
        universe_context: curios_core::UniverseContext::empty(),
        arity: curios_core::Telescope::done(curios_core::Telescope::done(())),
        constructors: Vec::from([(
            curios_core::Atom::from("mk"),
            curios_core::InductParam::new(
                curios_core::Telescope::build([(x, payload)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )])
        .into(),
        result_sort: curios_core::Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// The payload-shape classifier chases newtype chains and terminates on cycles: a payload wrapped in two single-field structs lands on `Nat` and records `Immediate`; a payload whose struct names itself is `Opaque` because the visited guard cuts the cycle (a self-referential struct elaborates — it is merely uninhabited); a boxed `Flt` payload records its carrier, `Flt`.
#[test]
fn payload_shapes_chase_newtype_chains_and_terminate_on_cycles() {
    let mut struct_decls = BTreeMap::new();
    struct_decls.insert(
        nominal("Meters"),
        newtype_struct(
            "m",
            curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        ),
    );
    struct_decls.insert(
        nominal("Outer"),
        newtype_struct("o", struct_type_of("Meters")),
    );
    struct_decls.insert(nominal("Loop"), newtype_struct("s", struct_type_of("Loop")));
    let induct_type = |family: &str| {
        curios_core::Term::induct_type(
            nominal(family),
            Vec::<curios_core::Term>::new(),
            Vec::<curios_core::Term>::new(),
        )
    };
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Wrapped"), unary_induct(struct_type_of("Outer")));
    induct_decls.insert(nominal("Knotted"), unary_induct(struct_type_of("Loop")));
    induct_decls.insert(
        nominal("Boxed"),
        unary_induct(curios_core::Term::intrinsic(
            curios_core::Intrinsic::FltType,
        )),
    );
    // A payload whose type is itself a single-constructor family chases through it — the collapsed encoding makes such a family *be* its payload — while a recursive single-constructor family terminates at the visited guard.
    induct_decls.insert(
        nominal("Inner"),
        unary_induct(curios_core::Term::intrinsic(
            curios_core::Intrinsic::NatType,
        )),
    );
    induct_decls.insert(nominal("Chained"), unary_induct(induct_type("Inner")));
    induct_decls.insert(nominal("Selfy"), unary_induct(induct_type("Selfy")));

    // The families register lazily on first construction, and the classifier reads the declared field types: each family is built by a function of its payload, so a family no value inhabits is built as readily as the others.
    let building = |family: &str, payload: curios_core::Term| {
        let value = fresh(Some("v"));
        (
            curios_core::Term::func(
                [(value, payload.clone())],
                curios_core::Term::variant(
                    nominal(family),
                    Vec::<curios_core::Term>::new(),
                    curios_core::Atom::from("mk"),
                    [curios_core::Term::free_var(&value)],
                ),
            ),
            curios_core::Term::func_type([(value, payload)], induct_type(family)),
        )
    };
    let (built, types): (Vec<_>, Vec<_>) = [
        ("Wrapped", struct_type_of("Outer")),
        ("Knotted", struct_type_of("Loop")),
        (
            "Boxed",
            curios_core::Term::intrinsic(curios_core::Intrinsic::FltType),
        ),
        ("Chained", induct_type("Inner")),
        ("Selfy", induct_type("Selfy")),
    ]
    .into_iter()
    .map(|(family, payload)| building(family, payload))
    .unzip();
    let body = curios_core::Term::tuple(built);
    let expected = curios_core::Term::tuple_type(
        ["a", "b", "c", "d", "e"]
            .into_iter()
            .map(|label| fresh(Some(label)))
            .zip(types),
    );
    let fixture = curios_core::Program {
        module: curios_core::test_support::declaring(
            curios_core::Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            struct_decls,
            [],
        ),
        entry: curios_core::Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, expected);

    assert_eq!(
        payload(&erased, "/Wrapped"),
        vec![("x", curios_anf::FieldShape::Number)]
    );
    assert_eq!(
        payload(&erased, "/Knotted"),
        vec![("x", curios_anf::FieldShape::Opaque)]
    );
    // `Boxed` holds a `Flt`, and the recorder names that carrier rather than merely withholding `immediate`.
    assert_eq!(
        payload(&erased, "/Boxed"),
        vec![("x", curios_anf::FieldShape::Flt)]
    );
    assert_eq!(
        payload(&erased, "/Chained"),
        vec![("x", curios_anf::FieldShape::Number)]
    );
    assert_eq!(
        payload(&erased, "/Selfy"),
        vec![("x", curios_anf::FieldShape::Opaque)]
    );
}

/// An applied lambda is transcribed as the function and the call it is, whatever its parameters are marked: erasure reads the callee's type off the lambda itself and holds no opinion on what a declaration may type a `use` parameter at.
#[test]
fn an_applied_lambda_with_a_witness_slot_erases() {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let held = fresh(Some("held"));
    let x = fresh(Some("x"));
    let lambda = curios_core::Term::func_marked(
        [
            (Plicity::Witness, held, struct_type_of("Dict")),
            (Plicity::Explicit, x, nat.clone()),
        ],
        curios_core::Term::free_var(&x),
    );
    let call = curios_core::Term::apply_marked(
        lambda,
        [
            (
                Plicity::Witness,
                curios_core::Term::struct_at(
                    nominal("Dict"),
                    Vec::<curios_core::Level>::new(),
                    Vec::<curios_core::Term>::new(),
                    [nat_lit(1)],
                ),
            ),
            (Plicity::Explicit, nat_lit(3)),
        ],
    );
    let mut fixture = module(Vec::new(), call);
    fixture.module = curios_core::test_support::declaring(
        std::mem::take(&mut fixture.module),
        [],
        [(nominal("Dict"), newtype_struct("held", nat.clone()))],
        [],
    );

    let erased = erase(&fixture, nat);

    assert_eq!(
        curios_anf::test_support::shape(&erased),
        "\
entry
  Functions
    function ~f0$main/1(~v0$held, ~v1$x)
      Return ~v1$x
  Let ~v2 = Apply ~f0$main/1 [Nat(1), Nat(3)]
  Return ~v2
"
    );
}

/// What erasing a function over a chain of `depth` eliminations spends, in budget units, read off the reader its erasure ran through. `step` wraps the type one level deeper and `eliminate` the term.
fn chain_cost(
    depth: usize,
    step: impl Fn(curios_core::Term) -> curios_core::Term,
    eliminate: impl Fn(curios_core::Term) -> curios_core::Term,
) -> u64 {
    let nat = curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let mut domain = nat.clone();
    for _ in 0..depth {
        domain = step(domain);
    }
    let x = fresh(Some("x"));
    let mut body = curios_core::Term::free_var(&x);
    for _ in 0..depth {
        body = eliminate(body);
    }
    let function = curios_core::Term::func([(x, domain.clone())], body);
    let function_type =
        curios_core::Term::func_type_marked([(Plicity::Explicit, x, domain)], nat.clone());

    let unit = zonked(
        &module(
            vec![definition("reads", function_type, function)],
            nat_lit(0),
        )
        .module,
    );
    // On a grown stack, as every entry point erases: a chain this deep is walked a frame a level.
    grown(|| {
        let globals = Globals::default();
        let (_, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
        let places = curios_core::Places::of(unit.as_module());
        kernel.read(|reader| {
            crate::erase_items(
                reader,
                Erasures::nothing(),
                (unit.as_module(), &places),
                None,
            )
            .expect("the unit erases");

            reader.consumed().units()
        })
    })
}

/// A head's type is read once however many eliminations stand on it, so twice the chain is at most twice the work. Reading each head afresh at every level it sits under makes it four times.
#[test]
fn a_projection_chain_erases_in_work_proportional_to_its_depth() {
    let cost = |depth| {
        chain_cost(
            depth,
            |inner| {
                let kept = fresh(Some("kept"));
                let pad = fresh(Some("pad"));
                curios_core::Term::tuple_type([
                    (kept, inner),
                    (
                        pad,
                        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    ),
                ])
            },
            |head| curios_core::Term::proj(head, 0),
        )
    };
    let (short, long) = (cost(256), cost(512));

    assert!(
        long < 2 * short,
        "a 512-deep chain spent {long} where a 256-deep one spent {short}",
    );
}

/// The same for a function applied once per parameter list.
#[test]
fn a_curried_call_chain_erases_in_work_proportional_to_its_depth() {
    let cost = |depth| {
        chain_cost(
            depth,
            |inner| {
                let n = fresh(Some("n"));
                curios_core::Term::func_type_marked(
                    [(
                        Plicity::Explicit,
                        n,
                        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    )],
                    inner,
                )
            },
            |head| curios_core::Term::apply(head, [nat_lit(1)]),
        )
    };
    let (short, long) = (cost(256), cost(512));

    assert!(
        long < 2 * short,
        "a 512-deep chain spent {long} where a 256-deep one spent {short}",
    );
}

/// Erasure's reader is handed no proof's body either: the kernel it reads through defines a definition marked a proof as unfolding to nothing, and the definition beside it to its body.
#[test]
fn a_proofs_body_is_withheld_from_erasures_reader() {
    let p = fresh(Some("P"));
    let proof = fresh(Some("p"));
    let curios_core::Item::Let(lemma) = definition(
        "lemma",
        curios_core::Term::func_type(
            [
                (p, curios_core::Term::prop()),
                (proof, curios_core::Term::free_var(&p)),
            ],
            curios_core::Term::free_var(&p),
        ),
        curios_core::Term::func(
            [
                (p, curios_core::Term::prop()),
                (proof, curios_core::Term::free_var(&p)),
            ],
            curios_core::Term::free_var(&proof),
        ),
    ) else {
        unreachable!("a fixture's definition is a `let`")
    };
    let unit = zonked(
        &module(
            vec![
                curios_core::Item::Let(Box::new(curios_core::Definition {
                    proof: true,
                    ..*lemma
                })),
                definition(
                    "two",
                    curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
                    nat_lit(2),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );

    let globals = Globals::default();
    let (rechecked, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
    assert!(rechecked.verdicts.is_empty(), "{:?}", rechecked.verdicts);
    kernel.read(|reader| {
        assert!(reader.value(&global("lemma")).is_none());
        assert!(reader.value(&global("two")).is_some());
    });
}

/// A record of two fields at one type, `levels` deep: a graph of `levels` nodes over a tree of two to the `levels` leaves. Its binders are minted from `mint`, so two such types name no binder in common.
fn doubled_record(levels: u32, mint: u32) -> curios_core::Term {
    (0..levels).fold(
        curios_core::Term::intrinsic(curios_core::Intrinsic::NatType),
        |type_, level| {
            let first = curios_core::Free::local(u64::from(mint + 2 * level), None);
            let second = curios_core::Free::local(u64::from(mint + 2 * level + 1), None);

            curios_core::Term::tuple_type([(first, type_.clone()), (second, type_)])
        },
    )
}

/// Erasure answers a term that shares in the term's own size: sixty levels that each hold the one below twice, a tree no walk per path finishes, as `walk_tests` puts the crate's other walks to one. A function at the doubling type, as a program's entry and as a definition; a chain of sixty projections out of a value at it; and sixty definitions that each pair the one before with itself, projected, whose type is its tail's with every binding substituted (`Lowering::type_of`).
///
/// Mutation-checked: with a type read again where `Lowering::type_of` kept one, the row of definitions does not return, and with the type a lambda is checked at walked once per occurrence, none of the other three does.
#[test]
fn erasure_answers_a_doubling_term_in_its_own_size() {
    let nat_type = || curios_core::Term::intrinsic(curios_core::Intrinsic::NatType);
    let doubled = doubled_record(60, 5_000);

    let identity = |mint: u64| {
        let a = curios_core::Free::local(mint, Some("a"));
        (
            curios_core::Term::func([(a, doubled.clone())], curios_core::Term::free_var(&a)),
            curios_core::Term::func_type([(a, doubled.clone())], doubled.clone()),
        )
    };
    let (entry, entry_type) = identity(4_000);
    let (held, held_type) = identity(4_001);

    let b = curios_core::Free::local(4_002, Some("b"));
    let chain = (0..60).fold(curios_core::Term::free_var(&b), |head, _| {
        curios_core::Term::proj(head, 0usize)
    });

    // `(let x0 = (n, n); let x1 = (x0, x0); …; x59).0`, each definition stated at its own doubling type: the projection's head is the whole chain, whose type is read by substituting every binding.
    let n = curios_core::Free::local(4_003, Some("n"));
    let names = (0..60u64)
        .map(|level| curios_core::Free::local(4_100 + level, Some("x")))
        .collect::<Vec<_>>();
    let mut paired = curios_core::Term::free_var(&names[59]);
    for level in (0..60u32).rev() {
        let below = match level {
            0 => curios_core::Term::free_var(&n),
            _ => curios_core::Term::free_var(&names[level as usize - 1]),
        };
        paired = curios_core::Term::let_(
            &names[level as usize],
            doubled_record(level + 1, 10_000 + 200 * level),
            curios_core::Term::tuple([below.clone(), below]),
            paired,
        );
    }
    let paired = curios_core::Term::proj(paired, 0usize);

    let rows = [
        (
            "a function at a doubling type",
            try_erase(&module(Vec::new(), entry), entry_type).is_ok(),
        ),
        (
            "a definition at a doubling type",
            try_erase(
                &module(vec![definition("held", held_type, held)], nat_lit(7)),
                nat_type(),
            )
            .is_ok(),
        ),
        (
            "a projection chain out of a doubling type",
            try_erase(
                &module(
                    Vec::new(),
                    curios_core::Term::func([(b, doubled.clone())], chain),
                ),
                curios_core::Term::func_type([(b, doubled.clone())], nat_type()),
            )
            .is_ok(),
        ),
        (
            "sixty definitions that each pair the one before",
            try_erase(
                &module(
                    Vec::new(),
                    curios_core::Term::func([(n, nat_type())], paired),
                ),
                curios_core::Term::func_type([(n, nat_type())], doubled_record(59, 30_000)),
            )
            .is_ok(),
        ),
    ];

    for (row, erased) in rows {
        assert!(erased, "{row} did not erase");
    }
}

/// An erasure that runs out of its budget is refused in erasure's words, at the term it was reading: the program was certified, and only erasing it stopped.
#[test]
fn an_erasure_out_of_budget_is_refused_in_erasures_words() {
    let unit = zonked(&module(Vec::new(), nat_lit(0)).module);
    let globals = Globals::default();
    let (_, mut kernel) = certify_module(&unit, DEFAULT_BUDGET, &globals, SYNTAX);
    let sum = curios_core::Term::intrinsic(curios_core::Intrinsic::nat_add(nat_lit(2), nat_lit(3)));
    let error = kernel.read(|reader| {
        // The whole budget spent, as a reading that ran out of it leaves it.
        reader.restore_budget();
        reader
            .spend(curios_core::Cost::units(DEFAULT_BUDGET))
            .expect("the budget affords itself");

        crate::reduced(reader, &sum).expect_err("a spent budget refuses the reading")
    });
    let reading = curios_elab::Reading {
        module: unit.as_module(),
        kept: None,
        scope: &[],
        syntax: &SYNTAX,
        spellings: None,
        anchors: &[],
        printer: None,
    };

    assert_eq!(
        error.format(&reading),
        "erasure ran out of steps reading: 2 + 3\n  recursion depth needed 1024 units with 0 left"
    );
}
