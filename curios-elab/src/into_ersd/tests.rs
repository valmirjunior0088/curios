use {
    super::Projected,
    crate::{reduce::test_support::qed, *},
    curios_abi::{ForeignFunction, WireReturn, WireSignature, WireType},
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Certification, Certified, Definition, DefinitionKind, Entrypoint, Free, Global,
        InductDecl, InductParam, Intrinsic, Item, Level, Module, Nat, Places, Program, RecItem,
        StructDecl, StructType, Subterm, Telescope, Term, Totality, UniverseContext,
        UniverseMetaId, UniverseParam, Universes, Zonkable, Zonked, project_erased_universes,
        test_support::declaring,
    },
    curios_ersd::{FieldShape, test_support::shape},
    curios_utilities::{Plicity, Qualifier, grown},
    std::{
        collections::{BTreeMap, BTreeSet},
        sync::Arc,
    },
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn context() -> Retyping<'static> {
    Retyping::new(DEFAULT_STEP_BUDGET, SYNTAX)
}

/// A top-level definition's identity, from the path a test writes — the same name [`definition`] declares it under. Fixture-only.
fn global(path: &str) -> Free {
    Free::global(Qualifier::from([path]))
}

fn nat_lit(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

fn definition(name: &str, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: Global::Authored(Qualifier::from([name])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        type_,
        body,
    })
}

/// A program of `items` closing with `body`, at no stated type yet — see [`stating`].
fn module(items: Vec<Item>, body: Term) -> Program {
    Program {
        module: Module {
            mounts: Vec::new(),
            items: items.into(),
            witnesses: BTreeSet::new(),
            tests: Vec::new(),
        },
        entry: Entrypoint { body, type_: None },
    }
}

fn zonked<T: Zonkable>(value: &T) -> Zonked<T> {
    Zonked::project(value).expect("the fixture is zonked")
}

/// `program` with its entry stating `type_`, which is what the entry's body is sealed at.
fn stating(program: &Program, type_: Term) -> Program {
    let mut program = program.clone();
    program.entry.type_ = Some(type_);
    program
}

/// `program` erased with nothing in scope, its entry stating `type_`.
fn try_erase(program: &Program, type_: Term) -> Result<curios_ersd::Module, Error> {
    erase_program(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &zonked(&stating(program, type_)),
    )
    .map(ErasedProgram::into_module)
}

fn erase(program: &Program, type_: Term) -> curios_ersd::Module {
    try_erase(program, type_).expect("the program erases")
}

/// The recorded payload row — field hint and carrier shape — of the one constructor of the single-constructor family whose debug name ends in `name`. The arena is what erasure wrote; the header the printer renders from it is a second spelling, and a test that scraped that spelling would pass by matching nothing if it moved.
fn payload(module: &curios_ersd::Module, name: &str) -> Vec<(&'static str, FieldShape)> {
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
fn bound(module: &curios_ersd::Module) -> impl Iterator<Item = &curios_ersd::Rhs> {
    module
        .statements()
        .iter()
        .flatten()
        .filter_map(|statement| match statement {
            curios_ersd::Statement::Let { rhs, .. } => Some(rhs),
            _ => None,
        })
}

#[test]
fn a_scalar_expression_erases_in_evaluation_order() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // let x = 2; x + 3
    let body = Term::let_(
        &x_binder,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(2),
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x_binder), nat_lit(3))),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v0 = Operation NatAdd [Nat(2), Nat(3)]
  Return ~v0
"
    );
}

#[test]
fn bool_and_byte_keep_their_shapes() {
    let mut context = context();
    let b = context.fresh(Some("b"));
    // Bool stays Bool-shaped and Byte stays Byte-shaped: no Nat carrier appears anywhere in the erased output.
    let body = Term::let_(
        &b,
        Term::intrinsic(Intrinsic::BoolType),
        Term::intrinsic(Intrinsic::BoolAnd(
            Term::intrinsic(Intrinsic::Bool(true)),
            Term::intrinsic(Intrinsic::Bool(false)),
        )),
        // Erasure drops the narrowing's bound, which is the half this asserts.
        Term::intrinsic(Intrinsic::nat_to_byte(
            Term::intrinsic(Intrinsic::Nat(Nat::new(7usize))),
            qed(),
        )),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::ByteType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v0$b = Operation BoolAnd [Bool(true), Bool(false)]
  Let ~v1 = Operation NatToByte [Nat(7)]
  Return ~v1
"
    );
}

#[test]
fn a_nat_spine_over_a_variable_erases_to_one_addition() {
    let items = vec![definition(
        "x",
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = Term::intrinsic(Intrinsic::Nat(Nat::Succ(
        3u32.into(),
        Term::free_var(&global("x")),
    )));
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&global("b")), nat_lit(1))),
        ),
        definition("b", Term::intrinsic(Intrinsic::NatType), nat_lit(2)),
    ];
    let mut program = module(items, Term::free_var(&global("a")));
    program.module = program.module.ordered();
    let erased = erase(&program, Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let dead = context.fresh(Some("dead"));
    // let dead = /std/proc/exit(3); 7 — the trailing computation is *not* dead, and that is the point. `exit` returns an `Io`, so binding it builds a description and performs nothing; the entry goes on to return 7. What the exit still does is seal the block it is written in — the thunk's, which ends on the halting terminator with no return after it.
    let body = Term::let_(
        &dead,
        Term::intrinsic(Intrinsic::io_type(Term::tuple_type_unit())),
        Term::foreign(
            Arc::new(ForeignFunction {
                name: "/exit".to_string(),
                signature: WireSignature {
                    params: vec![WireType::Byte],
                    returns: WireReturn::Never,
                },
            }),
            Global::Authored(Qualifier::from(["exit"])),
            vec![Term::tuple_type_unit(), Term::intrinsic(Intrinsic::Byte(3))],
        ),
        nat_lit(7),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
fn a_bound_call(binder: Binder, returns_a_type: bool) -> curios_ersd::Module {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let (result_type, result) = match returns_a_type {
        true => (Term::type_ground(), nat.clone()),
        false => (
            nat.clone(),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat_lit(1))),
        ),
    };
    let mut items = vec![definition(
        "callee",
        Term::func_type([(n, nat.clone())], result_type.clone()),
        Term::func([(n, nat.clone())], result),
    )];
    let call = Term::apply(Term::free_var(&global("callee")), [nat_lit(4)]);
    let body = match binder {
        Binder::Let => {
            let bound = context.fresh(Some("bound"));
            Term::let_(&bound, result_type, call, nat_lit(7))
        }
        Binder::Item => {
            items.push(definition("bound", result_type, call));
            nat_lit(7)
        }
    };
    erase(&module(items, body), nat)
}

fn calls(erased: &curios_ersd::Module) -> usize {
    bound(erased)
        .filter(|rhs| matches!(rhs, curios_ersd::Rhs::Apply { .. }))
        .count()
}

/// `identity : (Nat) -> Nat`, stamped `stamp` by elaboration, and an entry applying it — erased through the path production takes, as the program it links to.
fn identity_arena(stamp: Totality) -> ErasedProgram {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let mut items = vec![definition(
        "identity",
        Term::func_type([(x, nat.clone())], nat.clone()),
        Term::func([(x, nat.clone())], Term::free_var(&x)),
    )];
    let Item::Let(declared) = &mut items[0] else {
        unreachable!("the fixture declares a definition");
    };
    declared.totality = stamp;
    let body = Term::apply(Term::free_var(&global("identity")), [nat_lit(4)]);

    erase_program(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &zonked(&stating(&module(items, body), nat)),
    )
    .expect("the program erases")
}

/// Every erased function's termination flag.
fn totals(erased: &curios_ersd::Module) -> Vec<bool> {
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
    for stamp in [Totality::Total, Totality::Partial] {
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
            Totality::Partial,
            Certification::of(
                [(
                    identity,
                    Certified {
                        totality: Totality::Total,
                        ..Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            true,
        ),
        (
            Totality::Total,
            Certification::of(
                [(
                    identity,
                    Certified {
                        totality: Totality::Partial,
                        ..Certified::default()
                    },
                )],
                [],
                [],
                [],
            ),
            false,
        ),
        (Totality::Total, Certification::default(), false),
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
        assert_eq!(calls(&erased), 0, "{}", shape(&erased));
    }
}

#[test]
fn a_bound_value_is_computed_where_it_is_written() {
    // The control: the same binding at a `Nat` is evaluated under call-by-value, used or not. A local one is the call it was written as; a top-level one is what the call computes, computed where its item is erased.
    let erased = a_bound_call(Binder::Let, false);
    assert_eq!(calls(&erased), 1, "{}", shape(&erased));

    let erased = a_bound_call(Binder::Item, false);
    assert_eq!(calls(&erased), 0, "{}", shape(&erased));
    assert!(
        bound(&erased).any(|rhs| matches!(rhs, curios_ersd::Rhs::Alias(_))),
        "{}",
        shape(&erased)
    );
}

/// A unit of `step`, a function adding `by` to its argument, and `stepped`, which is `step(4)`, with the unit erased whole.
fn stepping(by: usize) -> (Zonked<Module>, ErasedUnit) {
    let mut context = context();
    let n = context.fresh(Some("n"));
    let nat = Term::intrinsic(Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition(
                    "step",
                    Term::func_type([(n, nat.clone())], nat.clone()),
                    Term::func(
                        [(n, nat.clone())],
                        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&n), nat_lit(by))),
                    ),
                ),
                definition(
                    "stepped",
                    nat,
                    Term::apply(Term::free_var(&global("step")), [nat_lit(4)]),
                ),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_unit(DEFAULT_STEP_BUDGET, SYNTAX, Erasures::nothing(), &unit)
        .expect("the unit erases whole");

    (unit, erased)
}

/// A top-level value is computed where its unit is erased: `stepped` is `step(4)`, and its erased form is the `5` that comes to, naming nothing of `step`. So the unit holds no item that computes when a program starts, and a program links `stepped` where it names it and `step` where it applies it.
#[test]
fn a_top_level_value_is_computed_where_its_unit_is_erased() {
    let (_, unit) = stepping(1);
    let stepped = unit
        .find(&nominal("stepped"))
        .expect("the unit declares it");

    assert_eq!(unit.computing().count(), 0);
    assert!(stepped.functions().is_empty() && stepped.values().is_empty());
    assert_eq!(
        shape(stepped.module()),
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

    let over = erase_unit_over(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &Places::of(before.as_module()),
            erased: &baseline,
            kept: &BTreeSet::from([nominal("stepped")]),
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

    let over = erase_unit_over(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &Places::of(before.as_module()),
            erased: &baseline,
            kept: &BTreeSet::from([nominal("stepped")]),
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(
            unit.find(&nominal("stepped"))
                .expect("the unit declares it"),
        )
    };

    assert_eq!(held(&over), held(&baseline), "`stepped` was erased again");
}

#[test]
fn sequences_transcribe_without_carrier_choices() {
    let mut context = context();
    let list = context.fresh(Some("list"));
    let body = Term::let_(
        &list,
        Term::intrinsic(Intrinsic::ListType(Term::intrinsic(Intrinsic::NatType))),
        Term::intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: vec![nat_lit(1), nat_lit(2)],
        }),
        Term::intrinsic(Intrinsic::ListLen {
            element: Term::intrinsic(Intrinsic::NatType),
            list: Term::free_var(&list),
        }),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
    let x = context().fresh(Some("x"));
    // Both runs erase the *same* term, each through a reader made for it, so any difference would be erasure's own, not a difference in the binders handed to it.
    let build = || {
        let body = Term::let_(
            &x,
            Term::intrinsic(Intrinsic::NatType),
            nat_lit(2),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat_lit(3))),
        );
        let erased = erase(
            &module(Vec::new(), body),
            Term::intrinsic(Intrinsic::NatType),
        );
        shape(&erased)
    };
    assert_eq!(build(), build());
}

/// What an item's erasure hands on holds no binder it opened. A binder's identity counts the binders its context had minted before it, so two erasures of one unit would part on every binder they kept, and an item that read the erased form would hold an identity it mints again.
#[test]
fn an_erased_form_keeps_no_binder_its_erasure_opened() {
    let erased = |opened: usize| {
        let mut names = context();
        let x = names.fresh(Some("x"));
        let y = names.fresh(Some("y"));
        let nat = Term::intrinsic(Intrinsic::NatType);
        // One constant: the module indexes its constants by hash, and two would print in an order of the table's own.
        let body = Term::func(
            [(y, nat.clone())],
            Term::let_(
                &x,
                nat.clone(),
                nat_lit(2),
                Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), Term::free_var(&y))),
            ),
        );
        let unit = module(
            vec![definition(
                "bump",
                Term::func_type([(y, nat.clone())], nat.clone()),
                body,
            )],
            nat_lit(0),
        )
        .module;

        let unit = zonked(&unit);
        let view = super::lower::projected(Erasures::nothing(), unit.as_module())
            .expect("the unit's universes are its schemes'");
        let mut context = Retyping::over(DEFAULT_STEP_BUDGET, SYNTAX, &view);
        for _ in 0..opened {
            context.fresh(None);
        }

        super::lower::erase_items(
            &mut context,
            &view,
            Erasures::nothing(),
            unit.as_module(),
            None,
        )
        .expect("the unit erases")
    };
    let first = erased(0);
    let second = erased(8);

    assert_eq!(format!("{first:?}"), format!("{second:?}"));
}

/// A unit of two definitions, `callee` and `caller`, which is `callee` by name, and the same unit erased whole.
fn calling(callee: usize) -> (Zonked<Module>, ErasedUnit) {
    let nat = Term::intrinsic(Intrinsic::NatType);
    let unit = zonked(
        &module(
            vec![
                definition("callee", nat.clone(), nat_lit(callee)),
                definition("caller", nat, Term::free_var(&global("callee"))),
            ],
            nat_lit(0),
        )
        .module,
    );
    let erased = erase_unit(DEFAULT_STEP_BUDGET, SYNTAX, Erasures::nothing(), &unit)
        .expect("the unit erases whole");

    (unit, erased)
}

/// How a unit holds the item declaring `name`, as its printed erased form.
fn form(unit: &ErasedUnit, name: &str) -> String {
    format!(
        "{:?}",
        unit.find(&nominal(name)).expect("the unit declares it")
    )
}

/// A unit erased over a baseline erases the items the elaborator did not keep and keeps the others. `kept` is given another body since the baseline was erased and is named among the kept, so what the unit holds for it is still what the baseline erased it to: an item erased again would show the new body.
#[test]
fn a_unit_erased_over_a_baseline_erases_what_was_elaborated_again_and_no_other() {
    let nat = Term::intrinsic(Intrinsic::NatType);
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
    let baseline = erase_unit(DEFAULT_STEP_BUDGET, SYNTAX, Erasures::nothing(), &before)
        .expect("the baseline erases");
    let edited = unit(3, 4);
    let whole = erase_unit(DEFAULT_STEP_BUDGET, SYNTAX, Erasures::nothing(), &edited)
        .expect("the unit erases whole");
    let over = erase_unit_over(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &Places::of(before.as_module()),
            erased: &baseline,
            kept: &BTreeSet::from([nominal("kept")]),
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

    let over = erase_unit_over(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &Places::of(before.as_module()),
            erased: &baseline,
            kept: &BTreeSet::from([nominal("caller")]),
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

    let over = erase_unit_over(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &edited,
        ErasedBaseline {
            core: before.as_module(),
            places: &Places::of(before.as_module()),
            erased: &baseline,
            kept: &BTreeSet::from([nominal("caller")]),
        },
    )
    .expect("the unit erases over its baseline");
    let held = |unit: &ErasedUnit| {
        std::ptr::from_ref(unit.find(&nominal("caller")).expect("the unit declares it"))
    };

    assert_eq!(held(&over), held(&baseline), "`caller` was erased again");
}

#[test]
fn universe_erasure_is_a_validated_structural_projection() {
    let parameter = Level::param(UniverseParam(0));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["poly"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            quality_count: 0,
            constraints: Vec::new(),
        },
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        type_: Term::type_at(parameter.succ().unwrap()),
        body: Term::induct_type_at(
            nominal("Family"),
            [parameter],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };
    let source = stating(
        &module(
            vec![Item::Let(definition)],
            Term::instance_of(&global("poly"), vec![Level::constant(2)]),
        ),
        Term::type_at(Level::constant(3)),
    );

    let source_module = zonked(&source.module);
    let projected = super::lower::projected(Erasures::nothing(), source_module.as_module())
        .expect("the module's universes are its schemes'");
    let Item::Let(definition) = projected.own(0) else {
        panic!("expected definition")
    };
    assert_eq!(definition.universe_context, UniverseContext::empty());
    assert_eq!(definition.type_, Term::type_ground());
    let Subterm::InductType(induct) = &*definition.body else {
        panic!("expected nominal type")
    };
    assert!(induct.universes.is_empty());
    // The entry is projected beside the module by the same structural rule, its instance dropped.
    assert_eq!(
        project_erased_universes(&source.entry.body),
        Term::free_var(&global("poly"))
    );

    let invalid = module(
        vec![self::definition(
            "unsolved",
            Term::type_at(Level::constant(1)),
            Term::type_at(Level::meta(UniverseMetaId(0))),
        )],
        nat_lit(0),
    );
    // The zonk evidence refuses a level that holds a metavariable, so the module is handed over as it stands: the projection's own validation is what answers here.
    assert!(Zonked::project(&invalid.module).is_err());
    assert!(super::lower::projected(Erasures::nothing(), &invalid.module).is_err());
}

#[test]
fn a_function_erases_with_a_parameter_for_each_binder_and_no_captures() {
    let mut context = context();
    let type_param = context.fresh(Some("A"));
    let x = context.fresh(Some("x"));
    // (A : Type, x : A) => x — the type parameter is a parameter like the other, filled with the unit constant where a type is passed; the function stores no captures.
    let func_type = Term::func_type(
        [
            (type_param, Term::type_ground()),
            (x, Term::free_var(&type_param)),
        ],
        Term::free_var(&type_param),
    );
    let items = vec![definition(
        "id",
        func_type,
        Term::func(
            [(type_param, Term::type_ground()), (x, Term::type_ground())],
            Term::free_var(&x),
        ),
    )];
    let body = Term::apply(
        Term::free_var(&global("id")),
        [Term::intrinsic(Intrinsic::NatType), nat_lit(4)],
    );
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y_binder = context.fresh(Some("y"));
    // (y : Nat) => (x : Nat) => x + y — the inner closure references the outer parameter freely; analysis derives it, nothing is stored.
    let inner_type = Term::func_type(
        [(x, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let outer_type = Term::func_type(
        [(y_binder, Term::intrinsic(Intrinsic::NatType))],
        inner_type,
    );
    let items = vec![definition(
        "make",
        outer_type,
        Term::func(
            [(y_binder, Term::type_ground())],
            Term::func(
                [(x, Term::type_ground())],
                Term::intrinsic(Intrinsic::nat_add(
                    Term::free_var(&x),
                    Term::free_var(&y_binder),
                )),
            ),
        ),
    )];
    let expected = Term::func_type(
        [(y_binder, Term::intrinsic(Intrinsic::NatType))],
        Term::func_type(
            [(x, Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::NatType),
        ),
    );
    let erased = erase(&module(items, Term::free_var(&global("make"))), expected);

    assert!(
        bound(&erased).any(|rhs| matches!(
            rhs,
            curios_ersd::Rhs::Operation {
                operation: curios_ersd::Operation::NatAdd,
                ..
            }
        )),
        "the inner body adds"
    );
    // The inner closure's capture of `y` is derived, never stored: the outer parameter is the inner function's one free value.
    let analysis = curios_ersd::Analysis::analyze(&erased);
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

fn opt_type() -> Term {
    Term::induct_type(nominal("Opt"), Vec::<Term>::new(), Vec::<Term>::new())
}

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some` tag 1 (registry-sorted). Registered on the module so erasure seeds it.
fn opt_induct() -> InductDecl {
    let mut context = context();
    let x = context.fresh(Some("x"));
    InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::from([
            (
                Atom::from("none"),
                InductParam::new(Telescope::done(Vec::new()), vec![]),
            ),
            (
                Atom::from("some"),
                InductParam::new(
                    Telescope::build([(x, Term::intrinsic(Intrinsic::NatType))], Vec::new()),
                    vec![Plicity::Explicit],
                ),
            ),
        ]),
        result_sort: Term::type_ground(),
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
    let body = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        Atom::from("some"),
        [nat_lit(6)],
    );
    let fixture = Program {
        module: declaring(
            Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, opt_type());
    assert_eq!(
        shape(&erased),
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
        vec![FieldShape::Number]
    );
}

/// A row is its declaration's. The item declaring `Opt` registers the family, each field's shape with it; an item that constructs an `Opt` holds the family as an import, at its widths and saying nothing of a field; and a program holds one family, the declaration's.
#[test]
fn a_row_is_registered_by_the_item_that_declares_it() {
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let former = Item::Let(Definition {
        name: nominal("Opt"),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        type_: Term::type_ground(),
        body: opt_type(),
    });
    let six = definition(
        "six",
        opt_type(),
        Term::variant(
            nominal("Opt"),
            Vec::<Term>::new(),
            Atom::from("some"),
            [nat_lit(6)],
        ),
    );
    let fixture = Program {
        module: declaring(
            Module {
                mounts: Vec::new(),
                items: vec![former, six].into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: Entrypoint {
            body: Term::free_var(&global("six")),
            type_: None,
        },
    };
    // The shapes of `some`'s payload in the one family `module` holds.
    let payload = |module: &curios_ersd::Module| {
        let [family] = module.families() else {
            panic!("the module holds one family");
        };
        let [_, some] = family.constructors[..] else {
            panic!("Opt has two constructors");
        };

        module
            .constructor(some)
            .expect("some is registered")
            .fields
            .iter()
            .map(|field| field.shape)
            .collect::<Vec<_>>()
    };

    let unit = erase_unit(
        DEFAULT_STEP_BUDGET,
        SYNTAX,
        Erasures::nothing(),
        &zonked(&fixture.module),
    )
    .expect("the unit erases");
    let fragment = |name: &str| {
        unit.find(&nominal(name))
            .expect("the unit declares it")
            .module()
    };
    assert_eq!(payload(fragment("Opt")), vec![FieldShape::Number]);
    assert_eq!(payload(fragment("six")), vec![FieldShape::Opaque]);

    assert_eq!(
        payload(&erase(&fixture, opt_type())),
        vec![FieldShape::Number]
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let pair = context.fresh(Some("pair"));
    let tuple_type = Term::tuple_type([
        (a, Term::intrinsic(Intrinsic::NatType)),
        (b, Term::intrinsic(Intrinsic::NatType)),
    ]);
    let body = Term::let_(
        &pair,
        tuple_type.clone(),
        Term::tuple([nat_lit(1), nat_lit(2)]),
        Term::proj(Term::free_var(&pair), 1),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
    assert!(schema.shared);
    assert_eq!(schema.width(), 2);
}

#[test]
fn an_anonymous_tuple_keeps_a_field_for_its_proof() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let w = context.fresh(Some("w"));
    let sub = context.fresh(Some("sub"));
    // { x : Nat, w : Prop-valued } is two fields, the proof's holding the unit constant: an anonymous tuple has no declaration to say which it keeps, and a reader that knows `w`'s type as a variable reads two.
    let subset_type =
        Term::tuple_type([(x, Term::intrinsic(Intrinsic::NatType)), (w, Term::prop())]);
    let body = Term::let_(
        &sub,
        subset_type.clone(),
        Term::tuple([nat_lit(9), Term::prop()]),
        Term::proj(Term::free_var(&sub), 0),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
    let body = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(10),
        nat_lit(20),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // match n | 0 => 0 | succ pred (ih dead) => pred — a case split, not a fold.
    let items = vec![definition(
        "n",
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = Term::nat_match(
        Term::free_var(&global("n")),
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::free_var(&pred),
    );
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // match n | 0 => 0 | succ pred ih => ih + 2 — genuine induction.
    let items = vec![definition(
        "n",
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = Term::nat_match(
        Term::free_var(&global("n")),
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat_lit(2))),
    );
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let h = context.fresh(Some("h"));
    let t = context.fresh(Some("t"));
    let ih = context.fresh(Some("ih"));
    // match xs | [] => 0 | h :: t (ih) => ih + 1 — a length fold over a list.
    let list_ty = Term::intrinsic(Intrinsic::ListType(Term::intrinsic(Intrinsic::NatType)));
    let items = vec![definition(
        "xs",
        list_ty.clone(),
        Term::intrinsic(Intrinsic::List {
            element: Term::intrinsic(Intrinsic::NatType),
            items: vec![nat_lit(1)],
        }),
    )];
    let body = Term::list_match(
        Term::free_var(&global("xs")),
        Term::intrinsic(Intrinsic::NatType),
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(0),
        &h,
        &t,
        &ih,
        Term::intrinsic(Intrinsic::nat_add(Term::free_var(&ih), nat_lit(1))),
    );
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let x = context.fresh(Some("x"));
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let scrutinee = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        Atom::from("some"),
        [nat_lit(6)],
    );
    let body = Term::induct_match(
        scrutinee,
        None,
        Term::intrinsic(Intrinsic::NatType),
        [
            ("none", Vec::<Free>::new(), nat_lit(0)),
            ("some", vec![x], Term::free_var(&x)),
        ],
    );
    let fixture = Program {
        module: declaring(
            Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            [],
            [],
        ),
        entry: Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let x = context.fresh(Some("x"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // The peel path re-derives Core terms from the head (`n - 1`); an effectful compound head must still evaluate exactly once, through the alias.
    let io_read = Term::apply(Term::free_var(&global("read")), [nat_lit(0)]);
    let items = vec![definition(
        "read",
        Term::func_type(
            [(x, Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func([(x, Term::type_ground())], Term::free_var(&x)),
    )];
    let body = Term::nat_match(
        io_read,
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::free_var(&pred),
    );
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    let read = erased.function_ids().next().expect("the read function");
    let applications = bound(&erased)
        .filter(|rhs| {
            matches!(
                rhs,
                curios_ersd::Rhs::Apply {
                    callee: curios_ersd::Atom::Function(callee),
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
    let mut context = context();
    let x = context.fresh(Some("x"));
    let f = context.fresh(Some("f"));
    // rec f(x) = f(x); body f(3)
    let func_type = Term::func_type(
        [(x, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let body = Term::rec(
        vec![(
            f,
            func_type.clone(),
            Term::func(
                [(x, Term::type_ground())],
                Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
            ),
        )],
        Term::apply(Term::free_var(&f), [nat_lit(3)]),
    );
    let erased = erase(
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let u = context.fresh(Some("u"));
    let produce = context.fresh(Some("produce"));
    let consume = context.fresh(Some("consume"));
    // rec { produce() = consume; consume = produce } — a dormant knot: the computed member's initializer references the function, and the function body references the computed member (dormant until applied).
    let produce_type = Term::func_type(
        [(u, Term::tuple_type(Vec::<(Free, Term)>::new()))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let body = Term::rec(
        vec![
            (
                produce,
                produce_type.clone(),
                Term::func([(u, Term::type_ground())], nat_lit(5)),
            ),
            (consume, produce_type.clone(), Term::free_var(&produce)),
        ],
        Term::free_var(&consume),
    );
    let erased = erase(&module(Vec::new(), body), produce_type);
    assert_eq!(
        shape(&erased),
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
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    // rec a = b; b = a — a mutual value-level cycle no forcing order satisfies. The verifier rejects it; erasure surfaces the diagnostic as an error, never a panic. (A *self*-knot `rec loop = loop` is admitted only while nothing outside its initializer reads it, which is when the lowering drops it.)
    let type_ = Term::intrinsic(Intrinsic::NatType);
    let body = Term::rec(
        vec![
            (a, type_.clone(), Term::free_var(&b)),
            (b, type_.clone(), Term::free_var(&a)),
        ],
        Term::free_var(&a),
    );
    let error =
        try_erase(&module(Vec::new(), body), type_).expect_err("the value-level cycle is rejected");
    assert!(matches!(error, Error::EvaluationCycle { .. }), "{error:?}");
}

#[test]
fn top_level_recursive_items_erase_through_the_item_chain() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let func_type = Term::func_type(
        [(x, Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let items = vec![Item::Rec(RecItem::new(vec![Definition {
        name: Global::Authored(Qualifier::from(["go"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        type_: func_type.clone(),
        body: Term::func(
            [(x, Term::type_ground())],
            Term::apply(Term::free_var(&global("go")), [Term::free_var(&x)]),
        ),
    }]))];
    let body = Term::apply(Term::free_var(&global("go")), [nat_lit(1)]);
    let erased = erase(&module(items, body), Term::intrinsic(Intrinsic::NatType));
    assert_eq!(
        shape(&erased),
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
fn newtype_struct(field: &str, type_: Term) -> StructDecl {
    let mut context = context();
    let f = context.fresh(Some(field));
    StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::build([(f, type_)], ())),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

fn struct_type_of(name: &str) -> Term {
    Term::from(Subterm::StructType(StructType {
        name: nominal(name),
        universes: Universes::default(),
        params: Vec::new(),
    }))
}

/// A no-parameter family with the single unary constructor `mk(x: payload)`.
fn unary_induct(payload: Term) -> InductDecl {
    let mut context = context();
    let x = context.fresh(Some("x"));
    InductDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::done(Telescope::done(())),
        constructors: Vec::from([(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([(x, payload)], Vec::new()),
                vec![Plicity::Explicit],
            ),
        )]),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

/// The payload-shape classifier chases newtype chains and terminates on cycles: a payload wrapped in two single-field structs lands on `Nat` and records `Immediate`; a payload whose struct names itself is `Opaque` because the visited guard cuts the cycle (a self-referential struct elaborates — it is merely uninhabited); a boxed `Flt` payload records its carrier, `Flt`.
#[test]
fn payload_shapes_chase_newtype_chains_and_terminate_on_cycles() {
    let mut context = context();
    let mut struct_decls = BTreeMap::new();
    struct_decls.insert(
        nominal("Meters"),
        newtype_struct("m", Term::intrinsic(Intrinsic::NatType)),
    );
    struct_decls.insert(
        nominal("Outer"),
        newtype_struct("o", struct_type_of("Meters")),
    );
    struct_decls.insert(nominal("Loop"), newtype_struct("s", struct_type_of("Loop")));
    let induct_type =
        |family: &str| Term::induct_type(nominal(family), Vec::<Term>::new(), Vec::<Term>::new());
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Wrapped"), unary_induct(struct_type_of("Outer")));
    induct_decls.insert(nominal("Knotted"), unary_induct(struct_type_of("Loop")));
    induct_decls.insert(
        nominal("Boxed"),
        unary_induct(Term::intrinsic(Intrinsic::FltType)),
    );
    // A payload whose type is itself a single-constructor family chases through it — the collapsed encoding makes such a family *be* its payload — while a recursive single-constructor family terminates at the visited guard.
    induct_decls.insert(
        nominal("Inner"),
        unary_induct(Term::intrinsic(Intrinsic::NatType)),
    );
    induct_decls.insert(nominal("Chained"), unary_induct(induct_type("Inner")));
    induct_decls.insert(nominal("Selfy"), unary_induct(induct_type("Selfy")));

    // The families register lazily on first construction, and the classifier reads the declared field types, never the operands — so a `Nat` literal stands in for every payload value.
    let construct = |family: &str| {
        Term::variant(
            nominal(family),
            Vec::<Term>::new(),
            Atom::from("mk"),
            [nat_lit(6)],
        )
    };
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let c = context.fresh(Some("c"));
    let d = context.fresh(Some("d"));
    let e = context.fresh(Some("e"));
    let body = Term::tuple([
        construct("Wrapped"),
        construct("Knotted"),
        construct("Boxed"),
        construct("Chained"),
        construct("Selfy"),
    ]);
    let expected = Term::tuple_type([
        (a, induct_type("Wrapped")),
        (b, induct_type("Knotted")),
        (c, induct_type("Boxed")),
        (d, induct_type("Chained")),
        (e, induct_type("Selfy")),
    ]);
    let fixture = Program {
        module: declaring(
            Module {
                mounts: Vec::new(),
                items: Vec::new().into(),
                witnesses: BTreeSet::new(),
                tests: Vec::new(),
            },
            induct_decls,
            struct_decls,
            [],
        ),
        entry: Entrypoint { body, type_: None },
    };
    let erased = erase(&fixture, expected);

    assert_eq!(
        payload(&erased, "/Wrapped"),
        vec![("x", FieldShape::Number)]
    );
    assert_eq!(
        payload(&erased, "/Knotted"),
        vec![("x", FieldShape::Opaque)]
    );
    // `Boxed` holds a `Flt`, and the recorder names that carrier rather than merely withholding `immediate`.
    assert_eq!(payload(&erased, "/Boxed"), vec![("x", FieldShape::Flt)]);
    assert_eq!(
        payload(&erased, "/Chained"),
        vec![("x", FieldShape::Number)]
    );
    assert_eq!(payload(&erased, "/Selfy"), vec![("x", FieldShape::Opaque)]);
}

/// An applied lambda is transcribed as the function and the call it is, whatever its parameters are marked: erasure reads the callee's type off the lambda itself and holds no opinion on what a declaration may type a `use` parameter at.
#[test]
fn an_applied_lambda_with_a_witness_slot_erases() {
    let mut context = context();
    let nat = Term::intrinsic(Intrinsic::NatType);
    let held = context.fresh(Some("held"));
    let x = context.fresh(Some("x"));
    let lambda = Term::func_marked(
        [
            (Plicity::Witness, held, struct_type_of("Dict")),
            (Plicity::Explicit, x, nat.clone()),
        ],
        Term::free_var(&x),
    );
    let call = Term::apply_marked(
        lambda,
        [
            (
                Plicity::Witness,
                Term::struct_at(
                    nominal("Dict"),
                    Vec::<Level>::new(),
                    Vec::<Term>::new(),
                    [nat_lit(1)],
                ),
            ),
            (Plicity::Explicit, nat_lit(3)),
        ],
    );
    let mut fixture = module(Vec::new(), call);
    fixture.module = declaring(
        std::mem::take(&mut fixture.module),
        [],
        [(nominal("Dict"), newtype_struct("held", nat.clone()))],
        [],
    );

    let erased = erase(&fixture, nat);

    assert_eq!(
        shape(&erased),
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
    step: impl Fn(&mut Retyping, Term) -> Term,
    eliminate: impl Fn(Term) -> Term,
) -> u64 {
    let mut context = context();
    let nat = Term::intrinsic(Intrinsic::NatType);
    let mut domain = nat.clone();
    for _ in 0..depth {
        domain = step(&mut context, domain);
    }
    let x = context.fresh(Some("x"));
    let mut body = Term::free_var(&x);
    for _ in 0..depth {
        body = eliminate(body);
    }
    let function = Term::func([(x, domain.clone())], body);
    let function_type = Term::func_type_marked([(Plicity::Explicit, x, domain)], nat.clone());

    let unit = zonked(
        &module(
            vec![definition("reads", function_type, function)],
            nat_lit(0),
        )
        .module,
    );
    // On a grown stack, as every entry point erases: a chain this deep is walked a frame a level.
    grown(|| {
        let view = super::lower::projected(Erasures::nothing(), unit.as_module())
            .expect("the unit's universes are its schemes'");
        let mut reader = Retyping::over(DEFAULT_STEP_BUDGET, SYNTAX, &view);
        super::lower::erase_items(
            &mut reader,
            &view,
            Erasures::nothing(),
            unit.as_module(),
            None,
        )
        .expect("the unit erases");

        reader.consumed().units()
    })
}

/// A head's type is read once however many eliminations stand on it, so twice the chain is at most twice the work. Reading each head afresh at every level it sits under makes it four times.
#[test]
fn a_projection_chain_erases_in_work_proportional_to_its_depth() {
    let cost = |depth| {
        chain_cost(
            depth,
            |context, inner| {
                let kept = context.fresh(Some("kept"));
                let pad = context.fresh(Some("pad"));
                Term::tuple_type([(kept, inner), (pad, Term::intrinsic(Intrinsic::NatType))])
            },
            |head| Term::proj(head, 0),
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
            |context, inner| {
                let n = context.fresh(Some("n"));
                Term::func_type_marked(
                    [(Plicity::Explicit, n, Term::intrinsic(Intrinsic::NatType))],
                    inner,
                )
            },
            |head| Term::apply(head, [nat_lit(1)]),
        )
    };
    let (short, long) = (cost(256), cost(512));

    assert!(
        long < 2 * short,
        "a 512-deep chain spent {long} where a 256-deep one spent {short}",
    );
}

/// Erasure's reader is handed no proof's body either: out of its universes a definition marked a proof unfolds to nothing, and the definition beside it to its body.
#[test]
fn a_proofs_body_is_withheld_from_erasures_reader() {
    let marked = |item| match item {
        Item::Let(definition) => Item::Let(Definition {
            proof: true,
            ..definition
        }),
        item => item,
    };
    let unit = module(
        vec![
            marked(definition("lemma", Term::prop(), Term::prop())),
            definition("two", Term::intrinsic(Intrinsic::NatType), nat_lit(2)),
        ],
        nat_lit(0),
    )
    .module;

    let view = Projected::over(Erasures::nothing(), &unit);
    let unfolds = |path| {
        view.definition(&nominal(path))
            .expect("the unit declares it")
            .body
            .is_some()
    };
    assert!(!unfolds("lemma"));
    assert!(unfolds("two"));
}

/// A record of two fields at one type, `levels` deep: a graph of `levels` nodes over a tree of two to the `levels` leaves. Its binders are minted from `mint`, so two such types name no binder in common.
fn doubled_record(levels: u32, mint: u32) -> Term {
    (0..levels).fold(Term::intrinsic(Intrinsic::NatType), |type_, level| {
        let first = Free::local(mint + 2 * level, None);
        let second = Free::local(mint + 2 * level + 1, None);

        Term::tuple_type([(first, type_.clone()), (second, type_)])
    })
}

/// Erasure answers a term that shares in the term's own size: sixty levels that each hold the one below twice, a tree no walk per path finishes, as `walk_tests` puts the crate's other walks to one. A function at the doubling type, as a program's entry and as a definition; a chain of sixty projections out of a value at it; and sixty definitions that each pair the one before with itself, projected, whose type is its tail's with every binding substituted (`Lowering::type_of`).
///
/// Mutation-checked: with a type read again where `Lowering::type_of` kept one, the row of definitions does not return, and with the type a lambda is checked at walked once per occurrence, none of the other three does.
#[test]
fn erasure_answers_a_doubling_term_in_its_own_size() {
    let nat_type = || Term::intrinsic(Intrinsic::NatType);
    let doubled = doubled_record(60, 5_000);

    let identity = |mint: u32| {
        let a = Free::local(mint, Some("a"));
        (
            Term::func([(a, doubled.clone())], Term::free_var(&a)),
            Term::func_type([(a, doubled.clone())], doubled.clone()),
        )
    };
    let (entry, entry_type) = identity(4_000);
    let (held, held_type) = identity(4_001);

    let b = Free::local(4_002, Some("b"));
    let chain = (0..60).fold(Term::free_var(&b), |head, _| Term::proj(head, 0usize));

    // `(let x0 = (n, n); let x1 = (x0, x0); …; x59).0`, each definition stated at its own doubling type: the projection's head is the whole chain, whose type is read by substituting every binding.
    let n = Free::local(4_003, Some("n"));
    let names = (0..60u32)
        .map(|level| Free::local(4_100 + level, Some("x")))
        .collect::<Vec<_>>();
    let mut paired = Term::free_var(&names[59]);
    for level in (0..60u32).rev() {
        let below = match level {
            0 => Term::free_var(&n),
            _ => Term::free_var(&names[level as usize - 1]),
        };
        paired = Term::let_(
            &names[level as usize],
            doubled_record(level + 1, 10_000 + 200 * level),
            Term::tuple([below.clone(), below]),
            paired,
        );
    }
    let paired = Term::proj(paired, 0usize);

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
                &module(Vec::new(), Term::func([(b, doubled.clone())], chain)),
                Term::func_type([(b, doubled.clone())], nat_type()),
            )
            .is_ok(),
        ),
        (
            "sixty definitions that each pair the one before",
            try_erase(
                &module(Vec::new(), Term::func([(n, nat_type())], paired)),
                Term::func_type([(n, nat_type())], doubled_record(59, 30_000)),
            )
            .is_ok(),
        ),
    ];

    for (row, erased) in rows {
        assert!(erased, "{row} did not erase");
    }
}
