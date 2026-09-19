use crate::reduce::test_support::qed;
use curios_core::Zonked;
use curios_core::*;
use curios_ersd::{FieldShape, Sign, test_support::shape};
use {
    crate::*,
    curios_analysis::fixture::SYNTAX,
    curios_utilities::{Plicity, Qualifier},
    std::collections::{BTreeMap, BTreeSet},
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn context() -> Context {
    Context::with_default_budget(SYNTAX)
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
        type_,
        body,
    })
}

fn module(items: Vec<Item>, body: Term) -> Module {
    Module {
        mounts: Vec::new(),
        items,
        universe_seeds: vec![],
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint { body, type_: None }),
    }
}

fn zonked(module: &Module) -> Zonked<Module> {
    Zonked::project(module).expect("the fixture is zonked")
}

fn erase(context: &mut Context, module: &Module, expected: Term) -> curios_ersd::Module {
    erase_module(context, &zonked(module), &expected).expect("the module erases")
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
        &mut context,
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
        &mut context,
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
    let mut context = context();
    let items = vec![definition(
        "x",
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(5),
    )];
    let body = Term::intrinsic(Intrinsic::Nat(Nat::Succ(
        3u32.into(),
        Term::free_var(&global("x")),
    )));
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v0 = Operation NatAdd [Nat(3), Nat(5)]
  Return ~v0
"
    );
}

#[test]
fn items_erase_in_dominance_order() {
    let mut context = context();
    // `a` references `b`, which is declared after it; the item chain must reorder so every reference is backward.
    let items = vec![
        definition(
            "a",
            Term::intrinsic(Intrinsic::NatType),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&global("b")), nat_lit(1))),
        ),
        definition("b", Term::intrinsic(Intrinsic::NatType), nat_lit(2)),
    ];
    let erased = erase(
        &mut context,
        &module(items, Term::free_var(&global("a"))),
        Term::intrinsic(Intrinsic::NatType),
    );
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
    // let dead = /std/proc/exit(3); 7 — the trailing computation is *not* dead any more, and that is the point. `exit` returns an `Io`, so binding it builds a description and performs nothing; the entry goes on to return 7. What the exit still does is seal the block it is written in — the thunk's, which ends on the terminator with no return after it.
    let body = Term::let_(
        &dead,
        Term::tuple_type_unit(),
        Term::intrinsic(Intrinsic::proc_exit(Term::tuple_type_unit(), nat_lit(3))),
        nat_lit(7),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Functions
    function ~f0$dead()
      Exit Nat(3)
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
        Term::func_type([(n.clone(), nat.clone())], result_type.clone()),
        Term::func([(n.clone(), nat.clone())], result),
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
    erase(&mut context, &module(items, body), nat)
}

fn calls(erased: &curios_ersd::Module) -> usize {
    bound(erased)
        .filter(|rhs| matches!(rhs, curios_ersd::Rhs::Apply { .. }))
        .count()
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
    // The control: the same binding at a `Nat` is evaluated under call-by-value, used or not.
    for binder in [Binder::Let, Binder::Item] {
        let erased = a_bound_call(binder, false);
        assert_eq!(calls(&erased), 1, "{}", shape(&erased));
    }
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
        &mut context,
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
    let mut context = context();
    let x = context.fresh(Some("x"));
    // Both runs erase the *same* term under a fresh context, so any difference would be erasure's own, not a difference in the binders handed to it.
    let build = |context: &mut Context| {
        let body = Term::let_(
            &x,
            Term::intrinsic(Intrinsic::NatType),
            nat_lit(2),
            Term::intrinsic(Intrinsic::nat_add(Term::free_var(&x), nat_lit(3))),
        );
        let erased = erase(
            context,
            &module(Vec::new(), body),
            Term::intrinsic(Intrinsic::NatType),
        );
        shape(&erased)
    };
    assert_eq!(build(&mut context), build(&mut self::context()));
}

#[test]
fn universe_erasure_is_a_validated_structural_projection() {
    let parameter = Level::param(UniverseParam(0));
    let definition = Definition {
        name: Global::Authored(Qualifier::from(["poly"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext {
            parameter_count: 1,
            constraints: Vec::new(),
        },
        island: Qualifier::empty(),
        totality: Totality::default(),
        type_: Term::type_at(parameter.succ().unwrap()),
        body: Term::induct_type_at(
            nominal("Family"),
            [parameter],
            Vec::<Term>::new(),
            Vec::<Term>::new(),
        ),
    };
    let source = module(
        vec![Item::Let(definition)],
        Term::instance_of(&global("poly"), vec![Level::constant(2)]),
    );

    let projected = super::lower::UniverseErased::<Zonked<Module>>::project(&zonked(&source))
        .unwrap()
        .into_inner()
        .into_module();
    let Item::Let(definition) = &projected.items[0] else {
        panic!("expected definition")
    };
    assert_eq!(definition.universe_context, UniverseContext::empty());
    assert_eq!(definition.type_, Term::type_ground());
    let Subterm::InductType(induct) = &*definition.body else {
        panic!("expected nominal type")
    };
    assert!(induct.universes.is_empty());
    assert_eq!(
        projected.entry.as_ref().map(|entry| &entry.body),
        Some(&Term::free_var(&global("poly")))
    );

    let invalid = module(Vec::new(), Term::type_at(Level::meta(UniverseMetaId(0))));
    assert!(super::lower::UniverseErased::<Zonked<Module>>::project(&zonked(&invalid)).is_err());
}

#[test]
fn a_function_erases_with_dropped_type_params_and_no_captures() {
    let mut context = context();
    let type_param = context.fresh(Some("A"));
    let x = context.fresh(Some("x"));
    // (A : Type, x : A) => x — the type parameter is dropped; the runtime function takes one parameter and stores no captures.
    let func_type = Term::func_type(
        [
            (type_param.clone(), Term::type_ground()),
            (x.clone(), Term::free_var(&type_param)),
        ],
        Term::free_var(&type_param),
    );
    let items = vec![definition(
        "id",
        func_type,
        Term::func(
            [
                (type_param.clone(), Term::type_ground()),
                (x.clone(), Term::type_ground()),
            ],
            Term::free_var(&x),
        ),
    )];
    let body = Term::apply(
        Term::free_var(&global("id")),
        [Term::intrinsic(Intrinsic::NatType), nat_lit(4)],
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
items
  Functions
    function ~f0$/id(~v0$x)
      Return ~v0$x
entry
  Let ~v1 = Apply ~f0$/id [Nat(4)]
  Return ~v1
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
        [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let outer_type = Term::func_type(
        [(y_binder.clone(), Term::intrinsic(Intrinsic::NatType))],
        inner_type,
    );
    let items = vec![definition(
        "make",
        outer_type,
        Term::func(
            [(y_binder.clone(), Term::type_ground())],
            Term::func(
                [(x.clone(), Term::type_ground())],
                Term::intrinsic(Intrinsic::nat_add(
                    Term::free_var(&x),
                    Term::free_var(&y_binder),
                )),
            ),
        ),
    )];
    let expected = Term::func_type(
        [(y_binder.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::func_type(
            [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::NatType),
        ),
    );
    let erased = erase(
        &mut context,
        &module(items, Term::free_var(&global("make"))),
        expected,
    );

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
                    Telescope::build(
                        [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
                        Vec::new(),
                    ),
                    vec![Plicity::Explicit],
                ),
            ),
        ]),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        rep_public: true,
        polarities: Vec::new(),
    }
}

#[test]
fn a_variant_constructs_with_its_registered_schema() {
    let mut context = context();
    let mut induct_decls = BTreeMap::new();
    induct_decls.insert(nominal("Opt"), opt_induct());
    let body = Term::variant(
        nominal("Opt"),
        Vec::<Term>::new(),
        Atom::from("some"),
        [nat_lit(6)],
    );
    let fixture = Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: vec![],
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint { body, type_: None }),
    };
    let erased =
        erase_module(&mut context, &zonked(&fixture), &opt_type()).expect("the module erases");
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v0 = Construct ~t1 [Nat(6)]
  Return ~v0
"
    );
    // The family the construction registered against, asked of the arena the header used to render: a nullary `none` and a `some` carrying one immediate, with `~t1` — the constructed one — the second.
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
        vec![FieldShape::Immediate(Sign::Unsigned)]
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let pair = context.fresh(Some("pair"));
    let tuple_type = Term::tuple_type([
        (a.clone(), Term::intrinsic(Intrinsic::NatType)),
        (b.clone(), Term::intrinsic(Intrinsic::NatType)),
    ]);
    let body = Term::let_(
        &pair,
        tuple_type.clone(),
        Term::tuple([nat_lit(1), nat_lit(2)]),
        Term::proj(Term::free_var(&pair), 1),
    );
    let erased = erase(
        &mut context,
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
    // The width row itself — that it is the interned shared one, which is the test's subject and what the header used to render.
    let [schema] = erased.products() else {
        panic!("the fixture registers one schema");
    };
    assert!(schema.shared);
    assert_eq!(schema.width(), 2);
}

#[test]
fn a_subset_tuple_collapses_to_its_relevant_field() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let w = context.fresh(Some("w"));
    let sub = context.fresh(Some("sub"));
    // { x : Nat, w : Prop-valued } erases to the bare Nat; its projection vanishes.
    let subset_type = Term::tuple_type([
        (x.clone(), Term::intrinsic(Intrinsic::NatType)),
        (w.clone(), Term::prop()),
    ]);
    let body = Term::let_(
        &sub,
        subset_type.clone(),
        Term::tuple([nat_lit(9), Term::prop()]),
        Term::proj(Term::free_var(&sub), 0),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Return Nat(9)
"
    );
}

#[test]
fn a_bool_match_erases_to_a_switch_bool() {
    let mut context = context();
    let body = Term::bool_match(
        Term::intrinsic(Intrinsic::Bool(true)),
        None,
        Term::intrinsic(Intrinsic::NatType),
        nat_lit(10),
        nat_lit(20),
    );
    let erased = erase(
        &mut context,
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
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v1 = SwitchNat Nat(5)
    case 0
      Return Nat(0)
    default
      Let ~v0 = Operation NatSub [Nat(5), Nat(1)]
      Return ~v0
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
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
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
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
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
            ("some", vec![x.clone()], Term::free_var(&x)),
        ],
    );
    let fixture = Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: vec![],
        induct_decls,
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint { body, type_: None }),
    };
    let erased = erase_module(
        &mut context,
        &zonked(&fixture),
        &Term::intrinsic(Intrinsic::NatType),
    )
    .expect("the module erases");
    assert_eq!(
        shape(&erased),
        "\
entry
  Let ~v0$scrutinee = Construct ~t1 [Nat(6)]
  Let ~v2 = MatchVariant ~d0 ~v0$scrutinee
    arm ~t0()
      Return Nat(0)
    arm ~t1(~v1$x)
      Return ~v1$x
  Return ~v2
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
            [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
            Term::intrinsic(Intrinsic::NatType),
        ),
        Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
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
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
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
        [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let body = Term::rec(
        vec![(
            f.clone(),
            func_type.clone(),
            Term::func(
                [(x.clone(), Term::type_ground())],
                Term::apply(Term::free_var(&f), [Term::free_var(&x)]),
            ),
        )],
        Term::apply(Term::free_var(&f), [nat_lit(3)]),
    );
    let erased = erase(
        &mut context,
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
        [(u.clone(), Term::tuple_type(Vec::<(Free, Term)>::new()))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let body = Term::rec(
        vec![
            (
                produce.clone(),
                produce_type.clone(),
                Term::func([(u.clone(), Term::type_ground())], nat_lit(5)),
            ),
            (
                consume.clone(),
                produce_type.clone(),
                Term::free_var(&produce),
            ),
        ],
        Term::free_var(&consume),
    );
    let erased = erase(&mut context, &module(Vec::new(), body), produce_type);
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
            (a.clone(), type_.clone(), Term::free_var(&b)),
            (b.clone(), type_.clone(), Term::free_var(&a)),
        ],
        Term::free_var(&a),
    );
    let error = erase_module(&mut context, &zonked(&module(Vec::new(), body)), &type_)
        .expect_err("the value-level cycle is rejected");
    assert!(matches!(error, Error::EvaluationCycle { .. }), "{error:?}");
}

#[test]
fn top_level_recursive_items_erase_through_the_item_chain() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let func_type = Term::func_type(
        [(x.clone(), Term::intrinsic(Intrinsic::NatType))],
        Term::intrinsic(Intrinsic::NatType),
    );
    let items = vec![Item::Rec(RecItem::new(vec![Definition {
        name: Global::Authored(Qualifier::from(["go"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        type_: func_type.clone(),
        body: Term::func(
            [(x.clone(), Term::type_ground())],
            Term::apply(Term::free_var(&global("go")), [Term::free_var(&x)]),
        ),
    }]))];
    let body = Term::apply(Term::free_var(&global("go")), [nat_lit(1)]);
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::intrinsic(Intrinsic::NatType),
    );
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
    }
}

fn struct_type_of(name: &str) -> Term {
    Term::from(Subterm::StructType(StructType {
        name: nominal(name),
        universes: Vec::new(),
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
    }
}

/// The payload-shape classifier chases newtype chains and terminates on cycles: a payload wrapped in two single-field structs lands on `Nat` and records `Immediate`; a payload whose struct names itself is `Opaque` because the visited guard cuts the cycle (a self-referential struct elaborates — it is merely uninhabited); a boxed `Flt` payload stays `Opaque`.
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
    let fixture = Module {
        mounts: Vec::new(),
        items: Vec::new(),
        universe_seeds: vec![],
        induct_decls,
        struct_decls,
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
        binder_floor: 0,
        entry: Some(Entrypoint { body, type_: None }),
    };
    let erased =
        erase_module(&mut context, &zonked(&fixture), &expected).expect("the module erases");

    let immediate = FieldShape::Immediate(Sign::Unsigned);
    assert_eq!(payload(&erased, "/Wrapped"), vec![("x", immediate)]);
    assert_eq!(
        payload(&erased, "/Knotted"),
        vec![("x", FieldShape::Opaque)]
    );
    // `Boxed` was this fixture's "not immediate" example; the full recorder now names its carrier instead of merely withholding `immediate`.
    assert_eq!(payload(&erased, "/Boxed"), vec![("x", FieldShape::Flt)]);
    assert_eq!(payload(&erased, "/Chained"), vec![("x", immediate)]);
    assert_eq!(payload(&erased, "/Selfy"), vec![("x", FieldShape::Opaque)]);
}
