use {
    crate::*,
    curios_base::{Plicity, Qualifier, RootId},
    std::collections::{BTreeMap, BTreeSet},
};

/// A declaration's name, from the path a test writes. Fixture-only.
fn nominal(path: &str) -> crate::Global {
    crate::Global::Authored(curios_base::Qualifier::from([path]))
}

fn context() -> Context {
    Context::with_default_budget()
}

/// A top-level definition's identity, from the path a test writes — the same
/// name [`definition`] declares it under. Fixture-only.
fn global(path: &str) -> crate::Free {
    crate::Free::global(Qualifier::from([path]))
}

fn nat_lit(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

fn definition(name: &str, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: Global::Authored(Qualifier::from([name])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        root: RootId::Entry,
        totality: Totality::default(),
        type_,
        body,
    })
}

fn module(items: Vec<Item>, body: Term) -> Module {
    Module {
        items,
        universe_seeds: vec![],
        induct_decls: BTreeMap::new(),
        struct_decls: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        binder_floor: 0,
        type_: None,
        body,
    }
}

fn erase(context: &mut Context, module: &Module, expected: Term) -> curios_ersd::Module {
    erase_module(context, module, &expected).expect("the module erases")
}

#[test]
fn a_scalar_expression_erases_in_evaluation_order() {
    let mut context = context();
    let x_binder = context.fresh(Some("x"));
    // let x = 2; x + 3
    let body = Term::let_(
        &x_binder,
        Term::prim(Prim::NatType),
        nat_lit(2),
        Term::prim(Prim::nat_add(Term::free_var(&x_binder), nat_lit(3))),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0 = NatAdd(2, 3)
  return ~v0
}
"
    );
}

#[test]
fn bool_and_byte_keep_their_shapes() {
    let mut context = context();
    let b = context.fresh(Some("b"));
    // Bool stays Bool-shaped and Byte stays Byte-shaped: no Nat carrier appears
    // anywhere in the erased output.
    let body = Term::let_(
        &b,
        Term::prim(Prim::BoolType),
        Term::prim(Prim::BoolAnd(
            Term::prim(Prim::Bool(true)),
            Term::prim(Prim::Bool(false)),
        )),
        Term::prim(Prim::ByteEql(
            Term::prim(Prim::Byte(7)),
            Term::prim(Prim::Byte(8)),
        )),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::prim(Prim::BoolType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0$b = BoolAnd(true, false)
  ~v1 = ByteEql(7:byte, 8:byte)
  return ~v1
}
"
    );
}

#[test]
fn a_nat_spine_over_a_variable_erases_to_one_addition() {
    let mut context = context();
    let items = vec![definition("x", Term::prim(Prim::NatType), nat_lit(5))];
    let body = Term::prim(Prim::Nat(Nat::Succ(
        3u32.into(),
        Term::free_var(&global("x")),
    )));
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0 = NatAdd(3, 5)
  return ~v0
}
"
    );
}

#[test]
fn items_erase_in_dominance_order() {
    let mut context = context();
    // `a` references `b`, which is declared after it; the item chain must
    // reorder so every reference is backward.
    let items = vec![
        definition(
            "a",
            Term::prim(Prim::NatType),
            Term::prim(Prim::nat_add(Term::free_var(&global("b")), nat_lit(1))),
        ),
        definition("b", Term::prim(Prim::NatType), nat_lit(2)),
    ];
    let erased = erase(
        &mut context,
        &module(items, Term::free_var(&global("a"))),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
~v0$/a = NatAdd(2, 1)
entry {
  return ~v0$/a
}
"
    );
}

#[test]
fn an_exit_seals_the_block_and_drops_dead_code() {
    let mut context = context();
    let dead = context.fresh(Some("dead"));
    // let _ = /std/proc/exit(3); 7 — the trailing computation is dead.
    let body = Term::let_(
        &dead,
        Term::prim(Prim::NatType),
        Term::prim(Prim::Exit(Term::prim(Prim::NatType), nat_lit(3))),
        nat_lit(7),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  exit 3
}
"
    );
}

#[test]
fn sequences_transcribe_without_carrier_choices() {
    let mut context = context();
    let lst = context.fresh(Some("lst"));
    let body = Term::let_(
        &lst,
        Term::prim(Prim::LstType(Term::prim(Prim::NatType))),
        Term::prim(Prim::Lst(vec![nat_lit(1), nat_lit(2)])),
        Term::prim(Prim::LstLen(
            Term::prim(Prim::NatType),
            Term::free_var(&lst),
        )),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0$lst = LstBuild(1, 2)
  ~v1 = LstLen(~v0$lst)
  return ~v1
}
"
    );
}

#[test]
fn erasure_is_deterministic() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    // Both runs erase the *same* term under a fresh context, so any difference
    // would be erasure's own, not a difference in the binders handed to it.
    let build = |context: &mut Context| {
        let body = Term::let_(
            &x,
            Term::prim(Prim::NatType),
            nat_lit(2),
            Term::prim(Prim::nat_add(Term::free_var(&x), nat_lit(3))),
        );
        erase(
            context,
            &module(Vec::new(), body),
            Term::prim(Prim::NatType),
        )
        .to_string()
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
        root: RootId::Entry,
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
        Term::universe_inst(Term::free_var(&global("poly")), vec![Level::constant(2)]),
    );

    let projected = super::lower::UniverseErased::<Module>::project(&source)
        .unwrap()
        .into_inner();
    let Item::Let(definition) = &projected.items[0] else {
        panic!("expected definition")
    };
    assert_eq!(definition.universe_context, UniverseContext::empty());
    assert_eq!(definition.type_, Term::type_ground());
    let Subterm::InductType(induct) = &*definition.body else {
        panic!("expected nominal type")
    };
    assert!(induct.universes.is_empty());
    assert_eq!(projected.body, Term::free_var(&global("poly")));

    let invalid = module(Vec::new(), Term::type_at(Level::meta(UniverseMetaId(0))));
    assert!(super::lower::UniverseErased::<Module>::project(&invalid).is_err());
}

#[test]
fn a_function_erases_with_dropped_type_params_and_no_captures() {
    let mut context = context();
    let type_param = context.fresh(Some("A"));
    let x = context.fresh(Some("x"));
    // (A : Type, x : A) => x — the type parameter is dropped; the runtime
    // function takes one parameter and stores no captures.
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
        [Term::prim(Prim::NatType), nat_lit(4)],
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
functions ~f0$/id
entry {
  ~v1 = apply ~f0$/id(4)
  return ~v1
}
function ~f0$/id(~v0$x) {
  return ~v0$x
}
"
    );
}

#[test]
fn a_capturing_closure_stores_no_capture_list() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let y_binder = context.fresh(Some("y"));
    // (y : Nat) => (x : Nat) => x + y — the inner closure references the
    // outer parameter freely; analysis derives it, nothing is stored.
    let inner_type = Term::func_type(
        [(x.clone(), Term::prim(Prim::NatType))],
        Term::prim(Prim::NatType),
    );
    let outer_type = Term::func_type([(y_binder.clone(), Term::prim(Prim::NatType))], inner_type);
    let items = vec![definition(
        "make",
        outer_type,
        Term::func(
            [(y_binder.clone(), Term::type_ground())],
            Term::func(
                [(x.clone(), Term::type_ground())],
                Term::prim(Prim::nat_add(Term::free_var(&x), Term::free_var(&y_binder))),
            ),
        ),
    )];
    let expected = Term::func_type(
        [(y_binder.clone(), Term::prim(Prim::NatType))],
        Term::func_type(
            [(x.clone(), Term::prim(Prim::NatType))],
            Term::prim(Prim::NatType),
        ),
    );
    let erased = erase(
        &mut context,
        &module(items, Term::free_var(&global("make"))),
        expected,
    );

    let printed = erased.to_string();
    assert!(printed.contains("function ~f0$/make(~v0$y)"), "{printed}");
    assert!(printed.contains("NatAdd("), "{printed}");
    // The inner closure's capture of `y` is derived, never stored: the outer
    // parameter is the inner function's one free value.
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

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some`
// tag 1 (registry-sorted). Registered on the module so erasure seeds it.
fn opt_induct() -> InductDecl {
    let mut context = context();
    let x = context.fresh(Some("x"));
    InductDecl {
        universe_context: UniverseContext::empty(),
        params: Telescope::done(()),
        indices: Telescope::done(()),
        constructors: Vec::from([
            (
                Atom::from("none"),
                InductParam {
                    telescope: Telescope::done(opt_type()),
                    plicities: vec![],
                },
            ),
            (
                Atom::from("some"),
                InductParam {
                    telescope: Telescope::build(
                        [(x.clone(), Term::prim(Prim::NatType))],
                        opt_type(),
                    ),
                    plicities: vec![Plicity::Explicit],
                },
            ),
        ]),
        result_sort: Term::type_ground(),
        module: Qualifier::empty(),
        root: RootId::Entry,
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
    let erased = erase_module(
        &mut context,
        &Module {
            items: Vec::new(),
            universe_seeds: vec![],
            induct_decls,
            struct_decls: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            binder_floor: 0,
            type_: None,
            body,
        },
        &opt_type(),
    )
    .expect("the module erases");
    assert_eq!(
        erased.to_string(),
        "\
family ~d0$/Opt { ~t0$none() ~t1$some(x) }
entry {
  ~v0 = construct ~t1(6)
  return ~v0
}
"
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    let pair = context.fresh(Some("pair"));
    let tuple_type = Term::tuple_type([
        (a.clone(), Term::prim(Prim::NatType)),
        (b.clone(), Term::prim(Prim::NatType)),
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
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
product ~p0(0, 1)
entry {
  ~v0$pair = product ~p0(1, 2)
  ~v1 = project ~p0.1 ~v0$pair
  return ~v1
}
"
    );
}

#[test]
fn a_subset_tuple_collapses_to_its_relevant_field() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let w = context.fresh(Some("w"));
    let sub = context.fresh(Some("sub"));
    // { x : Nat, w : Prop-valued } erases to the bare Nat; its projection
    // vanishes.
    let subset_type = Term::tuple_type([
        (x.clone(), Term::prim(Prim::NatType)),
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
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  return 9
}
"
    );
}

#[test]
fn a_bool_match_erases_to_a_switch_bool() {
    let mut context = context();
    let body = Term::bool_match(
        Term::prim(Prim::Bool(true)),
        None,
        Term::prim(Prim::NatType),
        nat_lit(10),
        nat_lit(20),
    );
    let erased = erase(
        &mut context,
        &module(Vec::new(), body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0 = switch-bool true {
    false => {
      return 10
    }
    true => {
      return 20
    }
  }
  return ~v0
}
"
    );
}

#[test]
fn a_dead_hypothesis_nat_match_peels_to_a_dispatch() {
    let mut context = context();
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // match n | 0 => 0 | succ pred (ih dead) => pred — a case split, not a fold.
    let items = vec![definition("n", Term::prim(Prim::NatType), nat_lit(5))];
    let body = Term::nat_match(
        Term::free_var(&global("n")),
        None,
        Term::prim(Prim::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::free_var(&pred),
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v1 = switch-nat 5 {
    0 => {
      return 0
    }
    default => {
      ~v0 = NatSub(5, 1)
      return ~v0
    }
  }
  return ~v1
}
"
    );
}

#[test]
fn a_live_hypothesis_nat_match_erases_to_a_fold() {
    let mut context = context();
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // match n | 0 => 0 | succ pred ih => ih + 2 — genuine induction.
    let items = vec![definition("n", Term::prim(Prim::NatType), nat_lit(5))];
    let body = Term::nat_match(
        Term::free_var(&global("n")),
        None,
        Term::prim(Prim::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::prim(Prim::nat_add(Term::free_var(&ih), nat_lit(2))),
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v3 = fold-nat 5 {
    zero => {
      return 0
    }
    step(~v0$pred, ~v1$ih) => {
      ~v2 = NatAdd(~v1$ih, 2)
      return ~v2
    }
  }
  return ~v3
}
"
    );
}

#[test]
fn a_live_hypothesis_lst_match_erases_to_a_sequence_fold() {
    let mut context = context();
    let h = context.fresh(Some("h"));
    let t = context.fresh(Some("t"));
    let ih = context.fresh(Some("ih"));
    // match xs | [] => 0 | h :: t (ih) => ih + 1 — a length fold over a list.
    let lst_ty = Term::prim(Prim::LstType(Term::prim(Prim::NatType)));
    let items = vec![definition(
        "xs",
        lst_ty.clone(),
        Term::prim(Prim::Lst(vec![nat_lit(1)])),
    )];
    let body = Term::lst_match(
        Term::free_var(&global("xs")),
        Term::prim(Prim::NatType),
        None,
        Term::prim(Prim::NatType),
        nat_lit(0),
        &h,
        &t,
        &ih,
        Term::prim(Prim::nat_add(Term::free_var(&ih), nat_lit(1))),
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
~v0$/xs = LstBuild(1)
entry {
  ~v5 = fold-seq[lst] ~v0$/xs {
    empty => {
      return 0
    }
    step(~v1$h, ~v2$t, ~v3$ih) => {
      ~v4 = NatAdd(~v3$ih, 1)
      return ~v4
    }
  }
  return ~v5
}
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
        Term::prim(Prim::NatType),
        [
            ("none", Vec::<crate::Free>::new(), nat_lit(0)),
            ("some", vec![x.clone()], Term::free_var(&x)),
        ],
    );
    let erased = erase_module(
        &mut context,
        &Module {
            items: Vec::new(),
            universe_seeds: vec![],
            induct_decls,
            struct_decls: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            binder_floor: 0,
            type_: None,
            body,
        },
        &Term::prim(Prim::NatType),
    )
    .expect("the module erases");
    assert_eq!(
        erased.to_string(),
        "\
family ~d0$/Opt { ~t0$none() ~t1$some(x) }
entry {
  ~v0$scrutinee = construct ~t1(6)
  ~v2 = match ~d0 ~v0$scrutinee {
    ~t0() => {
      return 0
    }
    ~t1(~v1$x) => {
      return ~v1$x
    }
  }
  return ~v2
}
"
    );
}

#[test]
fn an_effectful_scrutinee_is_erased_once() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let pred = context.fresh(Some("pred"));
    let ih = context.fresh(Some("ih"));
    // The peel path re-derives Core terms from the head (`n - 1`); an
    // effectful compound head must still evaluate exactly once, through the
    // alias.
    let io_read = Term::apply(Term::free_var(&global("read")), [nat_lit(0)]);
    let items = vec![definition(
        "read",
        Term::func_type(
            [(x.clone(), Term::prim(Prim::NatType))],
            Term::prim(Prim::NatType),
        ),
        Term::func([(x.clone(), Term::type_ground())], Term::free_var(&x)),
    )];
    let body = Term::nat_match(
        io_read,
        None,
        Term::prim(Prim::NatType),
        nat_lit(0),
        &pred,
        &ih,
        Term::free_var(&pred),
    );
    let erased = erase(
        &mut context,
        &module(items, body),
        Term::prim(Prim::NatType),
    );
    let printed = erased.to_string();
    assert_eq!(
        printed.matches("apply ~f0$/read").count(),
        1,
        "the compound scrutinee is applied exactly once:\n{printed}"
    );
}

#[test]
fn a_recursive_function_group_erases_to_functions() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let f = context.fresh(Some("f"));
    // rec f(x) = f(x); body f(3)
    let func_type = Term::func_type(
        [(x.clone(), Term::prim(Prim::NatType))],
        Term::prim(Prim::NatType),
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
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
entry {
  functions ~f0$f
  ~v2 = apply ~f0$f(3)
  return ~v2
}
function ~f0$f(~v0$x) {
  ~v1 = apply ~f0$f(~v0$x)
  return ~v1
}
"
    );
}

#[test]
fn a_mixed_recursive_group_erases_to_a_rec_group() {
    let mut context = context();
    let u = context.fresh(Some("u"));
    let produce = context.fresh(Some("produce"));
    let consume = context.fresh(Some("consume"));
    // rec { produce() = consume; consume = produce } — a dormant knot: the
    // computed member's initializer references the function, and the function
    // body references the computed member (dormant until applied).
    let produce_type = Term::func_type(
        [(
            u.clone(),
            Term::tuple_type(Vec::<(crate::Free, Term)>::new()),
        )],
        Term::prim(Prim::NatType),
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
        erased.to_string(),
        "\
entry {
  rec ~r0 {
    functions ~f0$produce
    ~v0$consume = init {
      return ~f0$produce
    }
  }
  return ~v0$consume
}
function ~f0$produce(~v1$u) {
  return 5
}
"
    );
}

#[test]
fn a_computed_only_evaluation_cycle_is_rejected_as_an_error() {
    let mut context = context();
    let a = context.fresh(Some("a"));
    let b = context.fresh(Some("b"));
    // rec a = b; b = a — a mutual value-level cycle no initialization order
    // satisfies. The verifier rejects it; erasure surfaces the diagnostic as
    // an error, never a panic. (A *self*-knot `rec loop = loop` is admitted:
    // the lowering drops it when unused, mirroring the legacy path.)
    let type_ = Term::prim(Prim::NatType);
    let body = Term::rec(
        vec![
            (a.clone(), type_.clone(), Term::free_var(&b)),
            (b.clone(), type_.clone(), Term::free_var(&a)),
        ],
        Term::free_var(&a),
    );
    let error = erase_module(&mut context, &module(Vec::new(), body), &type_)
        .expect_err("the value-level cycle is rejected");
    assert!(
        matches!(error, Error::ErasedModuleInvalid { .. }),
        "{error:?}"
    );
}

#[test]
fn top_level_recursive_items_erase_through_the_item_chain() {
    let mut context = context();
    let x = context.fresh(Some("x"));
    let func_type = Term::func_type(
        [(x.clone(), Term::prim(Prim::NatType))],
        Term::prim(Prim::NatType),
    );
    let items = vec![Item::Rec(RecItem::new(vec![Definition {
        name: Global::Authored(Qualifier::from(["go"])),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        root: RootId::Entry,
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
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
functions ~f0$/go
entry {
  ~v2 = apply ~f0$/go(1)
  return ~v2
}
function ~f0$/go(~v0$x) {
  ~v1 = apply ~f0$/go(~v0$x)
  return ~v1
}
"
    );
}
