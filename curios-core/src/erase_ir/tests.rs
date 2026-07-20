use {
    crate::*,
    curios_base::Qualifier,
    std::{
        collections::{BTreeMap, BTreeSet},
        time::Duration,
    },
};

fn context() -> Context {
    Context::new(Duration::from_secs(1))
}

fn nat_lit(n: usize) -> Term {
    Term::prim(Prim::Nat(Nat::new(n)))
}

fn definition(name: &str, type_: Term, body: Term) -> Item {
    Item::Let(Definition {
        name: name.into(),
        island: Qualifier::empty(),
        root: curios_abi::RootId::Entry,
        type_,
        body,
    })
}

fn module(items: Vec<Item>, body: Term) -> Module {
    Module {
        items,
        inductives: BTreeMap::new(),
        structures: BTreeMap::new(),
        concepts: BTreeMap::new(),
        witnesses: BTreeSet::new(),
        type_: None,
        body,
    }
}

fn erase(module: &Module, expected: Term) -> curios_ersd::ErasedModule {
    erase_module_to_ir(&mut context(), module, &expected).expect("the module erases")
}

#[test]
fn a_scalar_expression_erases_in_evaluation_order() {
    // let x = 2; x + 3
    let body = Term::let_(
        "x",
        Term::prim(Prim::NatType),
        nat_lit(2),
        Term::prim(Prim::nat_add(Term::free_var("x"), nat_lit(3))),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::NatType));
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
fn bln_and_byte_keep_their_shapes() {
    // Bln stays Bln-shaped and Byte stays Byte-shaped: no Nat carrier appears
    // anywhere in the erased output.
    let body = Term::let_(
        "b",
        Term::prim(Prim::BlnType),
        Term::prim(Prim::BlnAnd(
            Term::prim(Prim::Bln(true)),
            Term::prim(Prim::Bln(false)),
        )),
        Term::prim(Prim::ByteEql(
            Term::prim(Prim::Byte(7)),
            Term::prim(Prim::Byte(8)),
        )),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::BlnType));
    assert_eq!(
        erased.to_string(),
        "\
entry {
  ~v0$b = BlnAnd(true, false)
  ~v1 = ByteEql(7:byte, 8:byte)
  return ~v1
}
"
    );
}

#[test]
fn a_nat_spine_over_a_variable_erases_to_one_addition() {
    let items = vec![definition("x", Term::prim(Prim::NatType), nat_lit(5))];
    let body = Term::prim(Prim::Nat(Nat::Succ(3u32.into(), Term::free_var("x"))));
    let erased = erase(&module(items, body), Term::prim(Prim::NatType));
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
    // `a` references `b`, which is declared after it; the item chain must
    // reorder so every reference is backward.
    let items = vec![
        definition(
            "a",
            Term::prim(Prim::NatType),
            Term::prim(Prim::nat_add(Term::free_var("b"), nat_lit(1))),
        ),
        definition("b", Term::prim(Prim::NatType), nat_lit(2)),
    ];
    let erased = erase(
        &module(items, Term::free_var("a")),
        Term::prim(Prim::NatType),
    );
    assert_eq!(
        erased.to_string(),
        "\
~v0$a = NatAdd(2, 1)
entry {
  return ~v0$a
}
"
    );
}

#[test]
fn an_exit_seals_the_block_and_drops_dead_code() {
    // let _ = Io/exit(3); 7 — the trailing computation is dead.
    let body = Term::let_(
        "dead",
        Term::prim(Prim::NatType),
        Term::prim(Prim::IoExit(Term::prim(Prim::NatType), nat_lit(3))),
        nat_lit(7),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::NatType));
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
    let body = Term::let_(
        "lst",
        Term::prim(Prim::LstType(Term::prim(Prim::NatType))),
        Term::prim(Prim::Lst(vec![nat_lit(1), nat_lit(2)])),
        Term::prim(Prim::LstLen(
            Term::prim(Prim::NatType),
            Term::free_var("lst"),
        )),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::NatType));
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
    let build = || {
        let body = Term::let_(
            "x",
            Term::prim(Prim::NatType),
            nat_lit(2),
            Term::prim(Prim::nat_add(Term::free_var("x"), nat_lit(3))),
        );
        erase(&module(Vec::new(), body), Term::prim(Prim::NatType)).to_string()
    };
    assert_eq!(build(), build());
}

#[test]
fn a_function_erases_with_dropped_type_params_and_no_captures() {
    // (A : Type, x : A) => x — the type parameter is dropped; the runtime
    // function takes one parameter and stores no captures.
    let func_type = Term::func_type(
        [("A", Term::type_()), ("x", Term::free_var("A"))],
        Term::free_var("A"),
    );
    let items = vec![definition(
        "id",
        func_type,
        Term::func(
            [("A", Term::type_()), ("x", Term::type_())],
            Term::free_var("x"),
        ),
    )];
    let body = Term::apply(
        Term::free_var("id"),
        [Term::prim(Prim::NatType), nat_lit(4)],
    );
    let erased = erase(&module(items, body), Term::prim(Prim::NatType));
    assert_eq!(
        erased.to_string(),
        "\
functions ~f0$id
entry {
  ~v1 = apply ~f0$id(4)
  return ~v1
}
function ~f0$id(~v0$x) {
  return ~v0$x
}
"
    );
}

#[test]
fn a_capturing_closure_stores_no_capture_list() {
    // (y : Nat) => (x : Nat) => x + y — the inner closure references the
    // outer parameter freely; analysis derives it, nothing is stored.
    let inner_type = Term::func_type(
        [("x", Term::prim(Prim::NatType))],
        Term::prim(Prim::NatType),
    );
    let outer_type = Term::func_type([("y", Term::prim(Prim::NatType))], inner_type);
    let items = vec![definition(
        "make",
        outer_type,
        Term::func(
            [("y", Term::type_())],
            Term::func(
                [("x", Term::type_())],
                Term::prim(Prim::nat_add(Term::free_var("x"), Term::free_var("y"))),
            ),
        ),
    )];
    let expected = Term::func_type(
        [("y", Term::prim(Prim::NatType))],
        Term::func_type(
            [("x", Term::prim(Prim::NatType))],
            Term::prim(Prim::NatType),
        ),
    );
    let erased = erase(&module(items, Term::free_var("make")), expected);

    let printed = erased.to_string();
    assert!(printed.contains("function ~f0$make(~v0$y)"), "{printed}");
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
    Term::inductive_type("Opt", Vec::<Term>::new(), Vec::<Term>::new())
}

// induct Opt : Type | none() | some(x : Nat) end — `none` is tag 0, `some`
// tag 1 (registry-sorted). Registered on the module so erasure seeds it.
fn opt_inductive() -> Inductive {
    Inductive {
        params: Telescope::done(()),
        indices: Telescope::done(()),
        constructors: BTreeMap::from([
            (
                Atom::from("none"),
                InductiveParam {
                    telescope: Telescope::done(opt_type()),
                },
            ),
            (
                Atom::from("some"),
                InductiveParam {
                    telescope: Telescope::build([("x", Term::prim(Prim::NatType))], opt_type()),
                },
            ),
        ]),
        result_sort: Term::type_(),
        module: Qualifier::empty(),
        root: curios_abi::RootId::Entry,
        rep_public: true,
    }
}

#[test]
fn a_variant_constructs_with_its_registered_schema() {
    let mut inductives = BTreeMap::new();
    inductives.insert("Opt".to_string(), opt_inductive());
    let body = Term::variant("Opt", Vec::<Term>::new(), Atom::from("some"), [nat_lit(6)]);
    let erased = erase_module_to_ir(
        &mut context(),
        &Module {
            items: Vec::new(),
            inductives,
            structures: BTreeMap::new(),
            concepts: BTreeMap::new(),
            witnesses: BTreeSet::new(),
            type_: None,
            body,
        },
        &opt_type(),
    )
    .expect("the module erases");
    assert_eq!(
        erased.to_string(),
        "\
family ~d0$Opt { ~t0$none() ~t1$some(x) }
entry {
  ~v0 = construct ~t1(6)
  return ~v0
}
"
    );
}

#[test]
fn a_multi_field_tuple_shares_the_width_schema() {
    let tuple_type = Term::tuple_type([
        ("a", Term::prim(Prim::NatType)),
        ("b", Term::prim(Prim::NatType)),
    ]);
    let body = Term::let_(
        "pair",
        tuple_type.clone(),
        Term::tuple([nat_lit(1), nat_lit(2)]),
        Term::proj(Term::free_var("pair"), 1),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::NatType));
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
    // { x : Nat, w : Prop-valued } erases to the bare Nat; its projection
    // vanishes.
    let subset_type = Term::tuple_type([("x", Term::prim(Prim::NatType)), ("w", Term::prop())]);
    let body = Term::let_(
        "sub",
        subset_type.clone(),
        Term::tuple([nat_lit(9), Term::prop()]),
        Term::proj(Term::free_var("sub"), 0),
    );
    let erased = erase(&module(Vec::new(), body), Term::prim(Prim::NatType));
    assert_eq!(
        erased.to_string(),
        "\
entry {
  return 9
}
"
    );
}
