//! How terms erase: scalars in evaluation order, carriers in their shapes, items in module order, functions with a parameter a binder and no captures, recursive groups, and the same erasure every time.

use {
    crate::test_support::{
        bound, definition, erase, fresh, global, module, nat_lit, newtype_struct, nominal,
        struct_type_of, zonked,
    },
    crate::*,
    curios_abi::{ForeignFunction, WireReturn, WireSignature, WireType},
    curios_analysis::test_support::SYNTAX,
    curios_cert::{Globals, certify_module},
    curios_elab::DEFAULT_BUDGET,
    curios_utilities::{Plicity, Qualifier},
    std::sync::Arc,
};

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
