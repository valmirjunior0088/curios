//! What the ANF-Curios rendering promises: exact layout for representative modules, the header's reachability, the elision rule's two holes (`_` binders and undeclared rows), and the faithfulness boundary it stops at.

use {
    crate::*,
    curios_abi::{DeclaredForeign, ForeignFunction, WireResults, WireSignature, WireType},
    curios_num::{Floating, Natural},
    std::sync::Arc,
};

fn nat(value: u32) -> Constant {
    Constant::Nat(Natural::from(value))
}

fn doubling_module() -> Module {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(nat(1));
    let bound = builder.item_value(Some("one".into()), Rhs::Alias(Atom::Constant(one)));
    builder.open_block();
    let doubled = builder.let_value(
        Some("doubled".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Value(bound), Atom::Value(bound)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(doubled)));
    builder.set_entry(entry);
    builder.finalize().expect("the module verifies")
}

#[test]
fn a_representative_module_prints_exactly() {
    assert_eq!(
        doubling_module().to_string(),
        "\
let ~v0$one =
    1;
entry =
    let ~v1$doubled = Nat/add(~v0$one, ~v0$one);
    ~v1$doubled;
"
    );
}

#[test]
fn a_recursive_module_prints_exactly() {
    let mut builder = ErsdBuilder::new();
    let function = builder.reserve_function();
    let n = builder.value(Some("n".into()));
    let zero = builder.constant(nat(0));
    let one = builder.constant(nat(1));
    builder.open_block();
    let zero_case = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.open_block();
    let predecessor = builder.let_value(
        Some("predecessor".into()),
        Rhs::Operation {
            operation: Operation::NatSub,
            operands: vec![Atom::Value(n), Atom::Constant(one)],
        },
    );
    let recur = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Value(predecessor)],
        },
    );
    let default = builder.seal_block(Terminator::Return(Atom::Value(recur)));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchNat {
            scrutinee: Atom::Value(n),
            cases: vec![NatCase {
                key: 0,
                block: zero_case,
            }],
            default,
        },
    );
    let body = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.define_function(function, Some("loop".into()), vec![n], body);
    builder.item_functions(vec![function]);
    builder.open_block();
    let ten = builder.constant(nat(10));
    let run = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Function(function),
            arguments: vec![Atom::Constant(ten)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(run)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
let ~f0$loop(~v0$n) =
    let ~v3 =
        match ~v0$n
        | 0 => 0
        | _ =>
            let ~v1$predecessor = Nat/sub(~v0$n, 1);
            let ~v2 = ~f0$loop(~v1$predecessor);
            ~v2
        end;
    ~v3;
entry =
    let ~v4 = ~f0$loop(10);
    ~v4;
"
    );
}

#[test]
fn schemas_and_constants_print_deterministically() {
    let mut builder = ErsdBuilder::new();
    let schema = builder.product(ProductSchema {
        debug_name: Some("Pair".into()),
        fields: vec![Field::opaque(Some("lhs".into())), Field::opaque(None)],
        shared: false,
    });
    let family = builder.family(Some("Shape".into()));
    let circle = builder.constructor(
        family,
        Some("circle".into()),
        vec![Field::opaque(Some("radius".into()))],
    );
    builder.open_block();
    let negative_zero = builder.constant(Constant::Flt(Floating::from(-0.0)));
    let byte = builder.constant(Constant::Byte(7));
    let pair = builder.let_value(
        None,
        Rhs::Product {
            schema,
            fields: vec![Atom::Constant(negative_zero), Atom::Constant(byte)],
        },
    );
    let shape = builder.let_value(
        None,
        Rhs::Construct {
            constructor: circle,
            fields: vec![Atom::Value(pair)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(shape)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
product ~p0$Pair(lhs, 1)
family ~d0$Shape { ~t0$circle(radius) }
entry =
    let ~v0 = ~p0$Pair { lhs = -0.0:flt, 1 = 7:byte };
    let ~v1 = ~t0$circle(~v0);
    ~v1;
"
    );
}

#[test]
fn printing_is_deterministic_across_constructions() {
    assert_eq!(doubling_module().to_string(), doubling_module().to_string());
}

/// Printing a deep module costs heap, not native stack.
#[test]
fn a_deep_module_prints_without_native_stack() {
    let mut builder = ErsdBuilder::new();
    let zero = builder.constant(nat(0));
    let scrutinee = builder.item_value(Some("scrutinee".into()), Rhs::Alias(Atom::Constant(zero)));
    builder.open_block();
    let mut chain = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
    for _ in 0..50_000 {
        builder.open_block();
        let leaf = builder.seal_block(Terminator::Return(Atom::Value(scrutinee)));
        builder.open_block();
        let switched = builder.let_value(
            None,
            Rhs::SwitchNat {
                scrutinee: Atom::Value(scrutinee),
                cases: vec![NatCase {
                    key: 0,
                    block: leaf,
                }],
                default: chain,
            },
        );
        chain = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    }
    builder.set_entry(chain);
    let module = builder.finalize().expect("the deep module verifies");

    let printed = module.to_string();
    assert!(printed.starts_with("let ~v0$scrutinee =\n    0;\nentry =\n"));
    assert!(printed.ends_with(";\n"));
}

/// The defect the rewrite exists to fix: a lifted function used to print at top level with an empty parameter list and a body naming its enclosing function's parameter, which is an open term. Nesting at the binding site is what closes it.
#[test]
fn a_closure_nests_inside_the_function_that_binds_its_captures() {
    let mut builder = ErsdBuilder::new();
    let outer = builder.reserve_function();
    let inner = builder.reserve_function();
    let m = builder.value(Some("m".into()));

    builder.open_block();
    let forced = builder.let_value(
        None,
        Rhs::Apply {
            callee: Atom::Value(m),
            arguments: Vec::new(),
        },
    );
    let inner_body = builder.seal_block(Terminator::Return(Atom::Value(forced)));
    builder.define_function(inner, Some("inner".into()), Vec::new(), inner_body);

    builder.open_block();
    builder.let_functions(vec![inner]);
    let outer_body = builder.seal_block(Terminator::Return(Atom::Function(inner)));
    builder.define_function(outer, Some("outer".into()), vec![m], outer_body);
    builder.item_functions(vec![outer]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(outer)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
let ~f0$outer(~v0$m) =
    let ~f1$inner() =
        let ~v1 = ~v0$m();
        ~v1;
    ~f1$inner;
entry =
    ~f0$outer;
"
    );
}

/// A group binds its members in one `let … and …`, exactly as the surface declares mutually recursive definitions.
#[test]
fn a_function_group_prints_as_one_and_chain() {
    let mut builder = ErsdBuilder::new();
    let first = builder.reserve_function();
    let second = builder.reserve_function();
    let zero = builder.constant(nat(0));

    builder.open_block();
    let first_body = builder.seal_block(Terminator::Return(Atom::Function(second)));
    builder.define_function(first, Some("ping".into()), Vec::new(), first_body);
    builder.open_block();
    let second_body = builder.seal_block(Terminator::Return(Atom::Constant(zero)));
    builder.define_function(second, Some("pong".into()), Vec::new(), second_body);
    builder.item_functions(vec![first, second]);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Function(first)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
let ~f0$ping() =
    ~f1$pong
and ~f1$pong() =
    0;
entry =
    ~f0$ping;
"
    );
}

/// A `Rec` group mixes functions with values forced on first read. It spells `let rec` where a plain `Functions` group spells `let`, so the two never collapse into one reading.
#[test]
fn a_recursive_group_prints_as_a_let_rec_chain() {
    let mut builder = ErsdBuilder::new();
    let step = builder.reserve_function();
    let table = builder.value(Some("table".into()));
    let zero = builder.constant(nat(0));

    builder.open_block();
    let step_body = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.define_function(step, Some("step".into()), Vec::new(), step_body);

    builder.open_block();
    let init = builder.seal_block(Terminator::Return(Atom::Constant(zero)));

    let group = builder.rec_group(vec![step], vec![(table, init)]);
    builder.item_rec(group);

    builder.open_block();
    let entry = builder.seal_block(Terminator::Return(Atom::Value(table)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
let rec ~f0$step() =
    ~v0$table
and ~v0$table =
    0;
entry =
    ~v0$table;
"
    );
}

/// The surface writes a `Bool` match true-arm first; the representation stores false first, and the printer reorders rather than exposing the field order.
#[test]
fn a_bool_match_prints_its_true_arm_first_with_statement_free_arms_riding() {
    let mut builder = ErsdBuilder::new();
    let flag = builder.constant(Constant::Bool(true));
    let yes = builder.constant(nat(1));
    let no = builder.constant(nat(2));
    builder.open_block();
    let if_false = builder.seal_block(Terminator::Return(Atom::Constant(no)));
    builder.open_block();
    let if_true = builder.seal_block(Terminator::Return(Atom::Constant(yes)));
    builder.open_block();
    let switched = builder.let_value(
        None,
        Rhs::SwitchBool {
            scrutinee: Atom::Constant(flag),
            if_false,
            if_true,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(switched)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
entry =
    let ~v0 =
        match true
        | true => 1
        | false => 2
        end;
    ~v0;
"
    );
}

/// `_` is what a hintless value nothing reads *is*, following `curios-core`'s rule for an unreferenced unnameable binder. A hinted one keeps its hint, because the hint is information the elaborator recorded.
#[test]
fn an_unread_binder_spells_a_hole_only_when_it_is_also_hintless() {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(nat(1));
    let two = builder.constant(nat(2));
    builder.open_block();
    let _hintless = builder.let_value(None, Rhs::Alias(Atom::Constant(one)));
    let _hinted = builder.let_value(Some("named".into()), Rhs::Alias(Atom::Constant(one)));
    let entry = builder.seal_block(Terminator::Return(Atom::Constant(two)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
entry =
    let _ = 1;
    let ~v1$named = 1;
    2;
"
    );
}

/// The faithfulness boundary: a binding read exactly once still prints as a binding. Collapsing it would read better and would erase a `ValueId` the module holds.
#[test]
fn a_single_use_binding_is_not_inlined() {
    let mut builder = ErsdBuilder::new();
    let one = builder.constant(nat(1));
    builder.open_block();
    let bound = builder.let_value(
        Some("once".into()),
        Rhs::Operation {
            operation: Operation::NatAdd,
            operands: vec![Atom::Constant(one), Atom::Constant(one)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(bound)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
entry =
    let ~v0$once = Nat/add(1, 1);
    ~v0$once;
"
    );
}

/// The header declares what the program refers to and nothing else — a dense schema arena is never pruned, so without this a five-line program carries the whole standard library's rows.
#[test]
fn an_unreached_schema_or_foreign_row_is_not_declared() {
    let mut builder = ErsdBuilder::new();
    let used = builder.product(ProductSchema {
        debug_name: Some("Used".into()),
        fields: vec![Field::opaque(Some("only".into()))],
        shared: false,
    });
    let _unused = builder.product(ProductSchema {
        debug_name: Some("Unused".into()),
        fields: vec![Field::opaque(Some("gone".into()))],
        shared: false,
    });
    let _unused_family = builder.family(Some("Unreached".into()));
    let _unused_foreign = builder.foreign(Arc::new(ForeignFunction::Declared(DeclaredForeign {
        name: "/beep".into(),
        label: "beep".into(),
        signature: WireSignature {
            params: vec![],
            results: WireResults::single("r".into(), WireType::Nat),
        },
    })));

    let one = builder.constant(nat(1));
    builder.open_block();
    let built = builder.let_value(
        None,
        Rhs::Product {
            schema: used,
            fields: vec![Atom::Constant(one)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(built)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    assert_eq!(
        module.to_string(),
        "\
product ~p0$Used(only)
entry =
    let ~v0 = ~p0$Used { only = 1 };
    ~v0;
"
    );
}

/// Reachability is a fixpoint: a reached row's field shapes name further rows, and a header that declared a shape in terms of an identity it never introduced would be unreadable.
#[test]
fn a_reached_row_pulls_in_the_rows_its_field_shapes_name() {
    let mut builder = ErsdBuilder::new();
    let innermost = builder.product(ProductSchema {
        debug_name: Some("Innermost".into()),
        fields: vec![Field::immediate(Some("leaf".into()))],
        shared: false,
    });
    let middle = builder.product(ProductSchema {
        debug_name: Some("Middle".into()),
        fields: vec![Field {
            debug_name: Some("nested".into()),
            shape: FieldShape::Product(innermost),
        }],
        shared: false,
    });
    let outer = builder.product(ProductSchema {
        debug_name: Some("Outer".into()),
        fields: vec![Field {
            debug_name: Some("held".into()),
            shape: FieldShape::Product(middle),
        }],
        shared: false,
    });

    let one = builder.constant(nat(1));
    builder.open_block();
    let built = builder.let_value(
        None,
        Rhs::Product {
            schema: outer,
            fields: vec![Atom::Constant(one)],
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(built)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    let printed = module.to_string();
    assert!(printed.contains("product ~p0$Innermost(leaf:immediate)"));
    assert!(printed.contains("product ~p1$Middle(nested:product/~p0)"));
    assert!(printed.contains("product ~p2$Outer(held:product/~p1)"));
}

/// A field name two reached rows share cannot name a field on its own, so a projection through it takes its schema — `build_shorten`'s shortest-unambiguous rule, applied to fields.
#[test]
fn a_projection_qualifies_a_field_name_two_reached_rows_share() {
    let mut builder = ErsdBuilder::new();
    let left = builder.product(ProductSchema {
        debug_name: Some("Left".into()),
        fields: vec![Field::opaque(Some("shared".into()))],
        shared: false,
    });
    let right = builder.product(ProductSchema {
        debug_name: Some("Right".into()),
        fields: vec![Field::opaque(Some("shared".into()))],
        shared: false,
    });

    let one = builder.constant(nat(1));
    builder.open_block();
    let built = builder.let_value(
        None,
        Rhs::Product {
            schema: left,
            fields: vec![Atom::Constant(one)],
        },
    );
    let other = builder.let_value(
        None,
        Rhs::Product {
            schema: right,
            fields: vec![Atom::Constant(one)],
        },
    );
    let _read = builder.let_value(
        None,
        Rhs::Project {
            schema: left,
            product: Atom::Value(built),
            field: 0,
        },
    );
    let also = builder.let_value(
        None,
        Rhs::Project {
            schema: right,
            product: Atom::Value(other),
            field: 0,
        },
    );
    let entry = builder.seal_block(Terminator::Return(Atom::Value(also)));
    builder.set_entry(entry);
    let module = builder.finalize().expect("the module verifies");

    // The literal keeps the bare name — it names its schema in front of the row — and only the projections qualify.
    assert_eq!(
        module.to_string(),
        "\
product ~p0$Left(shared)
product ~p1$Right(shared)
entry =
    let ~v0 = ~p0$Left { shared = 1 };
    let ~v1 = ~p1$Right { shared = 1 };
    let _ = ~v0.~p0/shared;
    let ~v3 = ~v1.~p1/shared;
    ~v3;
"
    );
}
