//! Resolving a witness through the table: a method called by its path or passed as a value, an explicit override, several parameters, a later declaration, the operator concepts, and how a witness is keyed.

use {
    crate::test_support::*,
    curios_common::Qualifier,
    curios_core::{Field, Global, Item, Proj, Subterm},
    curios_elab::DEFAULT_BUDGET,
    curios_module::RootSource,
    curios_pipeline::test_support::*,
    curios_surface::Entrypoint,
};

// The base case: a concept, a witness keyed on a rigid nominal head, and a call through the generated method wrapper. `Show/show(n)` saturates `@A` with a metavar and the `use` slot with a witness goal; solving `A := Nat` from `n` wakes the goal, which the global table resolves to the `Show(Nat)` witness.
#[test]
fn concept_witness_resolves_through_wrapper() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 42;
        /std/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// An explicit `use` argument overrides table resolution: a local dictionary value (an ordinary `let` of the concept's record type) is passed at the call site and used instead of the registered `Show(Nat)` witness.
#[test]
fn explicit_use_argument_overrides() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let parens : Show(Nat) =
            Show(Nat) { show = (n) => Str/concat("(", Str/concat(Nat/to_str(n), ")")) };
        let n : Nat = 7;
        /std/print(Show/show(@_, use parens, n))
        "#;

    assert_eq!(run(source), b"(7)");
}

// The prelude-provided `Show` concept and its witnesses resolve, proving the cached-prelude replay path registers concepts and witnesses.
#[test]
fn prelude_show_resolves() {
    let source = r#"
        use /std/{Nat, Show};
        let n : Nat = 42;
        /std/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// The prelude `Eql` concept resolves through the value-level witnesses.
#[test]
fn prelude_eql_resolves() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};
        let a : Nat = 5;
        let b : Nat = 5;
        /std/print(Bool/to_str(Eql/eql(a, b)))
        "#;

    assert_eq!(run(source), b"true");
}

// Multi-parameter concepts key on the tuple of every parameter head: two witnesses may share a first head as long as the full parameter tuple differs, and each resolves once both parameters are pinned.
#[test]
fn multi_param_witnesses_share_a_first_head() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Into(A : Type, B : Type) : pub Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        satisfy Into(Nat, Bool) {
            into(n) = Nat/eql(n, 1)
        }
        let s : Str = Into/into(2);
        let b : Bool = Into/into(2);
        /std/print(Bool/to_str(b))
        "#;

    assert_eq!(run(source), b"false");
}

// Every concept parameter participates in the witness key, so a goal whose second parameter is never pinned parks and surfaces as an error at the end of the module — no accidental inference from the witness. A witness stands where the parameter would key it, so the report names the parameter as the argument owed and closes on writing it.
#[test]
fn open_parameter_does_not_infer_from_the_witness() {
    let source = r#"
        use /std/{Nat, Str};
        pub concept Into(A : Type, B : Type) : pub Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        pub let discard(@A : Type, x : A) -> Nat = 0;
        /std/print(Nat/to_str(discard(Into/into(1))))
        "#;

    let message = error(source).to_lowercase();
    assert!(
        message.contains("uninferred argument")
            && message.contains(
                "the 2nd argument of the witness wanted, `into(nat, ?)`, was not inferred"
            )
            && message.contains("a witness stands for `into(nat, str)`")
            && message.contains("supply it explicitly, `into/into(@...)`"),
        "unexpected report:\n{message}"
    );
}

// A key an open argument holds, where no witness agrees with what is known of it, misses for the ordinary reason: no witness stands for `Bool`, whatever the second argument would be. Mutation-checked: naming the argument for any open key reports this one as an uninferred argument too.
#[test]
fn an_open_key_no_witness_agrees_with_reports_no_witness() {
    let source = r#"
        use /std/{Bool, Nat, Str};
        pub concept Into(A : Type, B : Type) : pub Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        pub let discard(@A : Type, x : A) -> Nat = 0;
        /std/print(Nat/to_str(discard(Into/into(true))))
        "#;

    let message = error(source).to_lowercase();
    assert!(
        message.contains("no witness") && !message.contains("uninferred argument"),
        "unexpected report:\n{message}"
    );
}

// The operator concepts: `Add/add` resolves on an intrinsic type through the `/std` witness (also proving the cached-prelude replay path registers the operator concepts and their `/std` witnesses), on a user struct through a user witness, and in generic code through a local `use Add(A)` premise.
#[test]
fn syn_add_concept_resolves_everywhere() {
    let source = r#"
        use /std/{Nat, Str};
        use /std/ops/{Add};
        struct Point : pub Type { x : Nat, y : Nat }
        satisfy Add(Point) {
            add(a, b) = Point { x = Nat/add(a.x, b.x), y = Nat/add(a.y, b.y) }
        }
        pub let double(@A : Type, use Add(A), v : A) -> A = Add/add(v, v);
        let p : Point = double(Point { x = 3, y = 4 });
        let n : Nat = Add/add(20, 1);
        /std/print(Nat/to_str(Nat/add(p.x, n)))
        "#;

    assert_eq!(run(source), b"27");
}

// `Eql` and `Ord` resolve across intrinsics with the witnesses homed beside each type — `Eql(Nat)`/`Ord(Nat)` in `/std/Nat`, `Eql(Str)` in `/std/Str`, `Ord(Int)` in `/std/Int` — rather than in the operator-concept facades, which keep only the concept re-exports.
#[test]
fn eql_and_ord_resolve_across_intrinsics() {
    let source = r#"
        use /std/{Nat, Int, Bool, Str};
        use /std/ops/{Eql, Ord};
        let a : Bool = Eql/eql(2, 2);
        let b : Bool = Eql/eql("abc", "abc");
        let c : Bool = Ord/lt(+1, +2);
        let d : Bool = Ord/le(3, 3);
        /std/print(Bool/to_str(Bool/and(Bool/and(a, b), Bool/and(c, d))))
        "#;

    assert_eq!(run(source), b"true");
}

// A witness declared *after* a value that uses it still resolves: the unit's witnesses are known by key before any of them elaborates, so the use-site goal finds the later `satisfy`, and the value is elaborated once the witness has been. This ordering freedom is what lets a `/std` witness live beside its type — a type module's own value functions may call an operator before the module's trailing witness block, the way `/std/Nat`'s `NonZero` uses `<` ahead of `Ord(Nat)`.
#[test]
fn forward_declared_witness_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Eqx(A : Type) : pub Type {
            eqx(A, A) -> Bool
        }
        pub let uses_eqx(a : Nat, b : Nat) -> Bool = Eqx/eqx(a, b);
        satisfy Eqx(Nat) {
            eqx(a, b) = Nat/eql(a, b)
        }
        /std/print(Bool/to_str(uses_eqx(3, 3)))
        "#;

    assert_eq!(run(source), b"true");
}

// A witness spelled through a name for its type registers where the type's own would: `Alias` is `Box`, so the witness is known under `Show(Box)` before it elaborates, and a goal written before it finds it.
#[test]
fn a_witness_spelled_through_a_name_for_its_type_is_keyed_at_the_type() {
    let source = r#"
        use /std/{Nat, Show, Str};
        pub struct Box: pub Type { value: Nat }
        let Alias: Type = Box;
        let shown: Str = Show/show(Box { value = 1 });
        satisfy Show(Alias) { show(b) = "boxed" }
        /std/print(shown)
        "#;

    assert_eq!(run(source), b"boxed");
}

// A goal's key is read off its parameter's weak-head form, and beneath a function's binders where the function's body names no former: one closed constructor is keyed alike whichever reduction spelled it, as a declaration's body and in an arm, and a body that reaches its former through a definition is keyed by the former. The witness below is the one each goal converts with.
#[test]
fn a_constructor_a_goal_computes_is_keyed_by_the_former_it_computes() {
    let source = r#"
        use /std/{Nat};
        induct Box(S : Type, A : Type) : Type | box(s : S, a : A) end
        concept Tag(F : (Type) -> Type) : Type {
            tag : Nat
        }
        let partial(F : (Type, Type) -> Type, S : Type) -> (Type) -> Type = (A) => F(S, A);
        let through(S : Type, A : Type) -> Type = Box(S, A);
        satisfy (@S : Type) => Tag((A : Type) => Box(S, A)) {
            tag = 1
        }
        let outside : Nat = Tag/tag(@partial(Box, Nat));
        let inside(n : Nat) -> Nat =
            match n
            | 0 => Tag/tag(@partial(Box, Nat))
            | k + 1 => k
            end;
        let defined : Nat = Tag/tag(@((A : Type) => through(Nat, A)));
        /std/print(Nat/to_str(outside + inside(0) + defined))
        "#;

    assert_eq!(run(source), b"3");
}

// A method named by its path is its concept's field: its call elaborates to the projection off the witness resolution finds, as an operator's does, and names no definition.
#[test]
fn a_method_called_by_its_path_is_the_projection_off_its_witness() {
    let source = r#"
        use /std/{Nat, Str, Show};
        let subject: Str = Show/show(5);
        /std/print(subject)
        "#;
    let entrypoint = source.parse::<Entrypoint>().expect("the fixture parses");
    let program = typecheck_with_prelude(DEFAULT_BUDGET, &entrypoint, &RootSource::none())
        .expect("the fixture type-checks")
        .program;
    let subject = program
        .definitions
        .items
        .iter()
        .flat_map(Item::definitions)
        .find(|definition| definition.name == Global::Authored(Qualifier::from(["subject"])))
        .expect("the fixture declares `subject`");

    let Subterm::Apply(call) = &*subject.body else {
        panic!("`subject` is a call: {:?}", subject.body);
    };
    let show = Global::Authored(Qualifier::from(["std", "Show", "Show"]));
    assert!(
        matches!(&*call.head, Subterm::Proj(Proj { field: Field::Struct(concept, 0), .. }) if *concept == show),
        "the call's head is the projection of `show`: {:?}",
        call.head
    );
}

// A method passed where a function is expected is its field of the witness the expected type resolves, read where it stands.
#[test]
fn a_method_passed_as_a_value_is_its_field_of_the_witness_its_use_resolves() {
    let source = r#"
        use /std/{Nat, Str, Show, List};
        let shown: List(Str) = List/map([1, 2, 3], Show/show);
        /std/print(Str/join(", ", shown))
        "#;

    assert_eq!(run(source), b"1, 2, 3");
}
