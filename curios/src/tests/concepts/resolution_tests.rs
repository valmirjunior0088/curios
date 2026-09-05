//! Resolving a witness: through a method wrapper, a premise, a superclass, a higher-kinded argument, or imitation against a partially applied family.

use crate::tests::{error, run};

// The base case: a concept, a witness keyed on a rigid nominal head, and a call through the generated method wrapper. `Show/show(n)` saturates `@A` with a metavar and the `use` slot with a witness goal; solving `A := Nat` from `n` wakes the goal, which the global table resolves to `show_nat`.
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

// A premised witness: `show_arr` needs a `Show(A)` to show its elements. The resolver instantiates its telescope — `@A := ?B` with premise goal `Show(?B)` — unifies `Show(List(?B)) ≡ Show(List(Nat))` to solve `?B := Nat`, then resolves the premise to `show_nat`.
#[test]
fn premised_witness_resolves_recursively() {
    let source = r#"
        use /std/{Nat, Str, List};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy (@A : Type, use Show(A)) => Show(List(A)) {
            show(l) =
                List/fold(l, "[", (x, acc) => Str/concat(acc, Show/show(x)))
        }
        let l : List(Nat) = [1, 2, 3];
        /std/print(Show/show(l))
        "#;

    assert_eq!(run(source), b"[123");
}

// An explicit `use` argument overrides table resolution: a local dictionary value (an ordinary `let` of the concept's record type) is passed at the call site and used instead of the registered `show_nat`.
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
        /std/print(Show/show(use parens, n))
        "#;

    assert_eq!(run(source), b"(7)");
}

// A superclass edge resolved by projection: inside `same`, the goal `Equal(A)` has a bound-variable head (no table entry), so it is solved by projecting the local `use Ordered(A)` binder's (anonymous) superclass field, keyed by index. The `use Ordered(A)` slot itself resolves through the table to `ord_nat`, whose own omitted superclass field resolves to `eql_nat` — no field names a witness anywhere.
#[test]
fn superclass_projection_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Ordering};
        pub concept Equal(A : Type) : pub Type {
            eql(A, A) -> Bool
        }
        pub concept Ordered(A : Type) : pub Type {
            use Equal(A),
            cmp(A, A) -> Ordering
        }
        satisfy Equal(Nat) {
            eql(a, b) = a == b
        }
        satisfy Ordered(Nat) {
            cmp(a, b) = Ordering/lt()
        }
        pub let same(@A : Type, use Ordered(A), x : A, y : A) -> Bool = Equal/eql(x, y);
        let n : Nat = 3;
        /std/print(Bool/to_str(same(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// No witness registered for the goal's head: a resolution-time error.
#[test]
fn missing_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let b : Bool = true;
        /std/print(Show/show(b))
        "#;

    assert!(error(source).contains("witness"));
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

// The prelude `Equal` concept resolves through the value-level witnesses.
#[test]
fn prelude_eql_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Equal};
        let a : Nat = 5;
        let b : Nat = 5;
        /std/print(Bool/to_str(Equal/eql(a, b)))
        "#;

    assert_eq!(run(source), b"true");
}

// The prelude `Ordered` concept resolves, and its `Equal` superclass is reachable by projection from an `Ordered` in scope.
#[test]
fn prelude_ord_superclass_projects() {
    let source = r#"
        use /std/{Nat, Bool, Ordered, Equal};
        pub let equal(@A : Type, use Ordered(A), x : A, y : A) -> Bool = Equal/eql(x, y);
        let n : Nat = 4;
        /std/print(Bool/to_str(equal(n, n)))
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

// Every concept parameter participates in the witness key, so a goal whose second parameter is never pinned parks and surfaces as an error at the end of the module — no accidental inference from the witness.
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
    assert!(message.contains("witness") || message.contains("infer"));
}

// The full higher-kinded chain: `Monad/bind(o, f)` parks its `Monad(?M)` goal on the flex parameter, checking `o : Option(Nat)` against `?M(?A)` fires the flex-apply imitation rule inside the conversion checker, and the committed `?M := Option` wakes the parked goal, which the table resolves to the prelude's `Monad(Option)` witness. Also covers the cached-prelude replay of a higher-kinded witness.
#[test]
fn prelude_monad_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
        let o : Option(Nat) = Monad/bind(Option/some(20), (x) => Monad/pure(Nat/add(x, 1)));
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"21");
}

// A *written* higher-kinded instantiation: `@Option` fills `M` at the call, so the `use Monad(M)` goal is minted from the telescope opened at the written argument. That argument must enter the telescope rebuilt (an `Instance` at its fresh levels): substituted raw, the bare polymorphic `Option` reference is inert under the reducer's monomorphic-variable gate, resolution's global-table step finds no rigid head to key on, and the registered witness is missed while the inferred path (`lift(7)` against an expected `Option(Nat)`) resolves fine.
#[test]
fn written_higher_kinded_argument_resolves_the_witness() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Str, Option};
        pub let lift(@M : (Type) -> Type, use Monad(M), seed : Nat) -> M(Nat) =
            Monad/pure(seed);
        let o : Option(Nat) = lift(@Option, 7);
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"7");
}

// A written hidden argument *behind an explicit slot*: the materialization walk substitutes the explicit `7` raw, so the trailing `use` binder's goal cannot be typed there — it is minted in the checking walk, where its domain is opened through the elaborated `@Option` rather than the raw spelling.
#[test]
fn written_hidden_argument_after_an_explicit_slot_resolves() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Str, Option};
        pub let lift2(seed : Nat, @M : (Type) -> Type, use Monad(M)) -> M(Nat) =
            Monad/pure(seed);
        let o : Option(Nat) = lift2(7, @Option);
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"7");
}

// A written type-level *lambda* as the carrier: an intro form keeps its postponement path, so the witness goal must be minted only after the lambda's own elaboration turns its body into the nominal normal form the head key can read (`Box`) — the raw application spelling inside the unelaborated lambda keys on nothing.
#[test]
fn written_type_lambda_argument_resolves_the_witness() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Str, Result};
        struct Box(A : Type) : pub Type {
            A
        }
        satisfy Monad(Box) {
            pure(x) = Box { x },
            bind(m, f) = f(m.0)
        }
        pub let lift(@M : (Type) -> Type, use Monad(M), seed : Nat) -> M(Nat) =
            Monad/pure(seed);
        let b : Box(Nat) = lift(@((A : Type) => Box(A)), 3);
        /std/print(Nat/to_str(b.0))
        "#;

    assert_eq!(run(source), b"3");
}

// A bare reference to an all-hidden generic function, checked against a rigid concrete carrier: the check turnaround inserts the implicit carrier and the witness goal, imitation pins `M := Option` from the expectation, and the goal resolves through the table.
#[test]
fn bare_generic_reference_resolves_toward_a_rigid_expectation() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Str, Option};
        pub let mk(@M : (Type) -> Type, use Monad(M)) -> M(Nat) =
            Monad/pure(5);
        let z : Option(Nat) = mk;
        /std/print(Nat/to_str(Option/unwrap_or(z, 0)))
        "#;

    assert_eq!(run(source), b"5");
}

// A higher-kinded superclass: inside the generic function the goal `Monad(M)` (M a bound variable) resolves through step 2's superclass projection of the local `use MonadPlus(M)` binder. The witness's own omitted `monad` field resolves through the table to the std `Monad(Option)` witness — a higher-kinded auto-fill.
#[test]
fn higher_kinded_superclass_projects() {
    let source = r#"
        use /std/{Nat, Str, Option, Monad};
        pub concept MonadPlus(M : (Type) -> Type) : Type {
            use Monad(M),
            empty(@A : Type) -> M(A)
        }
        satisfy MonadPlus(Option) {
            empty(@A) = Option/none()
        }
        pub let wrap(@M : (Type) -> Type, use MonadPlus(M), m : M(Nat)) -> M(Nat) =
            Monad/bind(m, (x) => Monad/pure(x));
        let o : Option(Nat) = wrap(Option/some(11));
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"11");
}

// `Intrinsic`-headed type constructors (`List`, `Cell`) carry their argument inside the `Intrinsic` node; the imitation rule rebuilds the node over the binder (`?M := λT. List(T)`), so `Monad/bind` over a `List` pins the witness from the action's type like any nominal constructor would.
#[test]
fn monad_over_intrinsic_constructor_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, List, Str, Monad};
        let a : List(Nat) = [1];
        let b : List(Nat) = Monad/bind(a, (x) => a);
        /std/print(Nat/to_str(List/len(b)))
        "#;

    assert_eq!(run(source), b"1");
}

// The syn-homed operator concepts: `Add/add` resolves on an intrinsic type through the `/std` witness (also proving the cached-prelude replay path registers the syn concepts and std witnesses), on a user struct through a user witness, and in generic code through a local `use Add(A)` premise.
#[test]
fn syn_add_concept_resolves_everywhere() {
    let source = r#"
        use /std/{Nat, Str, Add};
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

// `Equal` and `Compare` resolve across intrinsics with the witnesses now homed beside each type — `Equal(Nat)`/`Compare(Nat)` in `/std/Nat`, `Equal(Str)` in `/std/Str`, `Compare(Flt)` in `/std/Flt` — rather than in the operator-concept facades, which keep only the concept re-exports.
#[test]
fn eql_and_cmp_resolve_across_intrinsics() {
    let source = r#"
        use /std/{Nat, Flt, Bool, Str, Equal, Compare};
        let a : Bool = Equal/eql(2, 2);
        let b : Bool = Equal/eql("abc", "abc");
        let c : Bool = Compare/lt(1.0, 2.0);
        let d : Bool = Compare/ge(3, 3);
        /std/print(Bool/to_str(Bool/and(Bool/and(a, b), Bool/and(c, d))))
        "#;

    assert_eq!(run(source), b"true");
}

// A witness declared *after* a value that uses it still resolves: the use-site goal defers on the missing table entry, the later `satisfy` registers it, and the end-of-module sweep discharges the deferred goal. This ordering freedom is what lets a `/std` witness live beside its type — a type module's own value functions may call an operator before the module's trailing witness block, the way `/std/Nat`'s `min`/`cmp` use `<`/`==` ahead of `Compare(Nat)`.
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

// A missing operator witness used in an inductive's constructor *index type* once surfaced as a bare `?m ≡ ?n` metavariable mismatch: the constructor is elaborated twice, and reconciling the two elaborations parks a conversion between their (unsolvable) witness holes. It is now reported as the unresolved witness it is, naming the concept, the key, and the `+` that needed it.
#[test]
fn missing_witness_in_constructor_index_names_the_concept() {
    let source = r#"
        use /std/{Nat, Add};
        pub struct Wrap : pub Type { n : Nat }
        pub induct Foo : (w : Wrap) -> pub Type
        | mk(@w : Wrap, prev : Foo(w)) : (w + w)
        end
        /std/print("no")
        "#;

    let message = error(source);
    assert!(message.contains("witness"), "got: {message}");
    assert!(message.contains("Add(Wrap)"), "got: {message}");
}

// A `Type`-sorted field and a `Type`-returning method both spell `Type` in the field type's result spine. The record pass lowered that span under `input_type`'s lexical `Generalizable` while the method-wrapper re-lowering met it in output position at the default `Flexible` — one shared universe seed, two roles, and the lowerer's seed assert panicked. The wrapper signature now lowers under the record's role, so the associated type registers, resolves through the table, and its projection unfolds definitionally: `v : alias` checks `3` against `Nat`, and `b : picked` checks `true` against `Bool`.
#[test]
fn a_concept_field_may_carry_a_type() {
    let source = r#"
        use /std/{Nat, Bool, Str};
        pub concept Sized(A : Type) : pub Type {
            Carrier : Type,
            pick() -> Type,
        }
        satisfy Sized(Nat) {
            Carrier = Nat,
            pick() = Bool,
        }
        let alias : Type = Sized/Carrier(@Nat);
        let v : alias = 3;
        let picked : Type = Sized/pick(@Nat)();
        let b : picked = true;
        let _ = /std/print(Nat/to_str(v))!;
        match b
        | true => /std/print("t")
        | false => /std/print("f")
        end
        "#;

    assert_eq!(run(source), b"3t");
}

// The higher-kinded twin exercises the same wrapper machinery with the dictionary supplied explicitly. The explicit `use` is deliberate: table resolution for an explicitly written type-former argument (`@Option` as a global reference rather than an imitation-solved metavariable) is a separate, still-open gap, so this pins the wrapper, the projection, and the definitional unfolding without depending on it.
#[test]
fn a_higher_kinded_type_field_projects_through_an_explicit_dictionary() {
    let source = r#"
        use /std/{Nat, Option, Str};
        pub concept Named(M : (Type) -> Type) : pub Type {
            Carrier : Type,
        }
        let dict : Named(Option) = Named { Carrier = Nat };
        let alias : Type = Named/Carrier(@Option, use dict);
        let v : alias = 3;
        /std/print(Nat/to_str(v))
        "#;

    assert_eq!(run(source), b"3");
}

// A witness over a *partially applied* family: `(A : Type) => Box(S, A)` leaves a stuck application under the binder, and the key reads that application's head — so a two-parameter monad can register its parametric witness. Resolution then finds it when a goal's parameter is pinned to the same partial lambda, unifying the arguments below the head.
#[test]
fn a_witness_keys_through_a_partially_applied_family() {
    let source = r#"
        use /std/{Nat, Str, Monad};
        induct Box(S : Type, A : Type) : Type
        | wrap(A)
        end
        satisfy (@S : Type) => Monad((A : Type) => Box(S, A)) {
            pure(@A, a) = Box/wrap(a),
            bind(@A, @B, m, f) =
                match m : (_) => Box(S, B)
                | wrap(a) => f(a)
                end,
        }
        let unwrap(@S : Type, @A : Type, m : Box(S, A)) -> A =
            match m : (_) => A
            | wrap(a) => a
            end;
        let doubled(@M : (Type) -> Type, use Monad(M), m : M(Nat)) -> M(Nat) =
            Monad/bind(m, (n) => Monad/pure(Nat/add(n, n)));
        let boxed : Box(Str, Nat) = Box/wrap(21);
        /std/print(Nat/to_str(unwrap(doubled(@(A : Type) => Box(Str, A), boxed))))
        "#;

    assert_eq!(run(source), b"42");
}

// Which call needs the witness. A curried application — every partial application, and `Fmt/print(fmt)(a)(b)` in particular — heads its outer apply with another apply, so reading only the outermost node named `<function>` for exactly the calls a reader most needs identified.
//
// The argument is a *labeled* tuple deliberately: `/std/Tuple` shows every positional shape, and labels are part of a tuple type's identity, so the labeled product is what still has no `Show`.
#[test]
fn a_missing_witness_names_a_curried_head_by_its_innermost_reference() {
    let source = r#"
        use /std/{Nat, Bool, Fmt};
        let s = Fmt/print("issue % -> %")(42)((x = 1, y = true));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/std/Fmt/print'"),
        "expected the head named through the spine:\n{report}"
    );
}

// Which premise needs it. A `use` parameter is anonymous by design — `let`, `rec` and `satisfy` sugar declare one without a name — so naming the binder reported `_` for every premise a program actually writes. The position is always there to be named.
//
// The argument is a *labeled* tuple for the reason above: the positional shapes all have a `Show`.
#[test]
fn a_missing_witness_names_the_premise_by_position() {
    let source = r#"
        use /std/{Nat, Bool, Show, Str};
        let f(@A : Type, use Show(A), a : A) -> Str = Show/show(a);
        let s : Str = f((x = 1, y = true));
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/f' for its 1st 'use' premise"),
        "expected the premise named by position:\n{report}"
    );
}

// The position is a position, not a decoration: a head with two premises whose *second* is the unsatisfied one says so, which is the whole reason a number beats a name here.
#[test]
fn a_later_premise_is_named_by_its_own_position() {
    let source = r#"
        use /std/{Nat, Str, Show, Equal};
        induct T : pub Type | t() end
        satisfy Show(T) { show(x) = "t", }
        let g(@A : Type, use Show(A), use Equal(A), a : A) -> Str = Show/show(a);
        let s : Str = g(T/t());
        /std/print("unreachable")
        "#;

    let report = error(source);
    assert!(
        report.contains("needed by '/g' for its 2nd 'use' premise"),
        "expected the second premise named as the second:\n{report}"
    );
}

#[test]
fn a_missing_witness_over_a_nominal_type_spells_its_name() {
    // A goal is rendered as every report renders a type. A nominal declaration is a recursive group of one, and a zonked solution spells it as the `Rec` node itself until the refold gives the name back — the deferred-goal report once skipped that refold and wrote `Spell(rec #0: Type = Opaque; #0)`.
    let report = error(
        r#"
        use /std/{Str, Spell};
        induct Opaque: pub Type | o() end
        let spelled: Str = Spell/spell(Opaque/o());
        /std/print(spelled)
        "#,
    );
    assert!(
        report.contains("no witness of Spell(Opaque) found"),
        "{report}"
    );
}

// A premise may name a constant beside a binder, `Lift(Io, M)` under a head `Lift(Io, (A) => Try(M, E, A))`: it is strictly smaller than the head, so resolution through it still decreases. The transformer's `Io` edge is then written once for every base, and here it resolves at base `Async` through the prelude's `Lift(Io, Async)`.
#[test]
fn a_premise_naming_a_constant_beside_a_binder_resolves_through_the_constant_edge() {
    let source = r#"
        use /std/{Monad, Lift, Result, Io, Async, Nat, Str, print};
        pub struct Try(M: (Type) -> Type, E: Type, A: Type): Type { M(Result(E, A)) }
        let pure(@M: (Type) -> Type, @E: Type, @A: Type, use Monad(M), a: A) -> Try(M, E, A) =
            Try { Monad/pure(Result/success(a)) };
        let bind(@M: (Type) -> Type, @E: Type, @A: Type, @B: Type, use Monad(M), m: Try(M, E, A), f: (A) -> Try(M, E, B)) -> Try(M, E, B) =
            Try { Monad/bind(m.0, (r: Result(E, A)) => match r | success(a) => f(a).0 | failure(e) => Monad/pure(Result/failure(e)) end) };
        satisfy (@M: (Type) -> Type, @E: Type, use Monad(M)) => Monad((A: Type) => Try(M, E, A)) {
            pure(@A, a) = pure(a),
            bind(@A, @B, m, f) = bind(m, f),
        }
        satisfy (@M: (Type) -> Type, @E: Type, use Monad(M), use Lift(Io, M)) => Lift(Io, (A: Type) => Try(M, E, A)) {
            lift(@A, m) = Try { Monad/bind(Lift/lift(m), (a: A) => Monad/pure(Result/success(a))) },
        }
        satisfy (@E: Type) => Lift(Async, (A: Type) => Try(Async, E, A)) {
            lift(@A, m) = Try { Async/map(m, (a: A) => Result/success(a)) },
        }
        let body: Try(Async, Nat, Nat) =
            let _ = print("a")!;
            let _ = Async/yield_now!;
            let _ = print("b")!;
            pure(3);
        let fiber: Async({}) =
            let r = body.0!;
            match r
            | success(n) => Async/lift(print(Nat/to_str(n)))
            | failure(_) => Async/lift(print("failed"))
            end;
        Async/run(fiber)
        "#;

    assert_eq!(run(source), b"ab3");
}

// A premise no smaller than the head would let resolution recurse into an equal goal forever, so it is refused where it is declared.
#[test]
fn a_premise_no_smaller_than_its_head_is_refused() {
    let source = r#"
        use /std/{Nat, Str, List};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy (@A: Type, use Show(List(A))) => Show(List(A)) {
            show(l) = Show/show(l)
        }
        /std/print("unreached")
        "#;

    let message = error(source);
    assert!(
        message.contains("non-regular premise"),
        "expected the premise refusal, got: {message}"
    );
}
