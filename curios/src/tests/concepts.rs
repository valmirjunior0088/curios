use {super::run, curios_runtime::MockHost, std::time::Duration};

fn error(source: &str) -> String {
    let (system, _io) = MockHost::builder().build();
    match crate::run_text(Duration::from_secs(10), source, system) {
        Ok(_) => panic!("expected an error, program succeeded"),
        Err(error) => error.to_string(),
    }
}

// The base case: a concept, a witness keyed on a rigid nominal head, and a call
// through the generated method wrapper. `Show/show(n)` saturates `@A` with a
// metavar and the `use` slot with a witness goal; solving `A := Nat` from `n`
// wakes the goal, which the global table resolves to `show_nat`.
#[test]
fn concept_witness_resolves_through_wrapper() {
    let source = r#"
        use /std/{Nat, Handle, Str};
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

// A premised witness: `show_arr` needs a `Show(A)` to show its elements. The
// resolver instantiates its telescope — `@A := ?B` with premise goal
// `Show(?B)` — unifies `Show(Lst(?B)) ≡ Show(Lst(Nat))` to solve `?B := Nat`,
// then resolves the premise to `show_nat`.
#[test]
fn premised_witness_resolves_recursively() {
    let source = r#"
        use /std/{Nat, Handle, Str, Lst};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy (@A : Type, use Show(A)) => Show(Lst(A)) {
            show(l) =
                Lst/fold(l, "[", (x, acc) => Str/concat(acc, Show/show(x)))
        }
        let l : Lst(Nat) = [1, 2, 3];
        /std/print(Show/show(l))
        "#;

    assert_eq!(run(source), b"[123");
}

// An explicit `use` argument overrides table resolution: a local dictionary
// value (an ordinary `let` of the concept's record type) is passed at the call
// site and used instead of the registered `show_nat`.
#[test]
fn explicit_use_argument_overrides() {
    let source = r#"
        use /std/{Nat, Handle, Str};
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

// A superclass edge resolved by projection: inside `same`, the goal `Eql(A)`
// has a bound-variable head (no table entry), so it is solved by projecting the
// local `use Ord(A)` binder's (anonymous) superclass field, keyed by index. The
// `use Ord(A)` slot itself resolves through the table to `ord_nat`, whose own
// omitted superclass field resolves to `eql_nat` — no field names a witness
// anywhere.
#[test]
fn superclass_projection_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Order, Handle};
        pub concept Eql(A : Type) : pub Type {
            eql(A, A) -> Bool
        }
        pub concept Ord(A : Type) : pub Type {
            use Eql(A),
            cmp(A, A) -> Order
        }
        satisfy Eql(Nat) {
            eql(a, b) = a == b
        }
        satisfy Ord(Nat) {
            cmp(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord(A), x : A, y : A) -> Bool = Eql/eql(x, y);
        let n : Nat = 3;
        /std/print(Bool/to_str(same(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// No witness registered for the goal's head: a resolution-time error.
#[test]
fn missing_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str};
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

// The prelude-provided `Show` concept and its witnesses resolve, proving the
// cached-prelude replay path registers concepts and witnesses.
#[test]
fn prelude_show_resolves() {
    let source = r#"
        use /std/{Nat, Handle, Show};
        let n : Nat = 42;
        /std/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// The prelude `Eql` concept resolves through the value-level witnesses.
#[test]
fn prelude_eql_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Eql};
        let a : Nat = 5;
        let b : Nat = 5;
        /std/print(Bool/to_str(Eql/eql(a, b)))
        "#;

    assert_eq!(run(source), b"true");
}

// The prelude `Ord` concept resolves, and its `Eql` superclass is reachable by
// projection from an `Ord` in scope.
#[test]
fn prelude_ord_superclass_projects() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Ord, Eql};
        pub let equal(@A : Type, use Ord(A), x : A, y : A) -> Bool = Eql/eql(x, y);
        let n : Nat = 4;
        /std/print(Bool/to_str(equal(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// Registering two witnesses for the same `(concept, head)` key is a coherence
// error (global uniqueness) — independent of, and checked alongside, the
// orphan rule below.
#[test]
fn duplicate_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Handle, Str};
        pub concept Show(A : Type) : pub Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 1;
        /std/print(Show/show(n))
        "#;

    assert!(error(source).to_lowercase().contains("witness"));
}

// The orphan rule: a witness may be declared only where the concept it
// witnesses, or a type in its key, is already declared. `Ord` and `Bool` are
// both `/std`/`/sys`-owned and the entry program owns neither, so `Ord(Bool)`
// (not already witnessed anywhere in the standard library) is rejected.
#[test]
fn orphan_witness_is_rejected() {
    let source = r#"
        use /std/{Bool, Ord, Order};
        satisfy Ord(Bool) {
            cmp(a, b) = Order/eq()
        }
        let n : Bool = true;
        n
        "#;

    assert!(error(source).to_lowercase().contains("orphan"));
}

// The sanctioned counterpart: a user's own type is legal to `satisfy` a
// standard-library concept for, since the declaring root (the entry program)
// owns the key's type even though it doesn't own the concept.
#[test]
fn witness_for_a_locally_owned_type_is_not_an_orphan() {
    let source = r#"
        use /std/{Nat, Handle, Str, Show};
        pub struct Wrapper : pub Type { inner : Nat }
        satisfy Show(Wrapper) {
            show(w) = Nat/to_str(w.inner)
        }
        let w : Wrapper = Wrapper { inner = 7 };
        /std/print(Show/show(w))
        "#;

    assert_eq!(run(source), b"7");
}

// Multi-parameter concepts key on the tuple of every parameter head: two
// witnesses may share a first head as long as the full parameter tuple differs,
// and each resolves once both parameters are pinned.
#[test]
fn multi_param_witnesses_share_a_first_head() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str};
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

// Every concept parameter participates in the witness key, so a goal whose
// second parameter is never pinned parks and surfaces as an error at the end
// of the module — no accidental inference from the witness.
#[test]
fn open_parameter_does_not_infer_from_the_witness() {
    let source = r#"
        use /std/{Nat, Handle, Str};
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

// The full higher-kinded chain: `Monad/bind(o, f)` parks its `Monad(?M)` goal
// on the flex parameter, checking `o : Option(Nat)` against `?M(?A)` fires the
// flex-apply imitation rule inside the conversion checker, and the committed
// `?M := Option` wakes the parked goal, which the table resolves to the
// prelude's `monad_option`. Also covers the cached-prelude replay of a
// higher-kinded witness.
#[test]
fn prelude_monad_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, Handle, Str, Option, Monad};
        let o : Option(Nat) = Monad/bind(Option/some(20), (x) => Monad/pure(Nat/add(x, 1)));
        /std/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"21");
}

// The Lst witness: bind is concat-map.
#[test]
fn prelude_monad_arr_binds() {
    let source = r#"
        use /std/{Nat, Handle, Str, Lst, Monad};
        let l : Lst(Nat) = [1, 2];
        let doubled : Lst(Nat) = Monad/bind(l, (x) => [x, x]);
        /std/print(Nat/to_str(Lst/len(doubled)))
        "#;

    assert_eq!(run(source), b"4");
}

// The monadic sugar: each `e!` desugars to `/syn/Monad/bind(e, cont)`, whose
// `use` binder resolves the `Monad` witness from the action's type — no
// header, no imports needed for the dispatch itself.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Handle, Str, Option, Monad};
        pub let chain(a : Option(Nat), b : Option(Nat)) -> Option(Nat) =
            let x = a!;
            let y = b!;
            Monad/pure(Nat/add(x, y));
        /std/print(Nat/to_str(Option/unwrap_or(chain(Option/some(20), Option/some(22)), 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// Generic do-notation: `!` inside a function that is generic over the monad.
// Each site's `Monad(M)` goal (M a bound variable) resolves against the local
// `use` binder — impossible with a concrete bind function, and the payoff of
// dispatching `!` through the concept.
#[test]
fn bang_works_in_monad_generic_code() {
    let source = r#"
        use /syn/{Monad};
        use /std/{Nat, Handle, Str, Option, Lst};
        pub let add_both(@M : (Type) -> Type, use Monad(M), a : M(Nat), b : M(Nat)) -> M(Nat) =
            Monad/pure(a! + b!);
        let o : Option(Nat) = add_both(Option/some(20), Option/some(22));
        let l : Lst(Nat) = add_both([1, 2], [10]);
        /std/print(Str/concat(
            Nat/to_str(Option/unwrap_or(o, 0)),
            Nat/to_str(Lst/len(l))))
        "#;

    assert_eq!(run(source), b"422");
}

// A higher-kinded superclass: inside the generic function the goal
// `Monad(M)` (M a bound variable) resolves through step 2's superclass
// projection of the local `use MonadPlus(M)` binder. The witness's own
// omitted `monad` field resolves through the table to the std `Monad(Option)`
// witness — a higher-kinded auto-fill.
#[test]
fn higher_kinded_superclass_projects() {
    let source = r#"
        use /std/{Nat, Handle, Str, Option, Monad};
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

// `Prim`-headed type constructors (`Lst`, `Cell`) carry their argument inside
// the `Prim` node; the imitation rule rebuilds the node over the binder
// (`?M := λT. Lst(T)`), so `Monad/bind` over an `Lst` pins the witness from
// the action's type like any nominal constructor would.
#[test]
fn monad_over_prim_constructor_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, Lst, Handle, Str, Monad};
        let a : Lst(Nat) = [1];
        let b : Lst(Nat) = Monad/bind(a, (x) => a);
        /std/print(Nat/to_str(Lst/len(b)))
        "#;

    assert_eq!(run(source), b"1");
}

// The syn-homed operator concepts: `Add/add` resolves on a primitive type
// through the `/std` witness (also proving the cached-prelude replay path
// registers the syn concepts and std witnesses), on a user struct through a
// user witness, and in generic code through a local `use Add(A)` premise.
#[test]
fn syn_add_concept_resolves_everywhere() {
    let source = r#"
        use /std/{Nat, Handle, Str, Add};
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

// `Eql` and `Cmp` resolve across primitives with the witnesses now homed beside
// each type — `Eql(Nat)`/`Cmp(Nat)` in `/std/Nat`, `Eql(Str)` in `/std/Str`,
// `Cmp(Flt)` in `/std/Flt` — rather than in the operator-concept facades, which
// keep only the concept re-exports.
#[test]
fn eql_and_cmp_resolve_across_primitives() {
    let source = r#"
        use /std/{Nat, Flt, Bool, Handle, Str, Eql, Cmp};
        let a : Bool = Eql/eql(2, 2);
        let b : Bool = Eql/eql("abc", "abc");
        let c : Bool = Cmp/lt(1.0, 2.0);
        let d : Bool = Cmp/gte(3, 3);
        /std/print(Bool/to_str(Bool/and(Bool/and(a, b), Bool/and(c, d))))
        "#;

    assert_eq!(run(source), b"true");
}

// An explicit `use <term>` fill in a concept literal overrides table
// resolution for that field: the flipped equality rides inside the `Ord2`
// value, while the registered witness is untouched. The superclass field is
// anonymous, so the override is observed by resolution — with `o` in instance
// scope, the omitted `Eq2(Nat)` goal projects its superclass (the flipped
// equality), taking precedence over the global table.
#[test]
fn use_entry_fills_a_concept_field_explicitly() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Order
        }
        satisfy Eq2(Nat) {
            eq2(a, b) = a == b
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Order/lt() };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o)))
        "#;

    assert_eq!(run(source), b"false");
}

// A witness body is a concept literal, so `use <term>` fills its superclass
// field there too.
#[test]
fn use_entry_fills_a_witness_superclass() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq3(A : Type) : pub Type {
            eq3(A, A) -> Bool
        }
        pub concept Ord3(A : Type) : pub Type {
            use Eq3(A),
            cmp3(A, A) -> Order
        }
        satisfy Ord3(Nat) {
            use Eq3 { eq3(a, b) = a == b },
            cmp3(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord3(A), x : A, y : A) -> Bool = Eq3/eq3(x, y);
        /std/print(Bool/to_str(same(2, 2)))
        "#;

    assert_eq!(run(source), b"true");
}

// A superclass field is anonymous, so its concept's former field name is not a
// label: assigning it is a plain unknown-field error, with no special `use`-field
// diagnostic (`Eql`'s superclass is reached by resolution, never by name).
#[test]
fn labeled_fill_of_a_former_superclass_is_unknown() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq4(A : Type) : pub Type {
            eq4(A, A) -> Bool
        }
        pub concept Ord4(A : Type) : pub Type {
            use Eq4(A),
            cmp4(A, A) -> Order
        }
        satisfy Eq4(Nat) {
            eq4(a, b) = a == b
        }
        let bad : Ord4(Nat) = Ord4 { eq4 = Eq4 { eq4(a, b) = a == b } };
        /std/print("no")
        "#;

    let message = error(source);
    assert!(message.contains("'eq4'"), "got: {message}");
    assert!(message.contains("no field"), "got: {message}");
}

// `use` entries are rejected outside concept literals, and surplus entries
// are rejected against the concept's `use`-field count.
#[test]
fn misplaced_use_entries_are_errors() {
    let non_concept = r#"
        use /std/{Nat, Handle, Str};
        pub struct Pair : pub Type { fst : Nat, snd : Nat }
        let p = Pair { use 1, snd = 2 };
        /std/print("no")
        "#;
    assert!(error(non_concept).contains("not a concept"));

    let surplus = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq5(A : Type) : pub Type {
            eq5(A, A) -> Bool
        }
        pub concept Ord5(A : Type) : pub Type {
            use Eq5(A),
            cmp5(A, A) -> Order
        }
        satisfy Eq5(Nat) {
            eq5(a, b) = a == b
        }
        satisfy Ord5(Nat) {
            use Eq5 { eq5(a, b) = a == b },
            use Eq5 { eq5(a, b) = a == b },
            cmp5(a, b) = Order/lt()
        }
        /std/print("no")
        "#;
    assert!(error(surplus).contains("'use' entr"));
}

// An omitted superclass field inside a *premised* witness resolves through
// the local `use` premise (resolution's local step), not the table: the
// element equality is the premise's, threaded structurally.
#[test]
fn omitted_superclass_resolves_from_a_premise() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order, Lst};
        pub concept Eq6(A : Type) : pub Type {
            eq6(A, A) -> Bool
        }
        pub concept Ord6(A : Type) : pub Type {
            use Eq6(A),
            cmp6(A, A) -> Order
        }
        satisfy Eq6(Nat) {
            eq6(a, b) = a == b
        }
        satisfy (@A : Type, use Eq6(A)) => Eq6(Lst(A)) {
            eq6(a, b) = Lst/len(a) == Lst/len(b)
        }
        satisfy (@A : Type, use Ord6(A)) => Ord6(Lst(A)) {
            cmp6(a, b) = Order/lt()
        }
        satisfy Ord6(Nat) {
            cmp6(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord6(A), x : A, y : A) -> Bool = Eq6/eq6(x, y);
        let l : Lst(Nat) = [1, 2];
        /std/print(Bool/to_str(same(l, l)))
        "#;

    assert_eq!(run(source), b"true");
}

// A spread in a concept literal *copies* the anonymous superclass field from
// the base rather than re-resolving it: `o` carries the flipped (always
// false) equality, and the update must preserve it — table resolution would
// find the registered `Eq2(Nat)` and answer true.
#[test]
fn concept_literal_spread_copies_superclass() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Order
        }
        satisfy Eq2(Nat) {
            eq2(a, b) = a == b
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Order/lt() };
        let o2 : Ord2(Nat) = Ord2 { ..o, cmp2(a, b) = Order/gt() };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o2)))
        "#;

    assert_eq!(run(source), b"false");
}

// An explicit `use <term>` entry after the spread still overrides the
// superclass, while the plain fields copy across.
#[test]
fn concept_literal_spread_use_override() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str, Order};
        pub concept Eq2(A : Type) : pub Type {
            eq2(A, A) -> Bool
        }
        pub concept Ord2(A : Type) : pub Type {
            use Eq2(A),
            cmp2(A, A) -> Order
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let straight : Eq2(Nat) = Eq2 { eq2(a, b) = true };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Order/lt() };
        let o2 : Ord2(Nat) = Ord2 { ..o, use straight };
        pub let observe(use Ord2(Nat)) -> Bool = Eq2/eq2(1, 1);
        /std/print(Bool/to_str(observe(use o2)))
        "#;

    assert_eq!(run(source), b"true");
}

// A `use` entry after a spread is still only legal in concept literals.
#[test]
fn concept_literal_spread_use_on_non_concept_rejected() {
    let source = r#"
        use /std/{Nat, Handle};
        pub struct Pair(A : Type, B : Type) : pub Type { fst : A, snd : B }
        let p : Pair(Nat, Nat) = Pair { fst = 1, snd = 2 };
        let bad = Pair { ..p, use 1 };
        /std/print("no")
        "#;

    assert!(error(source).contains("not a concept"));
}

// A `Prop`-sorted concept: the witness is proof content and erases completely,
// and the method result is consumed in an erased argument slot. The runtime
// path never sees the concept apparatus.
#[test]
fn prop_concept_resolves_and_erases() {
    let source = r#"
        use /std/{Nat, Str, Eq, Handle};
        pub concept Refl(A : Type) : pub Prop {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let ignore_proof(p : Eq(2, 2), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(ignore_proof(Refl/proof(2), 3)))
        "#;

    assert_eq!(run(source), b"3");
}

// A proof-returning method wrapper demanded by a top-level binding. The
// wrapper call returns an erased method, so the outer application's callee is
// proof content rather than a function — `erase_apply` collapses it to the
// unit constant (value-driven: a direct function reference like
// `/std/proc/exit` keeps its call). Regression test: this used to survive
// erasure as an application of an erased callee and panic `into_cont`.
#[test]
fn prop_method_in_top_level_binding_collapses() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        pub concept Refl(A : Type) : pub Prop {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let probe(@A : Type, x : A, use Refl(A)) -> Eq(x, x) = Refl/proof(x);
        let direct : Eq(2, 2) = Refl/proof(2);
        let routed : Eq(3, 3) = probe(3);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The `Type`-sorted twin: the concept record is kept, but the method's result
// is still a proposition, so the wrapper application collapses identically.
// Regression test: this used to reach runtime as a call of an erased unit and
// trap.
#[test]
fn type_concept_prop_method_binding_collapses() {
    let source = r#"
        use /std/{Nat, Eq, Handle};
        pub concept Refl(A : Type) : pub Type {
            proof(x : A) -> Eq(x, x)
        }
        satisfy Refl(Nat) {
            proof(x) = Eq/refl()
        }
        let evidence : Eq(2, 2) = Refl/proof(2);
        /std/print("ok")
        "#;

    assert_eq!(run(source), b"ok");
}

// The laws pattern: a `Prop` concept whose field quantifies over another
// concept with a `use` parameter (a verified interface). The witness supplies
// a proof (binding the `use` slot positionally), resolution supplies both
// witnesses at the call, and everything erases.
#[test]
fn prop_laws_concept_resolves() {
    let source = r#"
        use /std/{Nat, Str, Show, Eq, Handle};
        pub concept ShowLaws(A : Type) : pub Prop {
            stable(use Show(A), x : A) -> Eq(Show/show(x), Show/show(x))
        }
        satisfy ShowLaws(Nat) {
            stable(use w, x) = Eq/refl()
        }
        let take(q : Eq(Show/show(7), Show/show(7)), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(take(ShowLaws/stable(7), 42)))
        "#;

    assert_eq!(run(source), b"42");
}

// A sealed concept (`: Type`, no `pub` on the representation) rejects a
// witness declared outside its module: the satisfy body is a dictionary
// literal, and construction requires the representation.
#[test]
fn sealed_concept_rejects_foreign_satisfy() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        satisfy Tag(Str) {
            tag(s) = s
        }
        /std/print("no")
        "#;

    assert!(error(source).contains("private"));
}

// A sealed concept also rejects a forged dictionary literal outside its
// module — the local-override idiom is only available on transparent
// concepts.
#[test]
fn sealed_concept_rejects_foreign_dictionary_literal() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        let forged : Tag(Nat) = Tag { tag(n) = "forged" };
        /std/print("no")
        "#;

    assert!(error(source).contains("private"));
}

// Sealing gates construction only: cross-module `use` parameters, global
// resolution, and the generated method wrappers (owner-module items) all
// keep working.
#[test]
fn sealed_concept_resolves_and_projects_cross_module() {
    let source = r#"
        use /std/{Nat, Str, Handle};
        mod Guard
            use /std/{Nat, Str};
            pub concept Tag(A : Type) : Type {
                tag(A) -> Str
            }
            satisfy Tag(Nat) {
                tag(n) = Nat/to_str(n)
            }
        end
        use Guard/{Tag};
        let describe(@A : Type, use Tag(A), x : A) -> Str = Tag/tag(x);
        /std/print(describe(42))
        "#;

    assert_eq!(run(source), b"42");
}

// A sealed concept with a superclass edge, resolved cross-module: the
// elaborator discharges the inner goal by projecting the local witness — a
// machinery-built projection of a private representation spliced into the
// consumer's body — and erasure re-derives its type with privacy suppressed.
// Regression test for the suppression bracket: with privacy enforced at
// erasure this program is spuriously rejected.
#[test]
fn sealed_concept_superclass_resolves_cross_module() {
    let source = r#"
        use /std/{Nat, Bool, Handle};
        mod Guard
            use /std/{Nat, Bool};
            pub concept Eq2(A : Type) : Type {
                eq2(A, A) -> Bool
            }
            pub concept Ord2(A : Type) : Type {
                use Eq2(A),
                le(A, A) -> Bool
            }
            satisfy Eq2(Nat) {
                eq2(a, b) = Nat/eql(a, b)
            }
            satisfy Ord2(Nat) {
                le(a, b) = true
            }
        end
        use Guard/{Eq2, Ord2};
        let same(@A : Type, use Ord2(A), x : A) -> Bool = Eq2/eq2(x, x);
        /std/print(Bool/to_str(same(7)))
        "#;

    assert_eq!(run(source), b"true");
}

// A sealed `Prop` concept is an owner-certified marker: only the owner mints
// witnesses, consumers demand the certificate as an erased premise, and the
// whole apparatus erases.
#[test]
fn sealed_prop_concept_certifies() {
    let source = r#"
        use /std/{Nat, Str, Eq, Handle};
        mod Guard
            use /std/{Nat, Eq};
            pub concept Certified(A : Type) : Prop {
                proof(x : A) -> Eq(x, x)
            }
            satisfy Certified(Nat) {
                proof(x) = Eq/refl()
            }
        end
        use Guard/{Certified};
        let ignore(p : Eq(2, 2), n : Nat) -> Nat = n;
        /std/print(Nat/to_str(ignore(Certified/proof(2), 3)))
        "#;

    assert_eq!(run(source), b"3");
}

// A witness declared *after* a value that uses it still resolves: the use-site
// goal defers on the missing table entry, the later `satisfy` registers it, and
// the end-of-module sweep discharges the deferred goal. This ordering freedom is
// what lets a `/std` witness live beside its type — a type module's own value
// functions may call an operator before the module's trailing witness block, the
// way `/std/Nat`'s `min`/`cmp` use `<`/`==` ahead of `Cmp(Nat)`.
#[test]
fn forward_declared_witness_resolves() {
    let source = r#"
        use /std/{Nat, Bool, Handle, Str};
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

// A missing operator witness used in an inductive's constructor *index type*
// once surfaced as a bare `?m ≡ ?n` metavariable mismatch: the constructor is
// elaborated twice, and reconciling the two elaborations parks a conversion
// between their (unsolvable) witness holes. It is now reported as the unresolved
// witness it is, naming the concept, the key, and the `+` that needed it.
#[test]
fn missing_witness_in_constructor_index_names_the_concept() {
    let source = r#"
        use /std/{Nat, Handle, Add};
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
