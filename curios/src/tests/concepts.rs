use {curios_rt::MockHost, std::time::Duration};

fn run(source: &str) -> Vec<u8> {
    let (system, io) = MockHost::builder().build();
    crate::run_text(Duration::from_secs(10), source, system).expect("expected result");
    io.output().to_vec()
}

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
        use /std/{Nat, Io, Str};
        pub concept Show(A : Type) : Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 42;
        Io/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// A premised witness: `show_lst` needs a `Show(A)` to show its elements. The
// resolver instantiates its telescope — `@A := ?B` with premise goal
// `Show(?B)` — unifies `Show(Lst(?B)) ≡ Show(Lst(Nat))` to solve `?B := Nat`,
// then resolves the premise to `show_nat`.
#[test]
fn premised_witness_resolves_recursively() {
    let source = r#"
        use /std/{Nat, Io, Str, Lst};
        pub concept Show(A : Type) : Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy(@A : Type, use Show(A)) Show(Lst(A)) {
            show(l) =
                Lst/fold(l, "[", (x, acc) => Str/concat(acc, Show/show(x)))
        }
        let l : Lst(Nat) = [1, 2, 3];
        Io/print(Show/show(l))
        "#;

    assert_eq!(run(source), b"[123");
}

// An explicit `use` argument overrides table resolution: a local dictionary
// value (an ordinary `let` of the concept's record type) is passed at the call
// site and used instead of the registered `show_nat`.
#[test]
fn explicit_use_argument_overrides() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Show(A : Type) : Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let parens : Show(Nat) =
            Show(Nat) { show = (n) => Str/concat("(", Str/concat(Nat/to_str(n), ")")) };
        let n : Nat = 7;
        Io/print(Show/show(use parens, n))
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
        use /std/{Nat, Bln, Order, Io};
        pub concept Eql(A : Type) : Type {
            eql(A, A) -> Bln
        }
        pub concept Ord(A : Type) : Type {
            use Eql(A),
            cmp(A, A) -> Order
        }
        satisfy Eql(Nat) {
            eql(a, b) = a == b
        }
        satisfy Ord(Nat) {
            cmp(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord(A), x : A, y : A) -> Bln = Eql/eql(x, y);
        let n : Nat = 3;
        Io/print(Bln/to_str(same(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// No witness registered for the goal's head: a resolution-time error.
#[test]
fn missing_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str};
        pub concept Show(A : Type) : Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let b : Bln = true;
        Io/print(Show/show(b))
        "#;

    assert!(error(source).contains("witness"));
}

// The prelude-provided `Show` concept and its witnesses resolve, proving the
// cached-prelude replay path registers concepts and witnesses.
#[test]
fn prelude_show_resolves() {
    let source = r#"
        use /std/{Nat, Io, Show};
        let n : Nat = 42;
        Io/print(Show/show(n))
        "#;

    assert_eq!(run(source), b"42");
}

// The prelude `Eql` concept resolves through the value-level witnesses.
#[test]
fn prelude_eql_resolves() {
    let source = r#"
        use /std/{Nat, Bln, Io, Eql};
        let a : Nat = 5;
        let b : Nat = 5;
        Io/print(Bln/to_str(Eql/eql(a, b)))
        "#;

    assert_eq!(run(source), b"true");
}

// The prelude `Ord` concept resolves, and its `Eql` superclass is reachable by
// projection from an `Ord` in scope.
#[test]
fn prelude_ord_superclass_projects() {
    let source = r#"
        use /std/{Nat, Bln, Io, Ord, Eql};
        pub let equal(@A : Type, use Ord(A), x : A, y : A) -> Bln = Eql/eql(x, y);
        let n : Nat = 4;
        Io/print(Bln/to_str(equal(n, n)))
        "#;

    assert_eq!(run(source), b"true");
}

// Registering two witnesses for the same `(concept, head)` key is a coherence
// error (global uniqueness, no orphan rule).
#[test]
fn duplicate_witness_is_an_error() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Show(A : Type) : Type {
            show(A) -> Str
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        satisfy Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        let n : Nat = 1;
        Io/print(Show/show(n))
        "#;

    assert!(error(source).to_lowercase().contains("witness"));
}

// Multi-parameter concepts key on the tuple of every input head: two
// witnesses may share a first head as long as the full input tuple differs,
// and each resolves once both parameters are pinned.
#[test]
fn multi_param_witnesses_share_a_first_head() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str};
        pub concept Into(A : Type, B : Type) : Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        satisfy Into(Nat, Bln) {
            into(n) = Nat/eql(n, 1)
        }
        let s : Str = Into/into(2);
        let b : Bln = Into/into(2);
        Io/print(Bln/to_str(b))
        "#;

    assert_eq!(run(source), b"false");
}

// Without an `out` marker every parameter is an input, so a goal whose second
// parameter is never pinned parks on the flex input and surfaces as an error
// at the end of the module — no accidental output inference from the witness.
#[test]
fn open_input_parameter_does_not_infer_from_the_witness() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Into(A : Type, B : Type) : Type {
            into(A) -> B
        }
        satisfy Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        pub let discard(@A : Type, x : A) -> Nat = 0;
        Io/print(Nat/to_str(discard(Into/into(1))))
        "#;

    let message = error(source).to_lowercase();
    assert!(message.contains("witness") || message.contains("infer"));
}

// An `out` parameter is excluded from the witness key: the goal
// `Convert(Nat, ?B)` resolves on `Nat` alone and the witness's terminal
// unification pins `?B := Str` — nothing else constrains `B`.
#[test]
fn out_parameter_is_inferred_from_the_witness() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Convert(A : Type, out B : Type) : Type {
            convert(A) -> B
        }
        satisfy Convert(Nat, Str) {
            convert(n) = Nat/to_str(n)
        }
        pub let ignore(@A : Type, x : A) -> Nat = 7;
        Io/print(Nat/to_str(ignore(Convert/convert(1))))
        "#;

    assert_eq!(run(source), b"7");
}

// Same input tuple + different outputs is a functional-dependency violation:
// both witnesses key on `Nat` once `B` is `out`, so the second registration
// collides.
#[test]
fn fundep_violation_is_a_duplicate_witness_error() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str};
        pub concept Convert(A : Type, out B : Type) : Type {
            convert(A) -> B
        }
        satisfy Convert(Nat, Str) {
            convert(n) = Nat/to_str(n)
        }
        satisfy Convert(Nat, Bln) {
            convert(n) = Nat/eql(n, 1)
        }
        let s : Str = Convert/convert(1);
        Io/print(s)
        "#;

    assert!(error(source).to_lowercase().contains("witness"));
}

// A local `use` binder pins an open `out` parameter through step 1's
// committing match: inside `go`, the goal `Convert(Nat, ?B)` matches the
// binder `w : Convert(Nat, Str)` and commits `?B := Str`.
#[test]
fn local_binder_pins_an_out_parameter() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Convert(A : Type, out B : Type) : Type {
            convert(A) -> B
        }
        satisfy Convert(Nat, Str) {
            convert(n) = Nat/to_str(n)
        }
        pub let ignore(@A : Type, x : A) -> Nat = 9;
        pub let go(use Convert(Nat, Str), x : Nat) -> Nat = ignore(Convert/convert(x));
        Io/print(Nat/to_str(go(1)))
        "#;

    assert_eq!(run(source), b"9");
}

// Marking every parameter `out` leaves an empty witness key — rejected at
// lowering.
#[test]
fn all_out_concept_is_rejected() {
    let source = r#"
        use /std/{Nat, Io, Str};
        pub concept Make(out A : Type) : Type {
            make() -> A
        }
        let n : Nat = 1;
        Io/print(Nat/to_str(n))
        "#;

    assert!(error(source).contains("out"));
}

// The full higher-kinded chain: `Monad/bind(o, f)` parks its `Monad(?M)` goal
// on the flex input, checking `o : Option(Nat)` against `?M(?A)` fires the
// flex-apply imitation rule inside the conversion checker, and the committed
// `?M := Option` wakes the parked goal, which the table resolves to the
// prelude's `monad_option`. Also covers the cached-prelude replay of a
// higher-kinded witness.
#[test]
fn prelude_monad_resolves_by_imitation() {
    let source = r#"
        use /std/{Nat, Io, Str, Option, Monad};
        let o : Option(Nat) = Monad/bind(Option/some(20), (x) => Monad/pure(Nat/add(x, 1)));
        Io/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"21");
}

// The Lst witness: bind is concat-map.
#[test]
fn prelude_monad_lst_binds() {
    let source = r#"
        use /std/{Nat, Io, Str, Lst, Monad};
        let l : Lst(Nat) = Lst/cons(1, Lst/cons(2, Lst/nil()));
        let doubled : Lst(Nat) = Monad/bind(l, (x) => Lst/cons(x, Lst/cons(x, Lst/nil())));
        Io/print(Nat/to_str(Lst/len(doubled)))
        "#;

    assert_eq!(run(source), b"4");
}

// The monadic sugar: each `e!` desugars to `/syn/Monad/bind(e, cont)`, whose
// `use` binder resolves the `Monad` witness from the action's type — no
// header, no imports needed for the dispatch itself.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Io, Str, Option, Monad};
        pub let chain(a : Option(Nat), b : Option(Nat)) -> Option(Nat) =
            let x = a!;
            let y = b!;
            Monad/pure(Nat/add(x, y));
        Io/print(Nat/to_str(Option/unwrap_or(chain(Option/some(20), Option/some(22)), 0)))
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
        use /std/{Nat, Io, Str, Option, Lst};
        pub let add_both(@M : (Type) -> Type, use Monad(M), a : M(Nat), b : M(Nat)) -> M(Nat) =
            Monad/pure(a! + b!);
        let o : Option(Nat) = add_both(Option/some(20), Option/some(22));
        let l : Lst(Nat) = add_both([1, 2], [10]);
        Io/print(Str/concat(
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
        use /std/{Nat, Io, Str, Option, Monad};
        pub concept MonadPlus(M : (Type) -> Type) : Type {
            use Monad(M),
            empty(@A : Type) -> M(A)
        }
        satisfy MonadPlus(Option) {
            empty(A) = Option/none()
        }
        pub let wrap(@M : (Type) -> Type, use MonadPlus(M), m : M(Nat)) -> M(Nat) =
            Monad/bind(m, (x) => Monad/pure(x));
        let o : Option(Nat) = wrap(Option/some(11));
        Io/print(Nat/to_str(Option/unwrap_or(o, 0)))
        "#;

    assert_eq!(run(source), b"11");
}

// `Prim`-headed type constructors (`Arr`, `Cell`) carry their argument inside
// the `Prim` node — deliberately outside the imitation rule's nominal-head
// scope. `Monad/bind` over an `Arr` fails with the ordinary type mismatch at
// its origin (no panic, no wrong solution).
#[test]
fn monad_over_prim_constructor_is_a_type_mismatch() {
    let source = r#"
        use /std/{Nat, Arr, Io, Str, Monad};
        let a : Arr(Nat) = [|1|];
        let b : Arr(Nat) = Monad/bind(a, (x) => a);
        Io/print("done")
        "#;

    assert!(error(source).contains("mismatch"));
}

// The sys-homed operator concepts: `Add/add` resolves on a primitive type
// through the `/sys` witness (also proving the cached-prelude replay path
// registers the sys concepts and witnesses), on a user record through a user
// witness, and in generic code through a local `use Add(A)` premise.
#[test]
fn sys_add_concept_resolves_everywhere() {
    let source = r#"
        use /std/{Nat, Io, Str, Add};
        record Point : Type { x : Nat, y : Nat }
        satisfy Add(Point) {
            add(a, b) = Point { x = Nat/add(a.x, b.x), y = Nat/add(a.y, b.y) }
        }
        pub let double(@A : Type, use Add(A), v : A) -> A = Add/add(v, v);
        let p : Point = double(Point { x = 3, y = 4 });
        let n : Nat = Add/add(20, 1);
        Io/print(Nat/to_str(Nat/add(p.x, n)))
        "#;

    assert_eq!(run(source), b"27");
}

// The migrated `Eql`: primitive witnesses now live in `/sys` (the std module
// is a facade), `eql_str` stays std-side beside its `Str` dependency, and the
// `Cmp` comparison concept resolves on every numeric type.
#[test]
fn sys_eql_and_cmp_resolve() {
    let source = r#"
        use /std/{Nat, Flt, Bln, Io, Str, Eql, Cmp};
        let a : Bln = Eql/eql(2, 2);
        let b : Bln = Eql/eql("abc", "abc");
        let c : Bln = Cmp/lt(1.0, 2.0);
        let d : Bln = Cmp/gte(3, 3);
        Io/print(Bln/to_str(Bln/and(Bln/and(a, b), Bln/and(c, d))))
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
        use /std/{Nat, Bln, Io, Str, Order};
        pub concept Eq2(A : Type) : Type {
            eq2(A, A) -> Bln
        }
        pub concept Ord2(A : Type) : Type {
            use Eq2(A),
            cmp2(A, A) -> Order
        }
        satisfy Eq2(Nat) {
            eq2(a, b) = a == b
        }
        let flipped : Eq2(Nat) = Eq2 { eq2(a, b) = false };
        let o : Ord2(Nat) = Ord2 { use flipped, cmp2(a, b) = Order/lt() };
        pub let observe(use Ord2(Nat)) -> Bln = Eq2/eq2(1, 1);
        Io/print(Bln/to_str(observe(use o)))
        "#;

    assert_eq!(run(source), b"false");
}

// A witness body is a concept literal, so `use <term>` fills its superclass
// field there too.
#[test]
fn use_entry_fills_a_witness_superclass() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str, Order};
        pub concept Eq3(A : Type) : Type {
            eq3(A, A) -> Bln
        }
        pub concept Ord3(A : Type) : Type {
            use Eq3(A),
            cmp3(A, A) -> Order
        }
        satisfy Ord3(Nat) {
            use Eq3 { eq3(a, b) = a == b },
            cmp3(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord3(A), x : A, y : A) -> Bln = Eq3/eq3(x, y);
        Io/print(Bln/to_str(same(2, 2)))
        "#;

    assert_eq!(run(source), b"true");
}

// A superclass field is anonymous, so its concept's former field name is not a
// label: assigning it is a plain unknown-field error, with no special `use`-field
// diagnostic (`Eql`'s superclass is reached by resolution, never by name).
#[test]
fn labeled_fill_of_a_former_superclass_is_unknown() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str, Order};
        pub concept Eq4(A : Type) : Type {
            eq4(A, A) -> Bln
        }
        pub concept Ord4(A : Type) : Type {
            use Eq4(A),
            cmp4(A, A) -> Order
        }
        satisfy Eq4(Nat) {
            eq4(a, b) = a == b
        }
        let bad : Ord4(Nat) = Ord4 { eq4 = Eq4 { eq4(a, b) = a == b } };
        Io/print("no")
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
        use /std/{Nat, Io, Str};
        pub record Pair : Type { fst : Nat, snd : Nat }
        let p = Pair { use 1, snd = 2 };
        Io/print("no")
        "#;
    assert!(error(non_concept).contains("not a concept"));

    let surplus = r#"
        use /std/{Nat, Bln, Io, Str, Order};
        pub concept Eq5(A : Type) : Type {
            eq5(A, A) -> Bln
        }
        pub concept Ord5(A : Type) : Type {
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
        Io/print("no")
        "#;
    assert!(error(surplus).contains("'use' entr"));
}

// An omitted superclass field inside a *premised* witness resolves through
// the local `use` premise (resolution's local step), not the table: the
// element equality is the premise's, threaded structurally.
#[test]
fn omitted_superclass_resolves_from_a_premise() {
    let source = r#"
        use /std/{Nat, Bln, Io, Str, Order, Lst};
        pub concept Eq6(A : Type) : Type {
            eq6(A, A) -> Bln
        }
        pub concept Ord6(A : Type) : Type {
            use Eq6(A),
            cmp6(A, A) -> Order
        }
        satisfy Eq6(Nat) {
            eq6(a, b) = a == b
        }
        satisfy(@A : Type, use Eq6(A)) Eq6(Lst(A)) {
            eq6(a, b) = Lst/len(a) == Lst/len(b)
        }
        satisfy(@A : Type, use Ord6(A)) Ord6(Lst(A)) {
            cmp6(a, b) = Order/lt()
        }
        satisfy Ord6(Nat) {
            cmp6(a, b) = Order/lt()
        }
        pub let same(@A : Type, use Ord6(A), x : A, y : A) -> Bln = Eq6/eq6(x, y);
        let l : Lst(Nat) = [1, 2];
        Io/print(Bln/to_str(same(l, l)))
        "#;

    assert_eq!(run(source), b"true");
}
