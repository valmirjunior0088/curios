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
        pub witness show_nat : Show(Nat) {
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
        pub witness show_nat : Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        pub witness show_lst(@A : Type, use Show(A)) : Show(Lst(A)) {
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
        pub witness show_nat : Show(Nat) {
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
// local `use Ord(A)` binder's superclass field `w.eql`. The `use Ord(A)` slot
// itself resolves through the table to `ord_nat`.
#[test]
fn superclass_projection_resolves() {
    let source = r#"
        use /std/{Nat, Bln, Order, Io};
        pub concept Eql(A : Type) : Type {
            eql(A, A) -> Bln
        }
        pub concept Ord(A : Type) : Type {
            use eql : Eql(A),
            cmp(A, A) -> Order
        }
        pub witness eql_nat : Eql(Nat) {
            eql(a, b) = a == b
        }
        pub witness ord_nat : Ord(Nat) {
            eql = eql_nat,
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
        pub witness show_nat : Show(Nat) {
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
        pub witness show_nat : Show(Nat) {
            show(n) = Nat/to_str(n)
        }
        pub witness show_nat_again : Show(Nat) {
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
        pub witness into_nat_str : Into(Nat, Str) {
            into(n) = Nat/to_str(n)
        }
        pub witness into_nat_bln : Into(Nat, Bln) {
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
        pub witness into_nat_str : Into(Nat, Str) {
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
        pub witness convert_nat_str : Convert(Nat, Str) {
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
        pub witness convert_nat_str : Convert(Nat, Str) {
            convert(n) = Nat/to_str(n)
        }
        pub witness convert_nat_bln : Convert(Nat, Bln) {
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
        pub witness convert_nat_str : Convert(Nat, Str) {
            convert(n) = Nat/to_str(n)
        }
        pub let ignore(@A : Type, x : A) -> Nat = 9;
        pub let go(use w : Convert(Nat, Str), x : Nat) -> Nat = ignore(Convert/convert(x));
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

// The monadic sugar promised by INSTANCE_ARGUMENTS: `let ! = Monad/bind;`
// sequences through the concept method, each `e!` resolving its own witness.
#[test]
fn monadic_sugar_binds_through_the_concept() {
    let source = r#"
        use /std/{Nat, Io, Str, Option, Monad};
        pub let chain(a : Option(Nat), b : Option(Nat)) -> Option(Nat) =
            let ! = Monad/bind;
            let x = a!;
            let y = b!;
            Monad/pure(Nat/add(x, y));
        Io/print(Nat/to_str(Option/unwrap_or(chain(Option/some(20), Option/some(22)), 0)))
        "#;

    assert_eq!(run(source), b"42");
}

// A higher-kinded superclass: inside the generic function the goal
// `Monad(M)` (M a bound variable) resolves through step 2's superclass
// projection of the local `use MonadPlus(M)` binder.
#[test]
fn higher_kinded_superclass_projects() {
    let source = r#"
        use /std/{Nat, Io, Str, Option, Monad};
        pub concept MonadPlus(M : (Type) -> Type) : Type {
            use monad : Monad(M),
            empty(@A : Type) -> M(A)
        }
        pub witness monad_plus_option : MonadPlus(Option) {
            monad = Monad/monad_option,
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
        pub witness add_point : Add(Point) {
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
