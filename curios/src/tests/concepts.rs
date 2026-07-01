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
