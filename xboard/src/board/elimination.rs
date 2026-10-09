use {
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Many, Module, Nat, Nominal, Scope,
        Telescope, Term, UniverseContext,
    },
    curios_text::SYNTAX,
    curios_utilities::{Plicity, Qualifier},
    xboard::{Date, Part, Proof, Status, Ticket, Witness, declared, false_type, refused},
};

/// May this value be taken apart this way, and what does an arm learn? A match, its motive, coverage, inversion and case equations, where a proposition may be eliminated, and that `Io` cannot be.
pub(super) const ELIMINATION: Part = Part {
    name: "elimination",
    tickets: &[
        Ticket {
            title: "An arm of a fold is typed with its scrutinee at the arm's case, and is taken at every tail",
            found_on: Date::new(2026, 10, 7),
            status: Status::Fixed,
            witnesses: &[
                Witness {
                    what: "a fold whose identity arm is a hypothesis about its scrutinee, false at zero",
                    proof: Proof::Program(AN_IDENTITY_ARM_READS_ITS_SCRUTINEE_AT_ZERO),
                    expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
                },
                Witness {
                    what: "a fold whose cons arm reads a hypothesis about its scrutinee beside the one at the tail",
                    proof: Proof::Program(A_CONS_ARM_READS_ITS_SCRUTINEE_AT_A_SUCCESSOR),
                    expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
                },
                Witness {
                    what: "the fold over a call, whose equation is recorded and not substituted",
                    proof: Proof::Program(AN_IDENTITY_ARM_READS_A_CALL_AT_ZERO),
                    expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
                },
                Witness {
                    what: "the fold over a list, whose identity arm reads its length at the empty one",
                    proof: Proof::Program(AN_IDENTITY_ARM_READS_A_LIST_AT_THE_EMPTY_ONE),
                    expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
                },
                Witness {
                    what: "the fold whose identity arm is a hypothesis about its scrutinee, as a term",
                    proof: Proof::Module(an_identity_arm_typed_at_zero),
                    expect: refused![kernel: curios_cert::Error::Mismatch { .. }],
                },
            ],
        },
        Ticket {
            title: "A guard's reduced spelling is settled under one case of a split inside its arm, and answers outside that case",
            found_on: Date::new(2026, 10, 9),
            status: Status::Open,
            witnesses: &[
                Witness {
                    what: "a guard over a match on a proof, read at zero in one arm of a split and answered at a successor in the next",
                    proof: Proof::Program(A_GUARD_SETTLED_AT_ZERO_ANSWERS_AT_A_SUCCESSOR),
                    expect: refused![elaborator],
                },
                Witness {
                    what: "the same guard settled in the false arm of a split and answered in the true one, as a term",
                    proof: Proof::Module(a_guard_settled_under_one_case_of_a_split),
                    expect: refused![kernel],
                },
                Witness {
                    what: "a guard decided by proof irrelevance under one case of a split, and answered after the split",
                    proof: Proof::Program(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS),
                    expect: refused![elaborator],
                },
                Witness {
                    what: "the same guard settled where its operands are proofs and answered where they are not, as a term",
                    proof: Proof::Module(a_guard_settled_where_its_operands_are_proofs),
                    expect: refused![kernel],
                },
                Witness {
                    what: "a guard decided by the unit type's one inhabitant under one case of a split, and answered after the split",
                    proof: Proof::Program(A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS),
                    expect: refused![elaborator: curios_elab::Error::TypeMismatch { .. }],
                },
                Witness {
                    what: "the same guard settled where its operands are units and answered where they are not, as a term",
                    proof: Proof::Module(a_guard_settled_where_its_operands_are_units),
                    expect: refused![kernel: curios_cert::Error::Mismatch { .. }],
                },
            ],
        },
    ],
};

/// The fold at `2` takes its cons arm twice and then its identity arm, where `n` is still `2` and `h` the proof it was handed. The arm was typed with `n` solved to `0`, where `h` proves `Holds(0 == 2)`, which is `False`.
const AN_IDENTITY_ARM_READS_ITS_SCRUTINEE_AT_ZERO: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True, False, Holds};
    let f(n: Nat, h: Holds(n == 2)) -> False =
        match n: (m) => False
        | 0 => h
        | k + 1; ih => ih
        end;
    f(2, True/qed())
    "#;

/// The cons arm is typed with `n` solved to `k + 1`, so `h` says `k + 1` is `2` there while the hypothesis says `k` is `0`, and the two refute each other. Below the outermost step `n` is not `k + 1`, and the hypothesis is the fold at a tail the arm was never typed for.
const A_CONS_ARM_READS_ITS_SCRUTINEE_AT_A_SUCCESSOR: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True, Holds};
    let step(k: Nat, a: Holds(k + 1 == 2), b: Holds(k == 0)) -> Holds(k + 1 == 0) = True/proved();
    let f(n: Nat, h: Holds(n == 2)) -> Holds(n == 0) =
        match n: (m) => Holds(m == 0)
        | 0 => True/qed()
        | k + 1; ih => step(k, h, ih)
        end;
    f(2, True/qed())
    "#;

/// A scrutinee that is no variable is substituted nowhere: the arm is typed under the recorded equation `g(x) = 0`, which reduction consults, and `h` is read through it.
const AN_IDENTITY_ARM_READS_A_CALL_AT_ZERO: &str = r#"
    use /std/{Nat};
    use /std/Bool/{True, False, Holds};
    let f(g: (Nat) -> Nat, x: Nat, h: Holds(g(x) == 2)) -> False =
        match g(x): (m) => False
        | 0 => h
        | k + 1; ih => ih
        end;
    f((y: Nat) => y, 2, True/qed())
    "#;

/// The rule is the free-monoid carriers', so a list's fold has it as `Nat`'s does.
const AN_IDENTITY_ARM_READS_A_LIST_AT_THE_EMPTY_ONE: &str = r#"
    use /std/{Nat, List};
    use /std/Bool/{True, False, Holds};
    let f(xs: List(Nat), h: Holds(List/len(xs) == 2)) -> False =
        match xs: (m) => False
        | [] => h
        | [x, ..t]; ih => ih
        end;
    f([1, 2], True/qed())
    "#;

/// The guard is the match on `p`, which stands while `n` is open: its proof's two indices are one only at a literal, so nothing states `z` before then. Its `true` arm assumes it, and the equation stands for the whole arm. Inside the `0` arm of the split `p` proves `Eq()(0, 0)`, the match is `h(0)`, and the equation's reduced spelling is settled there as `h(0)`, where it is true. The spelling is kept with the equation, which outlives that arm: under `k + 1`, where the guard says `h(1)`, `h(0)` is answered `true` still.
const A_GUARD_SETTLED_AT_ZERO_ANSWERS_AT_A_SUCCESSOR: &str = r#"
    use /std/{Nat, Eq, Bool};
    use /std/Bool/{True, False, Holds};
    let g(n: Nat) -> Nat = match n | 0 => 0 | j + 1 => 1 end;
    let f(
        h: (Nat) -> Bool,
        n: Nat,
        p: Eq()(g(n), g(g(n))),
        guard: Holds(match p: (i, j, q) => Bool | refl(@z) => h(z) end),
        nonzero: Holds(n != 0)) -> Holds(h(0)) =
        match (match p: (i, j, q) => Bool | refl(@z) => h(z) end)
        | false => match guard end
        | true =>
            match n
            | 0 =>
                let settled: Holds(h(0)) = True/qed();
                match nonzero end
            | k + 1 => True/qed()
            end
        end;
    f((m: Nat) => m != 0, 1, Eq/refl(), True/qed(), True/qed())
    "#;

/// `F(c)` is a proposition at `true` and `Nat` at `false`, so `p` and `q` are proofs only under the `true` case of the split, where conversion holds `h(p)` and `h(q)` one term without reading either. The guard is the match on `h(p) == h(q)`, which stands while `c` is open, and its reduced spelling is settled inside that arm, where the comparison is `true` and the match is `k(0)`. The spelling is kept with the guard's equation, which outlives the split: after it, with `c` open again, `k(0)` is answered `true`, and at `false`, where the guard says `k(1)`, that is `Holds(false)`. No match here fires on the type its scrutinee stands at: what the settlement read of the split is whether two locals are proofs.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS: &str = r#"
    use /std/{Nat, Bool};
    use /std/Bool/{True, False, Holds};
    induct Two: Prop | a() | b() end
    let F(c: Bool) -> Type = match c | true => Two | false => Nat end;
    let f(
        c: Bool,
        h: (F(c)) -> Nat,
        k: (Nat) -> Bool,
        p: F(c),
        q: F(c),
        guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
        match (match h(p) == h(q) | true => k(0) | false => k(1) end)
        | false => match guard end
        | true =>
            let split: {} = match c
                | false => ()
                | true => (let settled: Holds(k(0)) = True/qed(); ())
                end;
            True/qed()
        end;
    f(false, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, True/qed())
    "#;

/// [`A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_PROOFS`] with no proposition in it: `F(c)` is the unit type at `true`, where any two inhabitants are one by the type's shape, so the comparison is decided inside that arm as it was there and the guard's spelling is settled the same way. What the settlement read of the split is that a type has one inhabitant, which a proposition is one case of.
const A_GUARD_SETTLED_WHERE_ITS_OPERANDS_ARE_UNITS: &str = r#"
    use /std/{Nat, Bool};
    use /std/Bool/{True, False, Holds};
    let F(c: Bool) -> Type = match c | true => {} | false => Nat end;
    let f(
        c: Bool,
        h: (F(c)) -> Nat,
        k: (Nat) -> Bool,
        p: F(c),
        q: F(c),
        guard: Holds(match h(p) == h(q) | true => k(0) | false => k(1) end)) -> Holds(k(0)) =
        match (match h(p) == h(q) | true => k(0) | false => k(1) end)
        | false => match guard end
        | true =>
            let split: {} = match c
                | false => ()
                | true => (let settled: Holds(k(0)) = True/qed(); ())
                end;
            True/qed()
        end;
    f(false, (n: Nat) => n, (n: Nat) => n == 1, 0, 1, True/qed())
    "#;

/// `((n, h : Holds(n == 2)) => match n : (_) => False | 0 => h | pred + 1; ih => ih end)(2, qed)`.
///
/// The kernel types the identity arm with `n` substituted by `0` through the arm and the locals it re-types, so `h` stands at `Holds(0 == 2)` and inhabits the motive; the cons arm hands its hypothesis back. Nothing the arm learned holds where reduction takes it: at the tail the fold is over `0` and the arm's `n` is `2`.
fn an_identity_arm_typed_at_zero() -> (Module, Term) {
    let n = Free::local(910, Some("n"));
    let h = Free::local(911, Some("h"));
    let pred = Free::local(912, Some("pred"));
    let ih = Free::local(913, Some("ih"));

    let two = || Term::intrinsic(Intrinsic::Nat(Nat::new(2usize)));
    let holds = Term::apply(
        Term::free_var(&Free::global(SYNTAX.proof.holds.qualifier())),
        [Term::intrinsic(Intrinsic::NatEql(
            Term::free_var(&n),
            two(),
        ))],
    );
    let qed = Term::variant(
        Global::Authored(SYNTAX.proof.true_type.qualifier()),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    );

    let fold = Term::nat_match(
        Term::free_var(&n),
        None,
        false_type(),
        Term::free_var(&h),
        &pred,
        &ih,
        Term::free_var(&ih),
    );
    let forged = Term::func([(n, Term::intrinsic(Intrinsic::NatType)), (h, holds)], fold);

    (Module::default(), Term::apply(forged, [two(), qed]))
}

/// `induct Read : (Bool) -> Prop | mk(x : Bool) : (x) end`, and over `g : (Bool) -> Bool`, `b : Bool`, `p : Read(b)`, `guard : Holds(read)` and `chosen : Holds(b)`, with `read` standing for `match p : (_, _) => Bool | mk(x) => g(x) end` and every match at `Holds(g(false))`: `match read | false => match guard end | true => match b | false => match chosen end | true => qed end end`, closed at the identity, `true`, `Read/mk(true)` and two `qed`.
///
/// The `true` arm of the match on `read` assumes `read` is `true`, an equation recorded under `read`, which names `p` and `g` and not `b`. The `false` arm of the split re-types `p` at `Read(false)`, where `read` is `g(false)`, and a stuck form met while that arm is typed settles the equation's reduced spelling there, as `g(false)`, which the equation rightly answers `true` under that case. The settled spelling stays with the equation when the arm is left, so in the `true` arm, where `read` is `g(true)`, `g(false)` is answered `true` as well, and at the identity what the `qed` proves is `Holds(false)`. With the `true` arm of the match on `read` a bare `qed` and no split, the spelling is settled as `g(b)` and the kernel refuses the term.
fn a_guard_settled_under_one_case_of_a_split() -> (Module, Term) {
    let family = Global::Authored(Qualifier::from(["Read"]));
    let bool_type = || Term::intrinsic(Intrinsic::BoolType);
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let at = |index: Term| Term::induct_type(family, Vec::<Term>::new(), [index]);

    let held = Free::local(950, Some("x"));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(951, Some("i")), bool_type())],
            (),
        )),
        constructors: vec![(
            Atom::from("mk"),
            InductParam::new(
                Telescope::build([(held, bool_type())], vec![Term::free_var(&held)]),
                vec![Plicity::Explicit],
            ),
        )],
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let module = Module {
        items: vec![declared(family, Nominal::Inductive(declaration))].into(),
        ..Module::default()
    };

    let truth = Global::Authored(SYNTAX.proof.true_type.qualifier());
    let qed = || Term::variant(truth, Vec::<Term>::new(), "qed", Vec::<Term>::new());
    let holds = |decision: Term| {
        Term::apply(
            Term::free_var(&Free::global(SYNTAX.proof.holds.qualifier())),
            [decision],
        )
    };

    let test = Free::local(952, Some("g"));
    let decided = Free::local(953, Some("b"));
    let proof = Free::local(954, Some("p"));
    let guard = Free::local(955, Some("guard"));
    let chosen = Free::local(956, Some("chosen"));
    let payload = Free::local(957, Some("x"));

    // match p : (_, _) => Bool | mk(x) => g(x) end
    let read = Term::induct_match_scoped_marked(
        Term::free_var(&proof),
        Scope::constant(Many(2), bool_type()),
        [(
            "mk",
            vec![(Plicity::Explicit, payload)],
            Term::apply(Term::free_var(&test), [Term::free_var(&payload)]),
        )],
        None,
    );

    // Holds(g(false)), and a match with no arm on a hypothesis the arm refutes
    let claim = || holds(Term::apply(Term::free_var(&test), [literal(false)]));
    let refuted = |hypothesis: &Free| {
        Term::induct_match(
            Term::free_var(hypothesis),
            None,
            claim(),
            Vec::<(&str, Vec<Free>, Term)>::new(),
        )
    };

    // match b | false => match chosen end | true => qed end
    let split = Term::bool_match(
        Term::free_var(&decided),
        None,
        claim(),
        refuted(&chosen),
        qed(),
    );

    let forged = Term::func(
        [
            (
                test,
                Term::func_type([(Free::local(958, Some("c")), bool_type())], bool_type()),
            ),
            (decided, bool_type()),
            (proof, at(Term::free_var(&decided))),
            (guard, holds(read.clone())),
            (chosen, holds(Term::free_var(&decided))),
        ],
        Term::bool_match(read, None, claim(), refuted(&guard), split),
    );

    let same = Free::local(959, Some("c"));
    let identity = Term::func([(same, bool_type())], Term::free_var(&same));

    (
        module,
        Term::apply(
            forged,
            [
                identity,
                literal(true),
                Term::variant(family, Vec::<Term>::new(), "mk", [literal(true)]),
                qed(),
                qed(),
            ],
        ),
    )
}

/// `induct Two : Prop | a | b end`, and over `c : Bool`, `h : (F(c)) -> Nat`, `k : (Nat) -> Bool`, `p, q : F(c)` and `guard : Holds(read)`, with `F(c)` standing for `match c : (_) => Type | false => Nat | true => Two end` and `read` for `match h(p) == h(q) : (_) => Bool | false => k(1) | true => k(0) end`: `match read : (_) => Holds(k(0)) | false => match guard end | true => let w : True = match c : (_) => True | false => qed | true => let u : Holds(k(0)) = qed; qed end; qed end`, closed at `false`, the identity, `(n) => n == 1`, `0`, `1` and `qed`.
///
/// The `true` arm of the match on `read` assumes `read` is `true`, an equation recorded under `read`, which names `h`, `p`, `q` and `k` and not `c`. The `true` arm of the split re-types `h`, `p` and `q` at `Two`, where conversion holds `h(p)` and `h(q)` one term, the comparison folds to `true` and `read` is `k(0)`: the `qed` that arm binds at `Holds(k(0))` settles the equation's reduced spelling there, as `k(0)`. The settled spelling stays with the equation when the split is left, so the `qed` after it, typed with `c` open, is checked at a `Holds(k(0))` that still answers `True`, and at `false`, where `read` is `k(1)`, what it proves is `Holds(false)`. With the `true` arm of the split a bare `qed`, the spelling is settled with `c` open, as `read` stands there, and the kernel refuses the term.
fn a_guard_settled_where_its_operands_are_proofs() -> (Module, Term) {
    let two = Global::Authored(Qualifier::from(["Two"]));

    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::done(())),
        constructors: ["a", "b"]
            .iter()
            .map(|tag| {
                (
                    Atom::from(*tag),
                    InductParam::new(Telescope::done(Vec::new()), Vec::new()),
                )
            })
            .collect(),
        result_sort: Term::prop(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    let module = Module {
        items: vec![declared(two, Nominal::Inductive(declaration))].into(),
        ..Module::default()
    };

    a_guard_settled_where_its_operands_are_one(module, &|| {
        Term::induct_type(two, Vec::<Term>::new(), Vec::<Term>::new())
    })
}

/// [`a_guard_settled_where_its_operands_are_proofs`] with the unit type `{}` where `Two` stood, and no declaration: at `true` the operands' type has one inhabitant by its shape, and conversion holds `h(p)` and `h(q)` one term there as it does at a proposition.
fn a_guard_settled_where_its_operands_are_units() -> (Module, Term) {
    a_guard_settled_where_its_operands_are_one(Module::default(), &Term::tuple_type_unit)
}

/// The term both of those close with, over `module`, with `single` the type `F(c)` is at `true`.
fn a_guard_settled_where_its_operands_are_one(
    module: Module,
    single: &dyn Fn() -> Term,
) -> (Module, Term) {
    let bool_type = || Term::intrinsic(Intrinsic::BoolType);
    let nat_type = || Term::intrinsic(Intrinsic::NatType);
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let nat = |value: usize| Term::intrinsic(Intrinsic::Nat(Nat::new(value)));

    let truth = Global::Authored(SYNTAX.proof.true_type.qualifier());
    let true_type = || Term::induct_type(truth, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = || Term::variant(truth, Vec::<Term>::new(), "qed", Vec::<Term>::new());
    let holds = |decision: Term| {
        Term::apply(
            Term::free_var(&Free::global(SYNTAX.proof.holds.qualifier())),
            [decision],
        )
    };

    let decided = Free::local(970, Some("c"));
    let measure = Free::local(971, Some("h"));
    let test = Free::local(972, Some("k"));
    let left = Free::local(973, Some("p"));
    let right = Free::local(974, Some("q"));
    let guard = Free::local(975, Some("guard"));

    // match c : (_) => Type | false => Nat | true => single end
    let carried = || {
        Term::bool_match(
            Term::free_var(&decided),
            None,
            Term::type_ground(),
            nat_type(),
            single(),
        )
    };
    let measured =
        |operand: &Free| Term::apply(Term::free_var(&measure), [Term::free_var(operand)]);
    let tested = |at: usize| Term::apply(Term::free_var(&test), [nat(at)]);

    // match h(p) == h(q) : (_) => Bool | false => k(1) | true => k(0) end
    let read = Term::bool_match(
        Term::intrinsic(Intrinsic::NatEql(measured(&left), measured(&right))),
        None,
        bool_type(),
        tested(1),
        tested(0),
    );
    let claim = || holds(tested(0));

    // match c : (_) => True | false => qed | true => let u : Holds(k(0)) = qed; qed end
    let split = Term::bool_match(
        Term::free_var(&decided),
        None,
        true_type(),
        qed(),
        Term::let_(&Free::local(976, Some("u")), claim(), qed(), qed()),
    );

    // let w : True = split; qed
    let after = Term::let_(&Free::local(977, Some("w")), true_type(), split, qed());

    let forged = Term::func(
        [
            (decided, bool_type()),
            (
                measure,
                Term::func_type([(Free::local(978, Some("x")), carried())], nat_type()),
            ),
            (
                test,
                Term::func_type([(Free::local(979, Some("n")), nat_type())], bool_type()),
            ),
            (left, carried()),
            (right, carried()),
            (guard, holds(read.clone())),
        ],
        Term::bool_match(
            read,
            None,
            claim(),
            Term::induct_match(
                Term::free_var(&guard),
                None,
                claim(),
                Vec::<(&str, Vec<Free>, Term)>::new(),
            ),
            after,
        ),
    );

    let counted = Free::local(980, Some("n"));
    let identity = Term::func([(counted, nat_type())], Term::free_var(&counted));
    let is_one = Term::func(
        [(counted, nat_type())],
        Term::intrinsic(Intrinsic::NatEql(Term::free_var(&counted), nat(1))),
    );

    (
        module,
        Term::apply(
            forged,
            [literal(false), identity, is_one, nat(0), nat(1), qed()],
        ),
    )
}
