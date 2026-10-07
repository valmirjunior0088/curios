use {
    curios_core::{Free, Global, Intrinsic, Module, Nat, Term},
    curios_text::SYNTAX,
    xboard::{Date, Part, Proof, Status, Ticket, Witness, false_type, refused},
};

/// May this value be taken apart this way, and what does an arm learn? A match, its motive, coverage, inversion and case equations, where a proposition may be eliminated, and that `Io` cannot be.
pub(super) const ELIMINATION: Part = Part {
    name: "elimination",
    tickets: &[Ticket {
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
    }],
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
