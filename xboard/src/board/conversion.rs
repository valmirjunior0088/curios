use {
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Many, Module, Nominal, Scope,
        Telescope, Term, UniverseContext,
    },
    curios_text::SYNTAX,
    curios_utilities::{Plicity, Qualifier},
    xboard::{Date, Part, Proof, Status, Ticket, Witness, declared, refused},
};

/// Are these two terms equal? Definitional equality and everything it believes about computation: the intrinsics' folds and algebra, the bounds oracle, the binary64 model, the closed machine, the evaluation memo.
pub(super) const CONVERSION: Part = Part {
    name: "conversion",
    tickets: &[Ticket {
        title: "A reduct read off a local's type outlives the arm that re-typed the local",
        found_on: Date::new(2026, 10, 9),
        status: Status::Open,
        witnesses: &[Witness {
            what: "a match on a proof fired inside an arm at the payload the arm's case states, and answered again after the arm",
            proof: Proof::Module(a_reduct_read_under_an_arms_retyping),
            expect: refused![kernel],
        }],
    }],
};

/// `induct Read : (Bool) -> Prop | mk(x : Bool) : (x) end`, and over `b : Bool` and `p : Read(b)`, with `read` standing for `match p : (_, _) => Bool | mk(x) => x end`: `((w : True) => (k : Holds(read)) => k)(match b : (_) => True | false => qed | true => ((h : Holds(read)) => qed)(qed) end)(qed)`, closed at `false` and `Read/mk(false)`.
///
/// A match on a proposition whose indices pin its payload fires on the type its scrutinee stands at, so `read` is `b` wherever `p : Read(b)`. The `true` arm of the match on `b` re-types `p` at `Read(true)`, where `read` is `true` and `Holds(read)` is `True`, and the kernel remembers that reduct under `read`, which names `p` and not `b`. Leaving the arm puts `p` back at `Read(b)` and drops no case equation, and the reducts remembered of terms naming a local are cleared on the way out only where one is dropped. So the `qed` after the match is checked at a `Holds(read)` that still answers `True`, for every `b`, and at `false` what it proves is `Holds(false)`. With the `true` arm a bare `qed`, nothing reduces `read` inside it and the kernel refuses the term.
fn a_reduct_read_under_an_arms_retyping() -> (Module, Term) {
    let family = Global::Authored(Qualifier::from(["Read"]));
    let bool_type = || Term::intrinsic(Intrinsic::BoolType);
    let literal = |value: bool| Term::intrinsic(Intrinsic::Bool(value));
    let at = |index: Term| Term::induct_type(family, Vec::<Term>::new(), [index]);

    let held = Free::local(940, Some("x"));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(941, Some("i")), bool_type())],
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
    let true_type = || Term::induct_type(truth, Vec::<Term>::new(), Vec::<Term>::new());
    let qed = || Term::variant(truth, Vec::<Term>::new(), "qed", Vec::<Term>::new());

    let decided = Free::local(942, Some("b"));
    let proof = Free::local(943, Some("p"));
    let payload = Free::local(944, Some("x"));

    // match p : (_, _) => Bool | mk(x) => x end
    let read = Term::induct_match_scoped_marked(
        Term::free_var(&proof),
        Scope::constant(Many(2), bool_type()),
        [(
            "mk",
            vec![(Plicity::Explicit, payload)],
            Term::free_var(&payload),
        )],
        None,
    );
    let holds_read = || {
        Term::apply(
            Term::free_var(&Free::global(SYNTAX.proof.holds.qualifier())),
            [read.clone()],
        )
    };

    // match b : (_) => True | false => qed | true => ((h : Holds(read)) => qed)(qed) end
    let warmed = Term::bool_match(
        Term::free_var(&decided),
        None,
        true_type(),
        qed(),
        Term::apply(
            Term::func([(Free::local(945, Some("h")), holds_read())], qed()),
            [qed()],
        ),
    );

    // (w : True) => (k : Holds(read)) => k
    let stated = Free::local(946, Some("k"));
    let after = Term::func(
        [(Free::local(947, Some("w")), true_type())],
        Term::func([(stated, holds_read())], Term::free_var(&stated)),
    );

    let forged = Term::func(
        [
            (decided, bool_type()),
            (proof, at(Term::free_var(&decided))),
        ],
        Term::apply(Term::apply(after, [warmed]), [qed()]),
    );

    (
        module,
        Term::apply(
            forged,
            [
                literal(false),
                Term::variant(family, Vec::<Term>::new(), "mk", [literal(false)]),
            ],
        ),
    )
}
