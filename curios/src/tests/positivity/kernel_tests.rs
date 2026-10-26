//! A family nested in itself through an intrinsic carrier, built by hand and put to the kernel with the prelude in scope: the elaborator refuses each of these declarations before a module exists, so nothing else hands the kernel's own reading of a carrier's parameter a program to refuse.

use {
    curios_cert::{Error, Verdict},
    curios_core::{
        Definition, Entrypoint, Free, Global, InductDecl, InductParam, Intrinsic, Item, Level,
        Many, Module, Nat, Nominal, Program, RecItem, Scope, StructDecl, StructType, Subterm,
        Telescope, Term, UniverseContext, Universes, Zonked,
    },
    curios_pipeline::{DEFAULT_BUDGET, recheck_with_prelude},
    curios_text::SYNTAX,
    curios_utilities::{Plicity, Qualifier},
};

fn bad() -> Global {
    Global::Authored(Qualifier::from(["Bad"]))
}

fn sink() -> Global {
    Global::Authored(Qualifier::from(["Sink"]))
}

fn false_type() -> Term {
    Term::free_var(&Free::global(SYNTAX.proof.false_type.qualifier()))
}

fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

fn nat(value: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(value)))
}

/// `Holds(0 < count)`.
fn positive(count: Term) -> Term {
    Term::apply(
        Term::free_var(&Free::global(SYNTAX.proof.holds.qualifier())),
        [Term::intrinsic(Intrinsic::NatLt(nat(0), count))],
    )
}

fn qed() -> Term {
    Term::variant(
        Global::Authored(SYNTAX.proof.true_type.qualifier()),
        Vec::<Term>::new(),
        "qed",
        Vec::<Term>::new(),
    )
}

/// `(x: of) -> False`.
fn refutation(of: Term) -> Term {
    Term::func_type([(Free::local(950, Some("x")), of)], false_type())
}

/// `{m: Nat, x: Bad(m), h: Holds(0 < m)}`: a value of the family at an index that says the list it was built from is not empty.
fn witness_type() -> Term {
    let m = Free::local(951, Some("m"));

    Term::tuple_type([
        (m, nat_type()),
        (
            Free::local(952, Some("x")),
            Term::induct_type(bad(), Vec::<Term>::new(), [Term::free_var(&m)]),
        ),
        (Free::local(953, Some("h")), positive(Term::free_var(&m))),
    ])
}

/// What the list holds: `(x: {m: Nat, x: Bad(m), h: Holds(0 < m)}) -> False`.
fn element() -> Term {
    refutation(witness_type())
}

fn length(list: Term) -> Term {
    Term::intrinsic(Intrinsic::ListLen {
        element: element(),
        list,
    })
}

/// `induct Bad : (n: Nat) -> Type | c(payload) : Bad(len(list(payload))) end`: the length of the list a value is built from is its index, which the constructor's target states and a payload never does.
fn family(payload: (Free, Term), list: impl Fn(Term) -> Term) -> Item {
    let target = length(list(Term::free_var(&payload.0)));
    let declaration = InductDecl {
        universe_context: UniverseContext::default(),
        arity: Telescope::done(Telescope::build(
            [(Free::local(954, Some("n")), nat_type())],
            (),
        )),
        constructors: vec![(
            "c".into(),
            InductParam::new(
                Telescope::build([payload], vec![target]),
                vec![Plicity::Explicit],
            ),
        )]
        .into(),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };

    Item::Rec(RecItem::new(vec![Definition::former(
        bad(),
        Qualifier::default(),
        Nominal::Inductive(declaration),
    )]))
}

/// `w = (p) => (match p.x : (k, _) => (Holds(0 < k)) -> False | c(payload) => (h) => get(list(payload), 0, h)(p) end)(p.h)`, and `w(1, c(build([w])), qed)`: Curry's paradox read through the head of a list, the head there because the index says so.
fn curried(list: impl Fn(Term) -> Term, build: impl Fn(Term) -> Term) -> Term {
    let p = Free::local(955, Some("p"));
    let k = Free::local(956, Some("k"));
    let y = Free::local(957, Some("y"));
    let payload = Free::local(958, Some("payload"));
    let h = Free::local(959, Some("h"));

    let motive = Scope::close(
        Many(2),
        &[&k, &y],
        Term::func_type([(h, positive(Term::free_var(&k)))], false_type()),
    );
    let read = list(Term::free_var(&payload));
    let arm = Term::func(
        [(h, positive(length(read.clone())))],
        Term::apply(
            Term::intrinsic(Intrinsic::ListGet {
                element: element(),
                list: read,
                index: nat(0),
                in_range: Term::free_var(&h),
            }),
            [Term::free_var(&p)],
        ),
    );
    let opened = Term::induct_match_scoped_marked(
        Term::proj(Term::free_var(&p), 1),
        motive,
        [("c", vec![(Plicity::Explicit, payload)], arm)],
        None,
    );
    let open = Term::func(
        [(p, witness_type())],
        Term::apply(opened, [Term::proj(Term::free_var(&p), 2)]),
    );

    let refuting = Term::intrinsic(Intrinsic::List {
        element: element(),
        items: vec![open.clone()],
    });
    let built = Term::variant(bad(), Vec::<Term>::new(), "c", [build(refuting)]);

    Term::apply(open, [Term::tuple([nat(1), built, qed()])])
}

/// What the kernel says of `module` closed with `body` at `False`.
fn verdicts(module: Module, body: Term) -> Vec<Verdict> {
    let program = Program {
        module,
        entry: Entrypoint {
            body,
            type_: Some(false_type()),
        },
    };
    let program = Zonked::project(&program).expect("a module built by hand is zonked");

    recheck_with_prelude(&program, DEFAULT_BUDGET)
}

/// Whether `verdicts` is the one refusal positivity makes of `Bad`, and nothing else: the term it closes with is a proof of `False` the declaration alone stands between.
fn only_bad_is_refused_as_not_positive(verdicts: &[Verdict]) -> bool {
    matches!(
        verdicts,
        [Verdict { name: Some(name), error: Error::NotPositive { .. } }] if *name == bad()
    )
}

// `induct Bad : (n: Nat) -> Type | c(fs: List((p: {m: Nat, x: Bad(m), h: Holds(0 < m)}) -> False)) : Bad(len(fs)) end`, closed with `w(1, c([w]), qed)`: `Bad` reaches itself only through the list, its index carrying the length a read at the head needs, and the term it closes with types at `False` once the family is admitted. `List` is covariant, so the arrow inside it counts where it stands, and the kernel's refusal of the family is the only one.
//
// Mutation-checked at `curios-analysis`'s positivity walk: reading nothing through `ListType` admits the module whole.
#[test]
fn curry_through_a_list_is_refused_at_the_family_alone() {
    let fs = Free::local(960, Some("fs"));
    let module = Module {
        items: vec![family(
            (fs, Term::intrinsic(Intrinsic::ListType(element()))),
            |payload| payload,
        )]
        .into(),
        ..Module::default()
    };

    let verdicts = verdicts(module, curried(|payload| payload, |list| list));
    assert!(
        only_bad_is_refused_as_not_positive(&verdicts),
        "{verdicts:?}"
    );
}

// The formation ticket's instance, under a carrier: `struct Sink(A) { fs: List((x: A) -> False) }` names its parameter as an instance of it inside the list, and `Bad` holds a `Sink` of its own witnesses, its index the length of the sink's list. The parameter is an occurrence wherever the walk meets it, under a carrier as under an arrow, so `Sink`'s vector is negative and the kernel's refusal of `Bad` is the only one.
//
// Mutation-checked as the test above is.
#[test]
fn curry_through_a_list_behind_a_parameter_named_by_an_instance_is_refused_at_the_family_alone() {
    let parameter = Free::local(961, Some("A"));
    let declaration = StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build(
            [(parameter, Term::type_ground())],
            Telescope::build(
                [(
                    Free::local(962, Some("fs")),
                    Term::intrinsic(Intrinsic::ListType(refutation(Term::instance_of(
                        &parameter,
                        vec![Level::zero()],
                    )))),
                )],
                (),
            ),
        ),
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let held: Term = Subterm::StructType(StructType {
        name: sink(),
        universes: Universes::default(),
        params: vec![witness_type()],
    })
    .into();
    let read = |payload| Term::proj_struct(payload, sink(), 0);
    let module = Module {
        items: vec![
            Item::Let(Box::new(Definition::former(
                sink(),
                Qualifier::default(),
                Nominal::Struct(declaration),
            ))),
            family((Free::local(963, Some("s")), held), read),
        ]
        .into(),
        ..Module::default()
    };

    let verdicts = verdicts(
        module,
        curried(read, |list| Term::struct_(sink(), [witness_type()], [list])),
    );
    assert!(
        only_bad_is_refused_as_not_positive(&verdicts),
        "{verdicts:?}"
    );
}

// A record held by a carrier no value hands it back through: `struct Bad { held: Io((Bad) -> False) }`, a description of its own refutation, and `struct Bad { held: Cell(Bad) }` and `{ held: Channel(Bad) }`, which hold it as a plain payload would. `Io` is covariant, so the arrow inside it counts where it stands, and a cell or a channel is read and written, so either is invariant: each is refused, the shapes a carrier with an eliminator would turn into Curry's paradox.
#[test]
fn a_record_held_by_an_effectful_carrier_is_refused() {
    let bad_type: Term = Subterm::StructType(StructType {
        name: bad(),
        universes: Universes::default(),
        params: Vec::new(),
    })
    .into();
    let carriers: [(&str, Term); 3] = [
        (
            "a description of its refutation",
            Term::intrinsic(Intrinsic::io_type(refutation(bad_type.clone()))),
        ),
        (
            "a cell",
            Term::intrinsic(Intrinsic::CellType(bad_type.clone())),
        ),
        (
            "a channel",
            Term::intrinsic(Intrinsic::ChannelType(bad_type)),
        ),
    ];

    for (carrier, held) in carriers {
        let declaration = StructDecl {
            universe_context: UniverseContext::empty(),
            arity: Telescope::done(Telescope::build(
                [(Free::local(964, Some("held")), held)],
                (),
            )),
            result_sort: Term::type_ground(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        };
        let module = Module {
            items: vec![Item::Let(Box::new(Definition::former(
                bad(),
                Qualifier::default(),
                Nominal::Struct(declaration),
            )))]
            .into(),
            ..Module::default()
        };
        let verdicts = verdicts(module, false_type());

        assert!(
            verdicts.iter().any(|verdict| verdict.name == Some(bad())
                && matches!(verdict.error, Error::NotPositive { .. })),
            "a record held in {carrier} was not refused as not positive: {verdicts:?}",
        );
    }
}
