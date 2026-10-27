//! The totality walk through a real checker: every child position it reaches, where it descends from an arm, what it grades unread, and a doubling term in its own size.

use {
    super::test_support::{kernel, single_payload},
    curios_abi::{ForeignFunction, WireResults, WireReturn, WireSignature, WireType},
    curios_analysis::{
        Declarations, Invert, PositivityRefusal, group_totality, invert_indices,
        positivity_vectors, test_support::SYNTAX, variance_vectors,
    },
    curios_cert::Kernel,
    curios_core::{
        Apply, Argument, Bang, Carrier, Cases, Field, Free, Global, InductArm, InductType, Infix,
        Instance, InstanceHead, Intrinsic, Level, Many, Match, MatchResult, Matrix, Metavar,
        MetavarId, MetavarOrigin, MetavarSpine, Nat, Pattern, Proj, Rec, Row, Scope, Struct,
        StructEntry, StructType, Subterm, Term, Three, Totality, Transient, Tuple, Two,
        UniverseContext, Universes, Var, Variant,
    },
    curios_num::Natural,
    curios_utilities::{InfixOp, Plicity, Qualifier},
    std::{collections::BTreeMap, slice, sync::Arc},
};

/// The member every probe below plants a call to.
fn planted() -> Free {
    Free::local(1, Some("f"))
}

#[test]
fn a_huge_literal_call_argument_grades_without_expansion() {
    // `rec f : (n: Nat) -> Nat = (n) => f(u64::MAX); f` — grading the literal argument must read the packed spine, not peel it: a unary expansion would loop once per successor, and the value is unbounded by the source that spelled it.
    let mut kernel = Kernel::new(100_000, SYNTAX);
    let f = Free::local(1, Some("f"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let rec = Term::rec(
        vec![(
            f,
            Term::func_type([(n, nat())], nat()),
            Term::func(
                [(n, nat())],
                Term::apply(
                    Term::free_var(&f),
                    [Term::intrinsic(Intrinsic::Nat(Nat::new(u64::MAX)))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    // A constant argument never decreases, so the verdict is `Partial` — promptly.
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// Whether the engine finds a nullary self-call planted at one child position.
///
/// The group is `rec f : Nat = <plant(f)>`, whose member takes no lambda and so has an empty parameter vector: a self-call from it is a 0x0 matrix, idempotent with no diagonal, so the verdict is `Partial` exactly when [`Walk::walk`] reached the planted occurrence and `Total` exactly when it did not. Nothing here needs to be well typed — the engine is a total function of post-zonk terms and types nothing — which is what lets one probe cover every child position of every variant.
fn call_is_seen(body: Term) -> bool {
    let mut kernel = Kernel::new(100_000, SYNTAX);
    let rec = Term::rec(
        vec![(planted(), Term::intrinsic(Intrinsic::NatType), body)],
        Term::free_var(&planted()),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    group_totality(&mut kernel, group) == Ok(Totality::Partial)
}

/// The `index`th placeholder child: a literal no walk reads as anything, distinct from every other so that replacing one names one position.
fn marker(index: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(10_000 + index)))
}

/// The markers minted so far, and which of them the walk is documented not to reach.
#[derive(Default)]
struct Markers {
    minted: usize,
    unvisited: Vec<(Term, &'static str)>,
}

impl Markers {
    /// A child the walk must reach.
    fn visited(&mut self) -> Term {
        self.minted += 1;
        marker(self.minted)
    }

    /// A child on the whitelist, with the mechanism that keeps the walk from it.
    fn unvisited(&mut self, mechanism: &'static str) -> Term {
        let marker = self.visited();
        self.unvisited.push((marker.clone(), mechanism));
        marker
    }
}

/// One term whose every child is a marker, and the markers it was built from.
struct Specimen {
    term: Term,
    markers: Vec<Term>,
}

impl Specimen {
    fn of(markers: &mut Markers, build: impl FnOnce(&mut Markers) -> Term) -> Self {
        let first = markers.minted;
        let term = build(markers);
        let markers = (first + 1..=markers.minted).map(marker).collect();

        Self { term, markers }
    }
}

/// Every form a child can hang from, by the name its specimen is held under.
const FORMS: &[&str] = &[
    "a leaf",
    "a metavariable",
    "a universe instance of a variable",
    "a universe instance of a projected group",
    "a transient",
    "an intrinsic",
    "a foreign call",
    "a lambda",
    "a function type",
    "an application",
    "a tuple type",
    "a tuple",
    "a projection",
    "a nominal type",
    "a constructor",
    "a structure type",
    "a structure literal",
    "a match at a motive",
    "a match at an ambient goal",
    "a boolean match",
    "a dispatch",
    "a nominal match",
    "a `Nat` eliminator",
    "a `Bytes` eliminator",
    "a `List` eliminator",
    "a let",
    "a group",
];

/// The forms a specimen stands for. Wildcard-free at every level a child can hang from, so a new former — a `Subterm` variant, a `Cases` kind, a carrier, a match result, an instance head, a transient — is a compile error here, and the arm it forces is where its specimen is owed.
fn forms(term: &Term) -> Vec<&'static str> {
    match &**term {
        Subterm::Sort(..) | Subterm::Var(_) => vec!["a leaf"],
        Subterm::Metavar(_) => vec!["a metavariable"],
        Subterm::Instance(Instance { head, .. }) => vec![match head {
            InstanceHead::Var(_) => "a universe instance of a variable",
            InstanceHead::RecProj(..) => "a universe instance of a projected group",
        }],
        Subterm::Transient(transient) => vec![match transient {
            Transient::Infix(_)
            | Transient::Bang(_)
            | Transient::Ascribed(_)
            | Transient::Matrix(_) => "a transient",
            Transient::NumLit(_) | Transient::Derive => "a leaf",
        }],
        Subterm::Intrinsic(_) => vec!["an intrinsic"],
        Subterm::Foreign(..) => vec!["a foreign call"],
        Subterm::Func(_) => vec!["a lambda"],
        Subterm::FuncType(_) => vec!["a function type"],
        Subterm::Apply(_) => vec!["an application"],
        Subterm::TupleType(_) => vec!["a tuple type"],
        Subterm::Tuple(_) => vec!["a tuple"],
        Subterm::Proj(_) => vec!["a projection"],
        Subterm::InductType(_) => vec!["a nominal type"],
        Subterm::Variant(_) => vec!["a constructor"],
        Subterm::StructType(_) => vec!["a structure type"],
        Subterm::Struct(_) => vec!["a structure literal"],
        Subterm::Match(Match { result, cases, .. }) => vec![
            match result {
                MatchResult::Family(_) => "a match at a motive",
                MatchResult::Ambient(_) => "a match at an ambient goal",
            },
            match cases {
                Cases::Bool { .. } => "a boolean match",
                Cases::Switch { .. } => "a dispatch",
                Cases::Induct { .. } => "a nominal match",
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat { .. } => "a `Nat` eliminator",
                    Carrier::Bytes { .. } => "a `Bytes` eliminator",
                    Carrier::List { .. } => "a `List` eliminator",
                },
            },
        ],
        Subterm::Let(_) => vec!["a let"],
        Subterm::Rec(_) => vec!["a group"],
    }
}

/// The position-coverage differential: `Walk::walk` visits every child position `Subterm::any_child_term` reports, minus a named whitelist.
///
/// This matters more here than anywhere else on the soundness board because this is the only analysis whose blindness *admits*. Every other one refuses when it cannot see — positivity answers `Mixed` at an out-of-set name, `whnf` goes stuck, inversion derives nothing at a `Prop`-valued position, an under-applied call is graded `Matrix::unknown` — while a call site the walk never visits contributes no edge, and a group with no edges is `Total`. A projected inner group left unwalked is exactly that: `rec f(n) -> False = (rec g(m) -> False = f(m); g)(n)` would close to no call whatsoever, both groups would classify `Total`, and `f(0)` would diverge through `g` while (V) read the verdict.
///
/// The probe needs no instrumentation because the engine types nothing: it is a total function of post-zonk terms, so an ill-typed fixture is a legitimate input. Each row plants a *nullary* self-call at one child position — the member takes no lambda, so a self-call from it is a 0x0 matrix, idempotent with no diagonal — which makes the verdict `Partial` exactly when the walk reached the plant and `Total` exactly when it did not.
///
/// **The rows are the fold's, not a list kept here.** Each specimen is one form with a distinct marker in every child position; the fold is asked for its children, and the call is planted at each child it reports. A list written by hand holds a row for whatever its author thinks of, and passes while every position it forgot has none. Two things are asserted of every specimen before anything is planted: that the fold reports exactly the markers it was built from, so a specimen populating a field the fold does not visit fails here rather than passing unplanted, and that every form in `FORMS` has one.
///
/// **What that does and does not hold.** A term-bearing field added to a form whose specimen is a struct literal is a compile error at that literal, and the marker that repairs it is planted with no further edit. `Func`, `FuncType`, `TupleType`, `Let` and `Rec` are built through `Term`'s constructors, their fields being private, so a field added behind one of those is held only by whoever extends the constructor.
///
/// **The whitelist is three mechanisms.** A `Metavar`'s spine, which the elaborator's strict zonk refuses before this pass runs. A group member's declared type, under both spellings a group is reached by — a nested `Rec` and a universe instance's projection head. And the one the fold cannot show, because it is not a child position but a peel: `Member::of` takes the member body's *leading* lambdas and discards their domain annotations at `Telescope::Cons(_, _, rest)`, so a call planted there is invisible while the same lambda one node deeper is walked — which is why every row is wrapped in a tuple, and why the two spellings are asserted against each other at the end.
///
/// All three are type positions, and the argument that they are safe is the audit's: a call in one is consumed by β or read only by typing, never reduced, and an edge the engine misses is dangerous only where it can complete a reduction cycle. That argument is not what this test checks.
#[test]
fn the_walk_reaches_every_child_position_but_the_three_it_documents() {
    let name = || Global::Authored(Qualifier::from(["N"]));
    let binder = |index: u32| Free::local(500 + u64::from(index), None);
    let call = || Term::free_var(&planted());
    let group_of = |rec: &Term| {
        let Subterm::Rec(Rec { group, .. }) = &**rec else {
            panic!("the fixture changed shape");
        };
        group.clone()
    };
    let matching = |markers: &mut Markers, cases: Cases| -> Term {
        Subterm::Match(Match {
            head: markers.visited(),
            result: MatchResult::Family(Scope::close(Many(1), &[&binder(1)], markers.visited())),
            cases,
        })
        .into()
    };

    let mut markers = Markers::default();
    let specimens = vec![
        Specimen::of(&mut markers, |_| Term::type_ground()),
        Specimen::of(&mut markers, |_| {
            Subterm::Instance(Instance {
                head: InstanceHead::Var(Var::free(binder(7))),
                universes: Universes::default(),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Metavar(Metavar {
                id: MetavarId::from(0usize),
                spine: MetavarSpine::from(vec![markers.unvisited("a metavariable's spine")]),
                origin: MetavarOrigin::Hole,
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let rec = Term::rec(
                vec![(
                    binder(90),
                    markers.unvisited("a projected group's member type"),
                    markers.visited(),
                )],
                Term::free_var(&binder(90)),
            );
            Subterm::Instance(Instance {
                head: InstanceHead::RecProj(group_of(&rec), 0),
                universes: Universes::default(),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Transient(Transient::Infix(Infix {
                op: InfixOp::Add,
                left: markers.visited(),
                right: markers.visited(),
            }))
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Transient(Transient::Bang(Bang {
                action: markers.visited(),
                continuation: markers.visited(),
            }))
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Term::ascribed(markers.visited(), markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Term::matrix(Matrix {
                head: markers.visited(),
                motive: Some(markers.visited()),
                rows: vec![Row {
                    pattern: Pattern::Binder,
                    body: markers.visited(),
                }],
            })
        }),
        Specimen::of(&mut markers, |markers| {
            Term::intrinsic(Intrinsic::ListType(markers.visited()))
        }),
        Specimen::of(&mut markers, |markers| {
            let row = Arc::new(ForeignFunction {
                name: "/planted".to_string(),
                signature: WireSignature {
                    params: Vec::new(),
                    returns: WireReturn::Answers(WireResults::single(WireType::Nat)),
                },
            });
            Term::foreign(
                row,
                Global::Authored(Qualifier::from(["planted"])),
                vec![markers.visited()],
            )
        }),
        Specimen::of(&mut markers, |markers| {
            Term::func([(binder(1), markers.visited())], markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Term::func_type([(binder(1), markers.visited())], markers.visited())
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Apply(Apply {
                head: markers.visited(),
                arguments: vec![Argument {
                    term: markers.visited(),
                    plicity: Plicity::Explicit,
                }],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Term::tuple_type(vec![(binder(1), markers.visited())])
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Tuple(Tuple {
                fields: vec![markers.visited()],
                names: vec![None],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Proj(Proj {
                head: markers.visited(),
                field: Field::Tuple(0),
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::InductType(InductType {
                name: name(),
                universes: Universes::default(),
                params: vec![markers.visited()],
                indices: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Variant(Variant {
                name: name(),
                universes: Universes::default(),
                params: vec![markers.visited()],
                tag: "mk".into(),
                payload: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::StructType(StructType {
                name: name(),
                universes: Universes::default(),
                params: vec![markers.visited()],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Struct(Struct {
                name: name(),
                universes: Universes::default(),
                params: vec![markers.visited()],
                fields: vec![markers.visited()],
                entries: vec![StructEntry::Field(None)],
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let cases = Cases::Bool {
                false_case: markers.visited(),
                true_case: markers.visited(),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            Subterm::Match(Match {
                head: markers.visited(),
                result: MatchResult::Ambient(markers.visited()),
                cases: Cases::Bool {
                    false_case: markers.visited(),
                    true_case: markers.visited(),
                },
            })
            .into()
        }),
        Specimen::of(&mut markers, |markers| {
            let cases = Cases::Switch {
                cases: vec![(Natural::from(0usize), markers.visited())],
                default: markers.visited(),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            let arm = Scope::close(Many(1), &[&binder(2)], markers.visited());
            let cases = Cases::Induct {
                cases: vec![("mk".into(), InductArm::new(arm, vec![Plicity::Explicit]))],
                default: Some(markers.visited()),
                stated: Some(markers.visited()),
            };
            matching(markers, cases)
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::Nat {
                empty_case: markers.visited(),
                cons_case: Scope::close(Two, &[&binder(2), &binder(3)], markers.visited()),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::Bytes {
                empty_case: markers.visited(),
                cons_case: Scope::close(
                    Three,
                    &[&binder(2), &binder(3), &binder(4)],
                    markers.visited(),
                ),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            let carrier = Carrier::List {
                elem: markers.visited(),
                empty_case: markers.visited(),
                cons_case: Scope::close(
                    Three,
                    &[&binder(2), &binder(3), &binder(4)],
                    markers.visited(),
                ),
            };
            matching(markers, Cases::FreeMonoid { carrier })
        }),
        Specimen::of(&mut markers, |markers| {
            Term::let_(
                &binder(1),
                markers.visited(),
                markers.visited(),
                markers.visited(),
            )
        }),
        Specimen::of(&mut markers, |markers| {
            Term::rec(
                vec![(
                    binder(1),
                    markers.unvisited("a nested group's member type"),
                    markers.visited(),
                )],
                markers.visited(),
            )
        }),
    ];

    // The harness itself, first: a bare self-call must be seen and a call-free body must not, or every row below passes for the wrong reason.
    assert!(call_is_seen(call()), "the bare self-call was not seen");
    assert!(
        !call_is_seen(Term::type_ground()),
        "a call-free body was read as recursive"
    );

    let held = specimens
        .iter()
        .flat_map(|specimen| forms(&specimen.term))
        .collect::<Vec<_>>();
    for form in FORMS {
        assert!(held.contains(form), "{form} has no specimen");
    }

    for specimen in &specimens {
        let form = forms(&specimen.term).join(", ");
        let mut children = Vec::new();
        specimen.term.any_child_term(&mut |child| {
            children.push(child.clone());
            false
        });

        // The fold and the specimen agree on what the children are, in both directions: a marker the fold does not report is a field it misses, and a child that is no marker is a position the specimen left unpopulated.
        for marker in &specimen.markers {
            assert!(
                children.contains(marker),
                "{form}: the child fold does not report {marker:?}",
            );
        }
        for child in &children {
            assert!(
                specimen.markers.contains(child),
                "{form}: the specimen leaves the child {child:?} without a marker",
            );
        }

        for child in &children {
            let plant = specimen.term.replace_term(child, &call());
            // A replacement that reached nothing would hold every whitelisted row vacuously.
            assert!(
                plant.mentions_term(&call()),
                "{form}: nothing was planted at {child:?}",
            );

            let mechanism = markers
                .unvisited
                .iter()
                .find(|(marker, _)| marker == child)
                .map(|(_, mechanism)| *mechanism);
            // Wrapped in a tuple so the member body is never itself a lambda: `Member::of` peels a *leading* lambda and discards its domain annotations, which is the third mechanism and has its own pair below, and leaving it in the way would hide what `Walk::step` does with every other position.
            assert_eq!(
                call_is_seen(Term::tuple([plant])),
                mechanism.is_none(),
                "{form}, at the child {child:?}: the walk {} the planted self-call{}",
                if mechanism.is_none() {
                    "missed"
                } else {
                    "reached"
                },
                mechanism.map_or(String::new(), |mechanism| format!(" past {mechanism}")),
            );
        }
    }

    // The variable head is the node's own data rather than a child term, so the fold reports nothing there, but it is still a call position the walk must see.
    let head: Term = Subterm::Instance(Instance {
        head: InstanceHead::Var(Var::free(planted())),
        universes: Universes::default(),
    })
    .into();
    assert!(
        call_is_seen(Term::tuple([head])),
        "a universe instance's variable head: the walk missed the planted self-call",
    );

    // The third mechanism, stated as the pair that separates it from `Walk::step`: the same lambda is invisible at the top of a member body and visited one node deeper.
    let lambda = || Term::func([(binder(1), call())], Term::type_ground());
    assert!(
        !call_is_seen(lambda()),
        "a peeled lambda domain: the walk reached the planted self-call",
    );
    assert!(
        call_is_seen(Term::tuple([lambda()])),
        "a lambda domain below the peel: the walk missed the planted self-call",
    );
}

/// A declaration whose payload type an inline `rec` computes by calling itself: `rec Count : (n : Nat) -> Type = (n) => {Nat, Count(n)}`, planted as `Count(0)`.
///
/// The walk forces the `rec` head because a mutual `induct` group lowers its type constructors into one, then descends into what forcing exposed — and `RecGroup::member_body` substitutes `Term::rec_proj` for every recursive occurrence, so what it descends into holds the same group again. Each turn of the loop opens a fresh binder, so nothing about the term repeats for a memo on terms to catch, and `unfolded` cannot catch it either: it keys on `Free`, and an inline group has no name.
///
/// Written against the kernel rather than as a Curios program because this is where the shared analysis is *the* subject: the elaborator has its own copy of the divergence, but the kernel's is the one on the soundness board, and a program-level fixture exercises the elaborator's first and aborts before reaching this one.
#[test]
fn a_self_calling_type_level_rec_leaves_the_walk_terminating() {
    let mut kernel = kernel();

    let count = Free::local(1, Some("Count"));
    let n = Free::local(2, Some("n"));
    let nat = || Term::intrinsic(Intrinsic::NatType);

    let rec = Term::rec(
        vec![(
            count,
            Term::func_type([(n, nat())], Term::type_ground()),
            Term::func(
                [(n, nat())],
                Term::tuple_type([
                    (Free::local(3, None), nat()),
                    (
                        Free::local(4, None),
                        Term::apply(Term::free_var(&count), [Term::free_var(&n)]),
                    ),
                ]),
            ),
        )],
        Term::free_var(&count),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    let name = Global::Authored(Qualifier::from(["Boxed"]));
    let payload = Term::apply(
        Term::rec_proj(group.clone(), 0),
        [Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)))],
    );
    let mut inducts = BTreeMap::new();
    inducts.insert(name, single_payload(payload, Term::type_ground()));
    for (entry_name, entry) in &inducts {
        kernel
            .declare_induct(entry_name, entry)
            .expect("a name is declared once");
    }

    let vectors = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()))
        .expect("a payload the walk cannot see through is admitted, not refused");
    assert_eq!(vectors.get(&name), Some(&Vec::new()));
}

/// Well-founded recursion's shape, put to the engine under the kernel: `rec f : (a : T) -> Nat = (a) => match a | c(g) => f(g(0)) end`, where `T`'s one constructor stores a function. The call descends on `a` only because `g(0)` reads as the payload `g` it came from — a function-typed payload is a branching node whose children are its applications. The control moves the applied function out of the constructor into a second parameter, `rec f : (a : T, g : (Nat) -> T) -> Nat = (a, g) => f(g(0), g)`, and is `Partial`: `g` is bound by no arm, so nothing relates `g(0)` to `a`, and a rule that read every application of a binder as a decrease would have accepted it.
#[test]
fn an_application_of_a_constructor_payload_descends_only_from_its_arm() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let zero = || Term::intrinsic(Intrinsic::Nat(Nat::new(0usize)));
    let function = || Term::func_type([(Free::local(1, Some("n")), nat())], self_type.clone());
    kernel
        .declare_induct(&name, &single_payload(function(), Term::type_ground()))
        .expect("a name is declared once");

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let g = Free::local(4, Some("g"));
    let applied = || Term::apply(Term::free_var(&g), [zero()]);

    let descending = Term::rec(
        vec![(
            f,
            Term::func_type([(a, self_type.clone())], nat()),
            Term::func(
                [(a, self_type.clone())],
                Term::induct_match(
                    Term::free_var(&a),
                    None,
                    nat(),
                    [("c", vec![g], Term::apply(Term::free_var(&f), [applied()]))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = Term::rec(
        vec![(
            f,
            Term::func_type([(a, self_type.clone()), (g, function())], nat()),
            Term::func(
                [(a, self_type.clone()), (g, function())],
                Term::apply(Term::free_var(&f), [applied(), Term::free_var(&g)]),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// A lambda applied to an arm's payload is graded as its contractum: the call inside reads the payload, which is below the scrutinee. This is the shape a convoy takes — an arm generalized over a hypothesis and applied back to it — and without the rule a proof that descends through one closes to an all-unknown matrix. The control applies the same lambda to an unrelated parameter, which must stay unread: the rule reads a binder as what it stands for, never as a decrease of its own.
#[test]
fn a_lambda_applied_to_the_arm_payload_descends() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    kernel
        .declare_induct(
            &name,
            &single_payload(self_type.clone(), Term::type_ground()),
        )
        .expect("a name is declared once");

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let b = Free::local(4, Some("b"));
    let t = Free::local(5, Some("t"));
    let h = Free::local(6, Some("h"));

    let group_over = |argument: Term| {
        let redex = Term::apply(
            Term::func(
                [(h, self_type.clone())],
                Term::apply(Term::free_var(&f), [Term::free_var(&h), Term::free_var(&b)]),
            ),
            [argument],
        );
        Term::rec(
            vec![(
                f,
                Term::func_type([(a, self_type.clone()), (b, self_type.clone())], nat()),
                Term::func(
                    [(a, self_type.clone()), (b, self_type.clone())],
                    Term::induct_match(Term::free_var(&a), None, nat(), [("c", vec![t], redex)]),
                ),
            )],
            Term::free_var(&f),
        )
    };

    let descending = group_over(Term::free_var(&t));
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = group_over(Term::free_var(&b));
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// The same pair through a `let`: a binder aliasing the payload is below the scrutinee exactly as the payload is, and one aliasing an unrelated parameter is not.
#[test]
fn a_let_alias_of_the_arm_payload_descends() {
    let mut kernel = kernel();

    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    kernel
        .declare_induct(
            &name,
            &single_payload(self_type.clone(), Term::type_ground()),
        )
        .expect("a name is declared once");

    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let b = Free::local(4, Some("b"));
    let t = Free::local(5, Some("t"));
    let h = Free::local(6, Some("h"));

    let group_over = |aliased: Term| {
        let arm = Term::let_(
            &h,
            self_type.clone(),
            aliased,
            Term::apply(Term::free_var(&f), [Term::free_var(&h), Term::free_var(&b)]),
        );
        Term::rec(
            vec![(
                f,
                Term::func_type([(a, self_type.clone()), (b, self_type.clone())], nat()),
                Term::func(
                    [(a, self_type.clone()), (b, self_type.clone())],
                    Term::induct_match(Term::free_var(&a), None, nat(), [("c", vec![t], arm)]),
                ),
            )],
            Term::free_var(&f),
        )
    };

    let descending = group_over(Term::free_var(&t));
    let Subterm::Rec(Rec { group, .. }) = &*descending else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Total));

    let control = group_over(Term::free_var(&b));
    let Subterm::Rec(Rec { group, .. }) = &*control else {
        panic!("the fixture changed shape");
    };
    assert_eq!(group_totality(&mut kernel, group), Ok(Totality::Partial));
}

/// A key at a universe instance is the key the bare variable is. At the top of a position the first direction leaves a key to index refinement, which may have refined it to the very binder it stands against, and a variable under an instance is that key: solved against, the binder would be aliased to what reduces back to it.
///
/// Mutation-checked: with the guard reading a bare variable and a projection alone, the binder is solved to the instance.
#[test]
fn a_key_at_a_universe_instance_is_left_to_index_refinement() {
    let mut kernel = kernel();
    let n = Free::local(900, Some("n"));
    let k = Free::local(901, Some("k"));
    let nat_type = Term::intrinsic(Intrinsic::NatType);
    kernel.assume(&n, &nat_type);
    kernel.assume(&k, &nat_type);
    // At a level, since forcing spells an instance that states none as its bare head.
    let instance: Term = Subterm::Instance(Instance {
        head: InstanceHead::Var(Var::free(n)),
        universes: Universes::levels(vec![Level::zero()]),
    })
    .into();
    let literal = Term::intrinsic(Intrinsic::Nat(Nat::new(3usize)));

    for (label, actual, solved) in [
        ("a bare variable", Term::free_var(&n), None),
        ("a variable at an instance", instance, None),
        ("a literal", literal.clone(), Some(literal)),
    ] {
        let Invert::Solved(solutions) = invert_indices(
            &mut kernel,
            slice::from_ref(&actual),
            &[Term::free_var(&k)],
            slice::from_ref(&k),
        )
        .expect("inversion is a total function of finished terms") else {
            panic!("{label}: a reachable case was reported unreachable");
        };

        let expected = solved
            .map(|value| (k, value))
            .into_iter()
            .collect::<Vec<_>>();
        assert_eq!(solutions, expected, "{label}");
    }
}

/// The walks of the crate that read through a checker, over one doubling term, as `src/walk_tests.rs` holds those that read through a reducer alone: a tuple sixty levels deep, each level holding the one below twice, a tree no walk per path finishes and a graph of sixty-one nodes. A walk the crate adds that reads through a checker joins the table here.
#[test]
fn every_walk_answers_a_doubling_term_in_its_own_size() {
    let x = Free::local(910, Some("x"));
    let z = Free::local(911, Some("z"));
    let doubled = |leaf: Term| {
        (0..60).fold(leaf, |below, _| {
            Term::from(Subterm::Tuple(Tuple {
                fields: vec![below.clone(), below],
                names: vec![None, None],
            }))
        })
    };

    // A family whose one payload is a tuple type of sixty levels, each holding the one below twice: what a field type read by value through a tower of `let`s is.
    let wide = Global::Authored(Qualifier::from(["Wide"]));
    let payload = (0..60).fold(Term::intrinsic(Intrinsic::NatType), |below, _| {
        Term::tuple_type([
            (Free::local(912, Some("a")), below.clone()),
            (Free::local(913, Some("b")), below),
        ])
    });
    let inducts = BTreeMap::from([(wide, single_payload(payload, Term::type_ground()))]);
    let declared = || {
        let mut kernel = kernel();
        kernel
            .declare_induct(&wide, &inducts[&wide])
            .expect("a name is declared once");
        kernel
    };
    let structs = BTreeMap::new();
    let declarations = Declarations::of(&inducts, &structs);

    let rows: Vec<(&str, bool)> = vec![
        (
            "an index inverted against a target of its shape",
            matches!(
                invert_indices(
                    &mut kernel(),
                    &[doubled(Term::free_var(&x))],
                    &[doubled(Term::free_var(&z))],
                    slice::from_ref(&z),
                ),
                Ok(Invert::Solved(solutions)) if solutions == [(z, Term::free_var(&x))]
            ),
        ),
        (
            "positivity over a payload of its shape",
            positivity_vectors(&mut declared(), declarations).is_ok(),
        ),
        (
            "variance over a payload of its shape",
            variance_vectors(&mut declared(), declarations)
                .exhausted
                .is_none(),
        ),
    ];

    for (walk, answered) in rows {
        assert!(answered, "{walk} over a doubling term did not answer");
    }
}

/// A `Bool` the kernel holds as `true`, or as a name with no value, for a match on it to fire or to stand stuck on a neutral.
fn define_guard(kernel: &mut Kernel, guard: &Free, defined: bool) {
    let bool_type = Term::intrinsic(Intrinsic::BoolType);
    match defined {
        true => kernel.define(
            guard,
            &bool_type,
            &Term::intrinsic(Intrinsic::Bool(true)),
            &UniverseContext::default(),
        ),
        false => kernel.declare(guard, &bool_type, &UniverseContext::default()),
    }
    .expect("a name is declared once");
}

/// A payload type reduction cannot fire is read as nothing the walk concludes from: `Bad`'s constructor takes `match g | false => Nat | true => Bad end`, and with `g` a `Bool` the kernel holds no value for, the match is stuck on a neutral and `Env::force` hands it back as it stands. The walk reads a stuck form through its children at `Mixed`, so the self-occurrence in an arm is a non-strict path back and the set is refused by the rule — not for the budget, a stuck reduction being no refusal. The control defines `g` as `true`: the match fires to `Bad`, a strict self-occurrence, and the set is admitted, which says the refusal is the stuck shape's.
#[test]
fn a_payload_stuck_on_a_neutral_is_read_at_mixed_and_refused() {
    let bad_name = Global::Authored(Qualifier::from(["Bad"]));
    let bad_type = Term::induct_type(bad_name, Vec::<Term>::new(), Vec::<Term>::new());
    let guard = Free::local(30, Some("g"));
    let payload = Term::bool_match(
        Term::free_var(&guard),
        None,
        Term::type_ground(),
        Term::intrinsic(Intrinsic::NatType),
        bad_type,
    );

    for defined in [false, true] {
        let mut kernel = kernel();
        define_guard(&mut kernel, &guard, defined);
        let mut inducts = BTreeMap::new();
        inducts.insert(
            bad_name,
            single_payload(payload.clone(), Term::type_ground()),
        );
        for (name, entry) in &inducts {
            kernel
                .declare_induct(name, entry)
                .expect("a name is declared once");
        }

        let verdict = positivity_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()));
        match defined {
            false => assert!(
                matches!(&verdict, Err(PositivityRefusal::NotPositive(refusal)) if refusal.name == bad_name),
                "a match stuck on a neutral, refused by the rule: {verdict:?}",
            ),
            true => assert_eq!(
                verdict
                    .ok()
                    .and_then(|vectors| vectors.get(&bad_name).cloned()),
                Some(Vec::new()),
                "the match fired to a strict self-occurrence is admitted",
            ),
        }
    }
}

/// A call's argument reduction cannot fire is read as no size: `rec f : (a : T) -> Nat = (a) => match a | c(t) => f(match g | false => t | true => t end) end`, both arms of the inner match the payload `t`, and with `g` a `Bool` the kernel holds no value for, the inner match is stuck on a neutral. A reader that took either arm of a stuck match would grade the call below `a`; `Env::force` hands it back as it stands, the reader answers unread, and the group is `Partial`. The control defines `g` as `true`: the match fires to `t`, the payload, and the group is `Total`.
#[test]
fn a_call_argument_stuck_on_a_neutral_grades_unread() {
    let name = Global::Authored(Qualifier::from(["T"]));
    let self_type = Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new());
    let nat = || Term::intrinsic(Intrinsic::NatType);
    let f = Free::local(2, Some("f"));
    let a = Free::local(3, Some("a"));
    let t = Free::local(5, Some("t"));
    let guard = Free::local(30, Some("g"));
    let argument = Term::bool_match(
        Term::free_var(&guard),
        None,
        self_type.clone(),
        Term::free_var(&t),
        Term::free_var(&t),
    );
    let rec = Term::rec(
        vec![(
            f,
            Term::func_type([(a, self_type.clone())], nat()),
            Term::func(
                [(a, self_type.clone())],
                Term::induct_match(
                    Term::free_var(&a),
                    None,
                    nat(),
                    [("c", vec![t], Term::apply(Term::free_var(&f), [argument]))],
                ),
            ),
        )],
        Term::free_var(&f),
    );
    let Subterm::Rec(Rec { group, .. }) = &*rec else {
        panic!("the fixture changed shape");
    };

    for (defined, expected) in [(false, Totality::Partial), (true, Totality::Total)] {
        let mut kernel = kernel();
        kernel
            .declare_induct(
                &name,
                &single_payload(self_type.clone(), Term::type_ground()),
            )
            .expect("a name is declared once");
        define_guard(&mut kernel, &guard, defined);

        assert_eq!(
            group_totality(&mut kernel, group),
            Ok(expected),
            "with `g` {}",
            if defined { "defined" } else { "a neutral" },
        );
    }
}
