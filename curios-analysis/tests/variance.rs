//! The variance analysis, driven through the kernel.
//!
//! An integration test for the reason `driven.rs` gives: the analysis is written against `Env`, and the only implementations sit above this crate. A family here is built by hand at its universe parameters, declared to the kernel and put to `variance_vectors`; a vector is spelled with `*` for an irrelevant level and `=` for an invariant one.

use {
    curios_analysis::{Declarations, test_support::SYNTAX, variance_vectors},
    curios_cert::Kernel,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, StructDecl, Telescope, Term,
        UniverseContext, UniverseParam, Variance,
    },
    curios_utilities::{Plicity, Qualifier},
    std::collections::BTreeMap,
};

fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
}

fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from([path]))
}

fn level(index: usize) -> Level {
    Level::param(UniverseParam(index))
}

fn scheme(parameter_count: usize) -> UniverseContext {
    UniverseContext {
        parameter_count,
        constraints: Vec::new(),
    }
}

fn nat() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// An instance of the family `name` at `levels`, applied to `params` and no index.
fn at(name: &str, levels: &[Level], params: Vec<Term>) -> Term {
    Term::induct_type_at(named(name), levels.to_vec(), params, Vec::<Term>::new())
}

/// A family over `levels` universe parameters: one type parameter per entry of `params`, one index per entry of `indices`, and one constructor, `c`, holding `payloads` and targeting `targets`. A payload is built from the parameters' binders, so it names them free.
fn family(
    levels: usize,
    params: Vec<Term>,
    indices: Vec<Term>,
    payloads: impl FnOnce(&[Free]) -> Vec<Term>,
    targets: Vec<Term>,
) -> InductDecl {
    let binders = (100u32..)
        .take(params.len())
        .map(|index| Free::local(index, Some("A")))
        .collect::<Vec<_>>();
    let payloads = payloads(&binders);
    let typed = binders.iter().copied().zip(params).collect::<Vec<_>>();
    let indexed = (300u32..)
        .zip(indices)
        .map(|(index, type_)| (Free::local(index, Some("i")), type_));
    let held = (200u32..)
        .zip(payloads.iter().cloned())
        .map(|(index, type_)| (Free::local(index, Some("x")), type_));
    let plicities = binders
        .iter()
        .map(|_| Plicity::Implicit)
        .chain(payloads.iter().map(|_| Plicity::Explicit))
        .collect();

    InductDecl {
        universe_context: scheme(levels),
        arity: Telescope::build(typed.clone(), Telescope::build(indexed, ())),
        constructors: vec![(
            Atom::from("c"),
            InductParam::new(
                Telescope::build(typed.into_iter().chain(held), targets),
                plicities,
            ),
        )],
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    }
}

fn spelled(vector: &[Variance]) -> String {
    vector
        .iter()
        .map(|variance| match variance {
            Variance::Irrelevant => '*',
            Variance::Invariant => '=',
        })
        .collect()
}

/// Each declared family's vector, the families declared to `kernel` first.
fn vectors(
    kernel: &mut Kernel,
    inducts: &BTreeMap<Global, InductDecl>,
) -> BTreeMap<Global, String> {
    for (name, entry) in inducts {
        kernel.declare_induct(name, entry);
    }

    variance_vectors(kernel, Declarations::of(inducts, &BTreeMap::new()))
        .vectors
        .iter()
        .map(|(name, vector)| (*name, spelled(vector)))
        .collect()
}

/// One family's vector, analyzed alone.
fn vector(name: &str, declaration: InductDecl) -> String {
    let inducts = BTreeMap::from([(named(name), declaration)]);

    vectors(&mut kernel(), &inducts)[&named(name)].clone()
}

/// `Wrap.{u}(A: Type u) | c(x: A)`: a level only a parameter's type mentions.
fn wrap() -> InductDecl {
    family(
        1,
        vec![Term::type_at(level(0))],
        Vec::new(),
        |params| vec![Term::free_var(&params[0])],
        Vec::new(),
    )
}

/// `Box.{u} | c(x: Type u)`: a level a payload's `Type` mentions.
fn boxed() -> InductDecl {
    family(
        1,
        Vec::new(),
        Vec::new(),
        |_| vec![Term::type_at(level(0))],
        Vec::new(),
    )
}

/// `Uses.{u}(A: Type u) | c(x: head.{u}(A))`: a level a payload mentions only on an instance of `head`.
fn uses(head: &Free) -> InductDecl {
    family(
        1,
        vec![Term::type_at(level(0))],
        Vec::new(),
        |params| {
            vec![Term::apply(
                Term::instance_of(head, vec![level(0)]),
                [Term::free_var(&params[0])],
            )]
        },
        Vec::new(),
    )
}

#[test]
fn a_level_only_a_parameters_type_mentions_is_irrelevant() {
    assert_eq!(vector("Wrap", wrap()), "*");
}

// `Over.{u, v}(M: (Type u) -> Type v) | c(x: M(Nat))`: both levels type the parameter and nothing else, a function type being a parameter's type like any other.
#[test]
fn a_parameters_type_is_unread_whatever_its_shape() {
    let over = family(
        2,
        vec![Term::func_type(
            [(Free::local(1, Some("T")), Term::type_at(level(0)))],
            Term::type_at(level(1)),
        )],
        Vec::new(),
        |params| vec![Term::apply(Term::free_var(&params[0]), [nat()])],
        Vec::new(),
    );

    assert_eq!(vector("Over", over), "**");
}

#[test]
fn a_level_a_payloads_type_mentions_is_invariant() {
    assert_eq!(vector("Box", boxed()), "=");
}

// `Over.{u}: (T: Type u) -> Type`, with no constructor: two instances apart in `u` are families over different index types.
#[test]
fn a_level_an_index_type_mentions_is_invariant() {
    let over = InductDecl {
        constructors: Vec::new(),
        ..family(
            1,
            Vec::new(),
            vec![Term::type_at(level(0))],
            |_| Vec::new(),
            vec![nat()],
        )
    };

    assert_eq!(vector("Over", over), "=");
}

// `Fam.{u}: (D: Type 2) -> Type | c(): (Box.{u})`: no payload and no index type mentions `u`, and the target puts it at `Box`'s invariant position. The control targets `Nat`.
#[test]
fn a_level_only_an_index_target_mentions_is_invariant() {
    let two = Level::constant(2);
    let fam = |target: Term| {
        family(
            1,
            Vec::new(),
            vec![Term::type_at(two.clone())],
            |_| Vec::new(),
            vec![target],
        )
    };
    let inducts = BTreeMap::from([
        (named("Box"), boxed()),
        (named("Fam"), fam(at("Box", &[level(0)], Vec::new()))),
        (named("Control"), fam(nat())),
    ]);
    let vectors = vectors(&mut kernel(), &inducts);

    assert_eq!(vectors[&named("Fam")], "=");
    assert_eq!(vectors[&named("Control")], "*");
}

// A struct's fields are read as a constructor's payloads are: `Holds.{u} { Type u }` against `Keeps.{u}(A: Type u) { A }`.
#[test]
fn a_structs_field_is_read() {
    let carrier = Free::local(1, Some("A"));
    let field = Free::local(2, Some("x"));
    let declaration = |arity| StructDecl {
        universe_context: scheme(1),
        arity,
        result_sort: Term::type_ground(),
        module: Qualifier::default(),
        rep_public: true,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    let structs = BTreeMap::from([
        (
            named("Holds"),
            declaration(Telescope::done(Telescope::build(
                [(field, Term::type_at(level(0)))],
                (),
            ))),
        ),
        (
            named("Keeps"),
            declaration(Telescope::build(
                [(carrier, Term::type_at(level(0)))],
                Telescope::build([(field, Term::free_var(&carrier))], ()),
            )),
        ),
    ]);
    let mut kernel = kernel();
    for (name, entry) in &structs {
        kernel.declare_struct(name, entry);
    }
    let found = variance_vectors(&mut kernel, Declarations::of(&BTreeMap::new(), &structs)).vectors;

    assert_eq!(spelled(&found[&named("Holds")]), "=");
    assert_eq!(spelled(&found[&named("Keeps")]), "*");
}

// `Tree.{u}(A: Type u) | c(x: A, rest: Wrap.{u}(Tree.{u}(A)))` reaches `Wrap` and itself at positions each calls irrelevant, and stays irrelevant. `Held.{u} | c(x: Box.{u})` reaches `Box` at its invariant position.
#[test]
fn a_level_composes_through_the_families_it_reaches() {
    let tree = family(
        1,
        vec![Term::type_at(level(0))],
        Vec::new(),
        |params| {
            let element = Term::free_var(&params[0]);
            let itself = at("Tree", &[level(0)], vec![element.clone()]);

            vec![element, at("Wrap", &[level(0)], vec![itself])]
        },
        Vec::new(),
    );
    let held = family(
        1,
        Vec::new(),
        Vec::new(),
        |_| vec![at("Box", &[level(0)], Vec::new())],
        Vec::new(),
    );
    let inducts = BTreeMap::from([
        (named("Wrap"), wrap()),
        (named("Box"), boxed()),
        (named("Tree"), tree),
        (named("Held"), held),
    ]);
    let vectors = vectors(&mut kernel(), &inducts);

    assert_eq!(vectors[&named("Tree")], "*");
    assert_eq!(vectors[&named("Held")], "=");
}

// `First.{u} | c(x: Second.{u})` and `Second.{u} | c(x: Type u)`: `First` is walked before `Second` has been found invariant, and learns it on the next round.
#[test]
fn a_mutual_pair_settles_on_a_later_round() {
    let first = family(
        1,
        Vec::new(),
        Vec::new(),
        |_| vec![at("Second", &[level(0)], Vec::new())],
        Vec::new(),
    );
    let inducts = BTreeMap::from([(named("First"), first), (named("Second"), boxed())]);

    assert_eq!(vectors(&mut kernel(), &inducts)[&named("First")], "=");
}

// `Uses.{u}(A: Type u) | c(x: Alias.{u}(A))` over `Alias.{u} = (A: Type u) => A`: the reduct is `A`, which carries no level. With no budget to unfold the alias its instance is read as written, every level invariant, and the refusal is handed back.
#[test]
fn an_instance_reduction_removes_leaves_no_level() {
    let alias = Free::from(&named("Alias"));
    let carrier = Free::local(1, Some("A"));

    for (budget, expected, refused) in [(100_000, "*", false), (0, "=", true)] {
        let mut kernel = Kernel::new(budget, SYNTAX);
        kernel.define(
            &alias,
            &Term::func_type(
                [(carrier, Term::type_at(level(0)))],
                Term::type_at(level(0)),
            ),
            &Term::func(
                [(carrier, Term::type_at(level(0)))],
                Term::free_var(&carrier),
            ),
            &scheme(1),
        );
        let inducts = BTreeMap::from([(named("Uses"), uses(&alias))]);
        kernel.declare_induct(&named("Uses"), &inducts[&named("Uses")]);
        let found = variance_vectors(&mut kernel, Declarations::of(&inducts, &BTreeMap::new()));

        assert_eq!(spelled(&found.vectors[&named("Uses")]), expected);
        assert_eq!(
            found
                .exhausted
                .is_some_and(|(walked, _)| walked == named("Uses")),
            refused
        );
    }
}

// `Uses.{u}(A: Type u) | c(x: Opaque.{u}(A))` over an `Opaque` with a type and no value: its instance survives forcing, and a level on it is compared for equality.
#[test]
fn a_level_on_an_instance_that_does_not_unfold_is_invariant() {
    let opaque = Free::from(&named("Opaque"));
    let carrier = Free::local(1, Some("A"));
    let mut kernel = kernel();
    kernel.declare(
        &opaque,
        &Term::func_type(
            [(carrier, Term::type_at(level(0)))],
            Term::type_at(level(0)),
        ),
        &scheme(1),
    );
    let inducts = BTreeMap::from([(named("Uses"), uses(&opaque))]);

    assert_eq!(vectors(&mut kernel, &inducts)[&named("Uses")], "=");
}

// `Box` analyzed while carrying a vector that calls its `Type`-holding level irrelevant: a name inside the set answers from the fixpoint, never from what it carries.
#[test]
fn a_carried_vector_is_recomputed_rather_than_believed() {
    let lying = InductDecl {
        variances: vec![Variance::Irrelevant],
        ..boxed()
    };

    assert_eq!(vector("Box", lying), "=");
}

// A family registered and not analyzed is a unit in scope's: its vector was settled when that unit was taken, and a level composes through it as carried, an irrelevance and an invariance alike. A family no registry holds is invariant.
#[test]
fn a_family_outside_the_set_answers_from_the_vector_it_carries() {
    let holder = || {
        family(
            1,
            Vec::new(),
            Vec::new(),
            |_| vec![at("Box", &[level(0)], Vec::new())],
            Vec::new(),
        )
    };

    for (carried, expected) in [
        (Some(Variance::Irrelevant), "*"),
        (Some(Variance::Invariant), "="),
        (None, "="),
    ] {
        let mut kernel = kernel();
        if let Some(carried) = carried {
            let declaration = InductDecl {
                variances: vec![carried],
                ..boxed()
            };
            kernel.declare_induct(&named("Box"), &declaration);
        }
        let inducts = BTreeMap::from([(named("Holder"), holder())]);

        assert_eq!(vectors(&mut kernel, &inducts)[&named("Holder")], expected);
    }
}
