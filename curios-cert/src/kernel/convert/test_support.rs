//! Fixtures the kernel's conversion suites share: the kernel they start from, and the declarations they compare at.
//!
//! `pub(super)` rather than private: consumed by the sibling suites across `convert`, and nothing outside it.

use {
    crate::Kernel,
    curios_analysis::test_support::SYNTAX,
    curios_core::{
        Atom, Free, Global, InductDecl, InductParam, Intrinsic, Level, Nat, StructDecl, StructType,
        Subterm, Telescope, Term, UniverseContext, UniverseParam, Variance,
    },
    curios_utilities::{Plicity, Qualifier},
};

pub(super) fn kernel() -> Kernel {
    Kernel::new(100_000, SYNTAX)
}

pub(super) fn binder(index: u32, hint: &str) -> Free {
    Free::local(index, Some(hint))
}

pub(super) fn nat(n: usize) -> Term {
    Term::intrinsic(Intrinsic::Nat(Nat::new(n)))
}

pub(super) fn nat_type() -> Term {
    Term::intrinsic(Intrinsic::NatType)
}

/// A nominal family at a stated sort — the only way to obtain a base proposition, since the registry is what says a nominal type is one.
pub(super) fn declare(kernel: &mut Kernel, path: &str, result_sort: Term) -> Term {
    let name = Global::Authored(Qualifier::from([path]));

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(Telescope::done(())),
            constructors: Vec::new(),
            result_sort,
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    );

    Term::induct_type(name, Vec::<Term>::new(), Vec::<Term>::new())
}

/// `rec m : Type = (m) -> codomain; m`, optionally carrying a second unused member so that two such groups are not structurally equal and must take a delta step to be compared.
pub(super) fn equirecursive(member: Free, param: Free, codomain: Term, padded: bool) -> Term {
    let body = Term::func_type([(param, Term::free_var(&member))], codomain);
    let mut items = vec![(member, Term::type_ground(), body)];

    if padded {
        items.push((binder(99, "unused"), Term::type_ground(), nat_type()));
    }

    Term::rec(items, Term::free_var(&member))
}

/// `rec f : (t: Type⟨level⟩, x: Nat, y: Nat) -> Nat = (t, x, y) => match y | 0 => 0 | p + 1 => f(t, 0, p); f` — the projection of a one-member group whose only universe data is `level`, so two of them differ in nothing but their instance. The recursive call keeps the body restuck, so forcing returns the folded spelling, and it discards `x`, so two calls differing only there converge after one unfolding.
pub(super) fn polymorphic_fold(level: Level) -> Term {
    let f = binder(70, "f");
    let t = binder(71, "t");
    let x = binder(72, "x");
    let y = binder(73, "y");
    let motive = binder(74, "m");
    let pred = binder(75, "pred");
    let ih = binder(76, "ih");
    let sort = Term::type_at(level);

    let body = Term::func(
        [(t, sort.clone()), (x, nat_type()), (y, nat_type())],
        Term::nat_match(
            Term::free_var(&y),
            Some(&motive),
            nat_type(),
            nat(0),
            &pred,
            &ih,
            Term::apply(
                Term::free_var(&f),
                [Term::free_var(&t), nat(0), Term::free_var(&pred)],
            ),
        ),
    );

    Term::rec(
        [(
            f,
            Term::func_type([(t, sort), (x, nat_type()), (y, nat_type())], nat_type()),
            body,
        )],
        Term::free_var(&f),
    )
}

/// A struct type declared with `fields` and no parameter, for the goals that need a nominal type.
pub(super) fn declare_struct(kernel: &mut Kernel, path: &str, fields: Telescope<()>) -> Term {
    let name = Global::Authored(Qualifier::from([path]));
    kernel.declare_struct(
        &name,
        &StructDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::done(fields),
            result_sort: Term::type_ground(),
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    );

    Term::from(Subterm::StructType(StructType {
        name,
        universes: Vec::new(),
        params: Vec::new(),
    }))
}

/// How a fixture defines a family's former under the name it answers: as a projection of its own group, the spelling an `induct`'s has, or as the term itself.
#[derive(Clone, Copy, Debug)]
pub(super) enum Former {
    Projected,
    Plain,
}

impl Former {
    /// `value : type_`, generalized over `scheme`, in this spelling.
    fn spell(self, member: Free, type_: &Term, value: Term, scheme: &UniverseContext) -> Term {
        match self {
            Former::Projected => {
                let projection =
                    Term::rec([(member, type_.clone(), value)], Term::free_var(&member));
                let (group, index) = projection
                    .as_rec_proj()
                    .expect("a group whose tail is one of its members");

                Term::rec_proj(group.clone().with_universe_context(scheme.clone()), index)
            }
            Former::Plain => value,
        }
    }
}

/// `induct Wrap.{u}(A: Type u): Type u | wrap(a: A) end`, declared carrying `variances`, with its former `Wrap.{u} = (A: Type u) => Wrap.{u}(A)` defined under the name it answers in the spelling `former` names: a level only its parameter's type mentions.
pub(super) fn declare_wrap(
    kernel: &mut Kernel,
    variances: Vec<Variance>,
    former: Former,
) -> Global {
    let name = Global::Authored(Qualifier::from(["Wrap"]));
    let level = Level::param(UniverseParam(0));
    let scheme = UniverseContext {
        parameter_count: 1,
        constraints: Vec::new(),
    };
    let carrier = binder(80, "A");
    let held = binder(81, "a");
    let sort = Term::type_at(level.clone());

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: scheme.clone(),
            arity: Telescope::build([(carrier, sort.clone())], Telescope::done(())),
            constructors: vec![(
                Atom::from("wrap"),
                InductParam::new(
                    Telescope::build(
                        [(carrier, sort.clone()), (held, Term::free_var(&carrier))],
                        Vec::new(),
                    ),
                    vec![Plicity::Implicit, Plicity::Explicit],
                ),
            )],
            result_sort: sort.clone(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances,
        },
    );
    let type_ = Term::func_type([(carrier, sort.clone())], sort.clone());
    let value = Term::func(
        [(carrier, sort)],
        Term::induct_type_at(
            name,
            [level],
            [Term::free_var(&carrier)],
            Vec::<Term>::new(),
        ),
    );
    let value = former.spell(binder(87, "Wrap"), &type_, value, &scheme);
    kernel.define(&Free::from(&name), &type_, &value, &scheme);

    name
}

/// `induct Leaf.{u}: Type | leaf() end`, declared carrying `variances`, with its former defined under the name it answers in the spelling `former` names: a family with no parameter, which its bare name applies in full.
pub(super) fn declare_leaf(
    kernel: &mut Kernel,
    variances: Vec<Variance>,
    former: Former,
) -> Global {
    let name = Global::Authored(Qualifier::from(["Leaf"]));
    let scheme = UniverseContext {
        parameter_count: 1,
        constraints: Vec::new(),
    };

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: scheme.clone(),
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("leaf"),
                InductParam::new(Telescope::done(Vec::new()), Vec::new()),
            )],
            result_sort: Term::type_ground(),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances,
        },
    );
    let type_ = Term::type_ground();
    let node = Term::induct_type_at(
        name,
        [Level::param(UniverseParam(0))],
        Vec::<Term>::new(),
        Vec::<Term>::new(),
    );
    let value = former.spell(binder(86, "Leaf"), &type_, node, &scheme);
    kernel.define(&Free::from(&name), &type_, &value, &scheme);

    name
}

/// `induct Box.{u}: Type (u + 1) | box(T: Type u) end`, declared carrying `variances`: a level its payload's `Type` mentions.
pub(super) fn declare_box(kernel: &mut Kernel, variances: Vec<Variance>) -> Global {
    let name = Global::Authored(Qualifier::from(["Box"]));
    let level = Level::param(UniverseParam(0));
    let held = binder(82, "T");

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext {
                parameter_count: 1,
                constraints: Vec::new(),
            },
            arity: Telescope::done(Telescope::done(())),
            constructors: vec![(
                Atom::from("box"),
                InductParam::new(
                    Telescope::build([(held, Term::type_at(level.clone()))], Vec::new()),
                    vec![Plicity::Explicit],
                ),
            )],
            result_sort: Term::type_at(level.succ().expect("a parameter has a successor")),
            module: Qualifier::default(),
            rep_public: true,
            polarities: Vec::new(),
            variances,
        },
    );

    name
}

/// `induct Wit(P : <param_sort>) : (p : P)` — a family whose index type *is* its own parameter.
///
/// `induct_type_args` compares two index actuals at the type the declaration's telescope assigns, opened at the left instance's preceding actuals, so here the index pair is compared at whatever term stands in the parameter position. That makes the parameter's assumed type the whole of what decides the index goal, which is exactly the question a binder opened at a stand-in answers with the stand-in.
pub(super) fn declare_indexed(kernel: &mut Kernel, path: &str, param_sort: Term) -> Global {
    let name = Global::Authored(Qualifier::from([path]));
    let param = binder(90, "P");

    kernel.declare_induct(
        &name,
        &InductDecl {
            universe_context: UniverseContext::default(),
            arity: Telescope::build(
                [(param, param_sort)],
                Telescope::build([(binder(91, "p"), Term::free_var(&param))], ()),
            ),
            constructors: Vec::new(),
            result_sort: Term::type_ground(),
            module: Qualifier::from([path]),
            rep_public: true,
            polarities: Vec::new(),
            variances: Vec::new(),
        },
    );

    name
}
