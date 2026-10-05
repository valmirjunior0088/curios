//! Which universe levels of an `induct` or a `struct` two instances may differ in and still be one type.
//!
//! A level is **irrelevant** when no constructor payload, index target, field or index type mentions it except through a position that is itself irrelevant: another family's irrelevant level, or an instance reduction removes. Every other level is **invariant**, and none is covariant. A parameter's type and the result sort go unread: they bound a level from below and say nothing of what an instance holds. An irrelevant level leaves two instances' constructor telescopes identical, targets included, so the two have the same inhabitants and every elimination reads the same telescopes.
//!
//! The walk reads each type's reduct, as [`positivity_vectors`](crate::positivity_vectors) does and for its reason: a type-level `let` has to be opened to see the former it names, and a `/sys` former unfolds to an intrinsic one that carries no level. What the walk cannot see through marks every level it carries. That is the refusing direction, an invariant level being compared for equality as every level was before a vector existed.
//!
//! # Shared, not duplicated
//!
//! Both checkers run this analysis, through [`Env`], for the reason the positivity analysis gives: it is a total function of the declarations, so a second implementation would be a second run on the same input. The elaborator computes a family's vector when its group finalizes, since a later item compares two of its instances, and again over the zonked module, which is the vector a unit carries. The kernel reads the carried vector while it walks the items and recomputes its module's after: a carried irrelevance its recomputation denies refuses the module.

use {
    crate::{Declarations, Env, binders, forceable, labelled, rec_member},
    curios_core::{
        Advance, Bound, Func, FuncType, Global, InductType, Instance, Level, RecGroup, Struct,
        StructType, Subterm, Telescope, Term, TupleType, Variance, Variant, universe_params,
    },
    std::collections::{BTreeMap, BTreeSet},
};

/// What [`variance_vectors`] found: each analyzed declaration's vector, one entry per universe parameter, and the first reduction the driver refused with the declaration whose walk it was.
///
/// A refused reduction does not stop the walk. The term is read as written and every level it carries is marked, so a vector computed past one claims no irrelevance the reduct would have denied. It is handed back because a driver that denies a carried irrelevance on such a vector reports the budget, which may be all that denied it.
#[derive(Debug)]
pub struct VarianceVectors<E> {
    pub vectors: BTreeMap<Global, Vec<Variance>>,
    pub exhausted: Option<(Global, E)>,
}

/// Each declaration's variance vector, iterated from every level irrelevant until no vector changes.
///
/// Whole-set for the reason the positivity fixpoint is: a family mentions itself, so its own vector is an input to computing it, and in a mutual pair one learns that a level is invariant on the round after the other does. Marking only grows a vector, so the iteration ends at the least fixpoint, which is the right one: a level nothing forces invariant is one two instances cannot be told apart by.
///
/// A name outside `declarations` answers from the driver's registry: a family of a unit in scope, whose vector the same checker held to that unit's declarations when it took the unit, and which it reads from then on as it reads the rest of the entry. A name no registry holds is invariant.
pub fn variance_vectors<E: Env>(
    env: &mut E,
    declarations: Declarations<'_>,
) -> VarianceVectors<E::Error> {
    curios_profile::profile!("variance_vectors");
    if declarations.is_empty() {
        return VarianceVectors {
            vectors: BTreeMap::new(),
            exhausted: None,
        };
    }

    // Opening a telescope mints binders, and the fixpoint re-walks the same pieces, so every declaration is split once, up front.
    let split = declarations
        .names()
        .into_iter()
        .map(|name| {
            let entry = Split::of(env, declarations, &name);
            (name, entry)
        })
        .collect::<BTreeMap<_, _>>();
    let mut vectors = Vectors {
        computed: split
            .iter()
            .map(|(name, entry)| (*name, vec![Variance::Irrelevant; entry.parameters]))
            .collect(),
    };
    let mut exhausted = None;

    loop {
        let mut changed = false;
        for (name, entry) in &split {
            let (marked, refused) = sweep(env, &vectors, entry);
            if exhausted.is_none()
                && let Some(error) = refused
            {
                exhausted = Some((*name, error));
            }

            let vector = vectors.computed.get_mut(name).expect("a vector per name");
            for (parameter, variance) in vector.iter_mut().enumerate() {
                // A level past the declaration's own count is one its telescopes should not mention; nothing compares it, so nothing is recorded for it.
                if *variance == Variance::Irrelevant && marked.holds(parameter) {
                    *variance = Variance::Invariant;
                    changed = true;
                }
            }
        }
        if !changed {
            break;
        }
    }

    VarianceVectors {
        vectors: vectors.computed,
        exhausted,
    }
}

/// A declaration opened for analysis: how many universe parameters it takes, and every type the walk reads.
#[derive(Debug)]
struct Split {
    parameters: usize,
    parts: Vec<Term>,
}

impl Split {
    fn of<E: Env>(env: &mut E, declarations: Declarations<'_>, name: &Global) -> Self {
        if let Some(declaration) = declarations.induct(name) {
            let params = binders(env, &declaration.arity);
            let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();

            // The index types are read here where positivity leaves them out: they store nothing, which is positivity's question, and two instances apart in a level one of them mentions are families over different index types, which is this one's.
            let mut parts = labelled(env, &declaration.indices_at(&arguments))
                .into_iter()
                .map(|(_, type_)| type_)
                .collect::<Vec<_>>();
            for (_, constructor) in &declaration.constructors {
                let payload = constructor.telescope.clone().open_params(&arguments);
                let mut cursor = payload.cursor();
                while let Some((_, type_)) = cursor.entry() {
                    parts.push(type_);
                    cursor.advance_fresh(|hint| env.fresh(hint.filter(|hint| !hint.is_empty())));
                }
                // The targets are read because a level may reach nothing else: `mk(): (N(F))` puts `N`'s invariant level in `Fam`'s index and in no payload.
                parts.extend(cursor.body().expect("a cursor past every entry"));
            }

            return Self {
                parameters: declaration.universe_context.parameter_count,
                parts,
            };
        }

        let declaration = declarations
            .struct_(name)
            .expect("every analyzed name is an inductive or a struct");
        let params = binders(env, &declaration.arity);
        let arguments = params.iter().map(Term::free_var).collect::<Vec<_>>();

        Self {
            parameters: declaration.universe_context.parameter_count,
            parts: labelled(env, &declaration.fields_at(&arguments))
                .into_iter()
                .map(|(_, type_)| type_)
                .collect(),
        }
    }
}

/// Each analyzed declaration's vector as the fixpoint currently estimates it.
#[derive(Debug)]
struct Vectors {
    computed: BTreeMap<Global, Vec<Variance>>,
}

impl Vectors {
    /// `name`'s variance in its `index`th universe parameter. A name outside the analyzed set answers from the registry, and one no registry holds is invariant, never irrelevant.
    fn at<E: Env>(&self, env: &E, name: &Global, index: usize) -> Variance {
        if let Some(vector) = self.computed.get(name) {
            return vector.get(index).copied().unwrap_or(Variance::Invariant);
        }
        env.induct_decl(name)
            .map(|declaration| declaration.variance(index))
            .or_else(|| {
                env.struct_decl(name)
                    .map(|declaration| declaration.variance(index))
            })
            .unwrap_or(Variance::Invariant)
    }
}

/// The universe parameters one declaration's parts fix under the current estimates, and the first reduction the driver refused while finding them.
fn sweep<E: Env>(env: &mut E, vectors: &Vectors, split: &Split) -> (Marked, Option<E::Error>) {
    let mut walk = Walk {
        env,
        vectors,
        forcing: Vec::new(),
        marked: Marked::default(),
        refused: None,
    };
    for part in &split.parts {
        walk.walk(part);
    }

    (walk.marked, walk.refused)
}

/// The universe parameters a walk found mentioned: the ones it met, or every one, where it met a term that may mention any.
#[derive(Debug, Default)]
struct Marked {
    levels: BTreeSet<usize>,
    every: bool,
}

impl Marked {
    fn holds(&self, parameter: usize) -> bool {
        self.every || self.levels.contains(&parameter)
    }
}

/// One declaration's traversal: which of its universe parameters something it holds mentions.
struct Walk<'a, E: Env> {
    env: &'a mut E,
    vectors: &'a Vectors,
    /// The `rec` members this walk is already inside, innermost last: positivity's guard, for positivity's reason.
    forcing: Vec<(RecGroup, usize)>,
    marked: Marked,
    refused: Option<E::Error>,
}

impl<E: Env> Walk<'_, E> {
    /// A level standing where two instances would differ in it. The walk opens every binder it crosses and enters no universe binder, so a parameter here is the declaration's own.
    fn level(&mut self, level: &Level) {
        self.marked
            .levels
            .extend(level.params().map(|param| param.0));
    }

    /// Every level `term` mentions, read as written: what the walk does with a term it cannot see through.
    fn everything(&mut self, term: &Term) {
        self.marked
            .levels
            .extend(universe_params(term).into_iter().map(|param| param.0));
    }

    /// A nominal occurrence's instance: its levels at the family's invariant positions, and nothing at an irrelevant one.
    fn nominal(&mut self, name: &Global, universes: &[Level]) {
        for (index, level) in universes.iter().enumerate() {
            if self.vectors.at(self.env, name, index) == Variance::Invariant {
                self.level(level);
            }
        }
    }

    fn forced(&mut self, term: &Term) -> (Term, bool) {
        if !forceable(term) {
            return (term.clone(), false);
        }
        match self.env.force(term) {
            Ok(reduced) => (reduced, false),
            Err(error) => {
                if self.refused.is_none() {
                    self.refused = Some(error);
                }
                (term.clone(), true)
            }
        }
    }

    /// Each entry's type under fresh binders, then the terminal, so what follows a binder holds free occurrences and can be reduced.
    fn telescope<B: Bound>(
        &mut self,
        telescope: &Telescope<B>,
        terminal: impl FnOnce(&mut Self, B),
    ) {
        let mut cursor = telescope.cursor();
        while let Some((_, type_)) = cursor.entry() {
            self.walk(&type_);
            cursor.advance_fresh(|hint| self.env.fresh(hint.filter(|hint| !hint.is_empty())));
        }
        terminal(self, cursor.body().expect("a cursor past every entry"));
    }

    /// A form the walk reads through its children alone. A child under a binder the walk did not open is closed and is read as written: a nominal node inside it is still read by its family's vector, and an applied former, unforced there, marks every level of its instance.
    fn children(&mut self, term: &Term) {
        let subterm: &Subterm = term;
        subterm.any_child_term(&mut |child| {
            self.walk(child);
            false
        });
    }

    fn walk(&mut self, term: &Term) {
        // A `rec` member this path is already inside is left folded, as positivity leaves it: forcing it again exposes the same node one level deeper.
        let member = rec_member(term);
        let cyclic = member
            .as_ref()
            .is_some_and(|member| self.forcing.contains(member));
        let (term, refused) = match cyclic {
            true => (term.clone(), false),
            false => self.forced(term),
        };
        if refused {
            self.everything(&term);
            return;
        }
        let entered = match (cyclic, member) {
            (false, Some(member)) => {
                self.forcing.push(member);
                true
            }
            _ => false,
        };

        match &*term {
            Subterm::Type(level) => self.level(level),
            // A parameter, a local or a monomorphic name carries no level.
            Subterm::Prop | Subterm::Var(_) => {}

            Subterm::FuncType(FuncType { telescope, .. }) => {
                self.telescope(telescope, |walk, codomain| walk.walk(&codomain));
            }
            Subterm::TupleType(TupleType { telescope }) => self.telescope(telescope, |_, ()| {}),
            // A lambda standing in a type is a family passed as an argument, `(A: Type) => Result(E, A)`: its binders' types and its body are what an instance holds of it.
            Subterm::Func(Func { telescope, .. }) => {
                self.telescope(telescope, |walk, body| walk.walk(&body));
            }

            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(indices) {
                    self.walk(argument);
                }
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                self.nominal(name, universes);
                for argument in params {
                    self.walk(argument);
                }
            }
            // A value compares at its family's variance.
            Subterm::Variant(Variant {
                name,
                universes,
                params,
                payload,
                ..
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(payload) {
                    self.walk(argument);
                }
            }
            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                ..
            }) => {
                self.nominal(name, universes);
                for argument in params.iter().chain(fields) {
                    self.walk(argument);
                }
            }

            // An instance that survived forcing is a bare or partly applied former, or a definition that does not unfold: its levels are compared for equality, so each is invariant. A projection head's group is closed over its own scheme, which these levels instantiate.
            Subterm::Instance(Instance { levels, .. }) => {
                for level in levels {
                    self.level(level);
                }
            }
            // A group carries a universe context of its own, so it is read as written rather than walked into.
            Subterm::Rec(_) => self.everything(&term),
            // A metavariable's solution may mention any level.
            Subterm::Metavar(_) => self.marked.every = true,

            Subterm::Intrinsic(intrinsic) => {
                for level in intrinsic.result_universes() {
                    self.level(level);
                }
                self.children(&term);
            }

            Subterm::Apply(_)
            | Subterm::Tuple(_)
            | Subterm::Match(_)
            | Subterm::Proj(_)
            | Subterm::Let(_)
            | Subterm::Foreign(..)
            | Subterm::Transient(_) => self.children(&term),
        }

        // Every arm above falls through but the refusal's, which entered nothing, so the path is popped however the walk leaves it.
        if entered {
            self.forcing.pop();
        }
    }
}
