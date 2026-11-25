//! Which universe a type inhabits, and the type of a neutral spine.
//!
//! Conversion is type-directed in Curios: a goal at a `Prop`-sorted type is discharged without comparing the sides at all, because any two inhabitants of a proposition are definitionally equal. So before conversion can compare anything it has to know a type's *sort*, and that is a typing question.
//!
//! Answering it in full would need [`infer`](super::infer()), which needs conversion, which is a cycle. The way out is the same one the elaborator takes: a *synthesis-only* type computation for neutral spines ([`ByValue::synth_neutral`]) that reads types off binders and declarations without ever checking anything, and therefore never reaches conversion. It answers "what type does this spine have, if it has one" and nothing else.
//!
//! Unlike the elaborator's, neither function here guesses. Where a shape cannot be classified the certifier refuses; see the module documentation on [`certifier`](super) for why a guessed level is the unsound direction.
//!
//! # Two roles, one body
//!
//! Asking a type's sort is two different questions depending on who asks. [`Sort::of`] is a **lookup**: it classifies a type something has already checked, which is the only thing conversion can afford to call, since typing reaches conversion and the cycle would close. [`infer_sort`] is a **judgment**: it accepts a term *as* a type, and is what [`infer`](super::infer()) calls.
//!
//! They differ in exactly one function — [`Establish`], which says whether a type former's parts are classified or typed — and share everything else, the Π and Σ rules included. That is deliberate: the rules are subtle enough that a second copy would be a second chance to get them wrong, while the role is a one-word choice a caller has to make at the call site.
//!
//! The split exists because the roles are easy to confuse. Answering a `FuncType` or a `TupleType` in `infer` with the lookup would admit a former whatever its parts said: a Σ whose field type is a nominal occurrence with ill-typed arguments would pass, and the projection rule would hand that field type back as a scrutinee at a forged equation.

#[cfg(test)]
mod test_support;

#[cfg(test)]
mod classification_tests;
#[cfg(test)]
mod memo_tests;

use {
    super::{ByValue, Certifier, Counted, Error, infer_type},
    curios_common::recurse,
    curios_core::{
        Advance, Bound, Field, Free, FuncType, Instance, Intrinsic, Level, Match, Produced, Proj,
        Quality, Reducer, StructType, Subterm, Telescope, Term, TupleType, instantiate_universes,
    },
};

/// The universe a type inhabits.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Sort {
    Type(Level),
    Prop(Level),
}

impl Sort {
    /// The universe term this sort denotes, which is what a type former reports as its own type.
    pub fn term(self) -> Term {
        match self {
            Sort::Type(level) => Term::type_at(level),
            Sort::Prop(level) => Term::prop_at(level),
        }
    }

    /// Whether this is a strict proposition — the question proof irrelevance turns on.
    pub fn is_prop(&self) -> bool {
        matches!(self, Sort::Prop(_))
    }
}

/// An occurrence supplies exactly as many parameters, or indices, as its declaration declares.
///
/// The counterpart of [`Certifier::check_instance`](crate::Certifier) for the arities a nominal term carries beside its universe instance — an occurrence's parameters and indices, and a value's parameters. Both are read from the declaration the moment anything asks what an occurrence *is*, so neither may be taken on the occurrence's own word: an `InductType` at no parameters for a one-parameter family, classified as a well-formed type, would leave every consumer of the arity wrong about it — `instantiate` peeling a prefix that is not there, and `indices_at` reaching `Telescope::open`'s assertion and aborting the process. Checked here, beside the universe width, because this is where a declaration is consulted for an occurrence at all.
pub fn arity_matches(counted: Counted, expected: usize, actual: usize) -> Result<(), Error> {
    match expected == actual {
        true => Ok(()),
        false => Err(Error::Arity {
            counted,
            expected,
            actual,
        }),
    }
}

impl ByValue<'_, '_> {
    /// Decode a term that is *already* a universe — a kind's codomain, a match's goal, a synthesized neutral's type — into the sort it names.
    ///
    /// Distinct from [`ByValue::sort_of`], which classifies an arbitrary type: `as_sort(Prop)` is `Prop`, whereas `Sort::of(Prop)` is `Type 0`, since the universe `Prop` is itself `Type`-sorted.
    pub(crate) fn as_sort(&mut self, universe: &Term) -> Result<Sort, Error> {
        let reduced = self.whnf(universe.clone())?;

        match &*reduced {
            Subterm::Sort(curios_core::Sort {
                quality: Quality::Prop,
                level,
            }) => Ok(Sort::Prop(level.clone())),
            Subterm::Sort(curios_core::Sort {
                quality: Quality::Type,
                level,
            }) => Ok(Sort::Type(level.clone())),
            Subterm::Sort(curios_core::Sort {
                quality: Quality::Param(_) | Quality::Meta(_),
                level: _,
            }) => Err(Error::OpenSort(reduced.clone())),
            _ => Err(Error::NotASort(reduced.clone())),
        }
    }

    /// Classify `type_`, which the caller must already have checked to be a type — the **lookup** of the two roles the module header separates, never a judgment.
    ///
    /// **By value, as it is reduced:** an answer is filed under the spelling a substituted `let` leaves, so one type asked about under two `let`-bound names is one question, and everything asked beneath is a part or a reduct of a type that was ([`ByValue`]).
    ///
    /// Remembered per type, and probed here at every field and domain the rules below ask about: a type is a graph, and a record of two fields at one type asks one question where a walk per path asks two at every level. `Memos` states how long an answer stands.
    pub(crate) fn sort_of(&mut self, type_: &Term) -> Result<Sort, Error> {
        curios_common::profile!("Sort::of");
        if let Some(sort) = self.sort_hit(type_) {
            return Ok(sort);
        }

        // Depth is bought with stack, as every other walk over data-shaped depth buys it, and each level is paid for from the budget, as the reducer pays for its own: a Π whose domain reduces to the Π itself is classified by descending into a type whose classification is already in flight, and the memo is written where the computation returns, so nothing closes that cycle short of the budget. Every reduction such a cycle asks after its first can be a memo hit, which spends nothing (`rec b: Type = List(b)`), so the level is the one charge it is sure to meet. Without the stack the native one goes first, and without the level the walk grows into memory until the process is killed; neither is a refusal (`documentation/design/soundness/the-soundness-board.md`).
        self.enter_level()?;
        let sort = recurse(|| Sort::classify(self, type_));
        self.leave_level();
        let sort = sort?;
        self.sort_store(type_.clone(), sort.clone());

        Ok(sort)
    }
}

impl Sort {
    /// [`ByValue::sort_of`]'s rules, one per type former.
    fn classify(certifier: &mut ByValue, type_: &Term) -> Result<Sort, Error> {
        // Asked of the type as written, before anything is run to classify it.
        if let Some(level) = declared_into_prop(certifier, type_)? {
            return Ok(Sort::Prop(level));
        }
        if let Some(sort) = published(certifier, type_)? {
            return Ok(sort);
        }

        let reduced = certifier.reduce_forced(type_.clone())?;

        match &*reduced {
            // A nominal type's sort is declared, not derived: the declaration says whether the family lands in `Type` or in `Prop`, at the universes this occurrence instantiated it at.
            Subterm::InductType(family) => {
                let result_sort = certifier.induct_at(family)?.result_sort().clone();

                certifier.as_sort(&result_sort)
            }
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => {
                let result_sort = certifier
                    .struct_at(name, universes, params)?
                    .result_sort()
                    .clone();

                certifier.as_sort(&result_sort)
            }

            // A *non-empty* record of propositions is a proposition. The empty tuple is unit rather than a proposition: it is what an effect returns (`/std/print : .. -> {}`), so it must be kept at runtime, and calling it a proposition would erase it.
            Subterm::TupleType(TupleType { telescope }) if !telescope.is_empty() => {
                tuple_sort(certifier, telescope.clone(), ByValue::sort_of)
            }
            Subterm::TupleType(_) => Ok(Sort::Type(Level::zero())),

            Subterm::FuncType(FuncType { telescope, .. }) => {
                func_sort(certifier, telescope.clone(), ByValue::sort_of)
            }

            Subterm::Intrinsic(intrinsic) => certifier.sort_of_intrinsic(intrinsic),

            // A type-valued `match` (`rec Lt = match n | ..` at `Prop`): its goal is the sort, which every arm shares.
            Subterm::Match(m) => certifier.as_sort(&m.goal),

            // A neutral type — a `Prop` hypothesis, or a family application stuck on a variable. A universe instance reduction left standing is one too, read at the levels the occurrence states: a local head, whose binder answers whatever they are, or a scheme declared without a body, instantiated at them.
            Subterm::Var(_) | Subterm::Apply(_) | Subterm::Proj(_) | Subterm::Instance(_) => {
                sort_of_neutral(certifier, &reduced)
            }

            // A projection of a recursive group is neutral in the same way: `rec Lt = ..; Lt` is stuck at its own fixed point, and its type is the one its group carries.
            Subterm::Rec(_) if reduced.as_rec_proj().is_some() => {
                sort_of_neutral(certifier, &reduced)
            }

            // `Type u : Type (u + 1)`, and `Prop : Type 0`.
            Subterm::Sort(curios_core::Sort {
                quality: Quality::Type | Quality::Prop,
                level,
            }) => Ok(Sort::Type(level.succ()?)),
            Subterm::Sort(curios_core::Sort {
                quality: Quality::Param(_) | Quality::Meta(_),
                level: _,
            }) => Err(Error::OpenSort(reduced.clone())),

            _ => Err(Error::Unclassified(reduced.clone())),
        }
    }
}

/// Whether `type_`, as written, is an application of a declaration that lands in `Prop`: a family or a definition, declared at a type whose codomain at these arguments is `Prop`.
///
/// **A proposition is known by its head.** A declaration says where its applications land whatever their arguments reduce to — `/std/Eq`'s family and `/sys/Bool/Holds: (Bool) -> Prop` say `Prop` — and [`ByValue::synth_neutral`] reads that without reducing an argument. Classified by its reduct instead, the type is run to be told what its declaration states: a closed `Holds(d)` is decided, and a text scanned to learn that `/std/Str/Valid(b)` is a proposition, by every question that only asks whether a proof stands here — the record of erased positions, and conversion's irrelevance gate, which exists so that no proof is reduced.
///
/// **Only a declaration's word is taken.** The head is a name this certifier defined, whose type it checked the body against before it stored either. A projection of a group the term itself carries states a claim nothing here has checked, and a member of the group being checked is a binder assumed at one: each is classified by what it reduces to, and [`Sort::classify`]'s neutral arm reads such a claim only where reduction leaves the spine standing.
///
/// **Only `Prop` is answered.** A head declared into `Type` says its application is no proposition, no proposition standing where a `Type` is declared, and says nothing more: which type it is, and at which level, is its reduct's to say, so such a type is reduced as before.
pub fn declared_into_prop(certifier: &mut ByValue, type_: &Term) -> Result<Option<Level>, Error> {
    if let Some(Sort::Prop(level)) = published(certifier, type_)? {
        return Ok(Some(level));
    }
    if !matches!(&**type_, Subterm::Apply(_)) || !heads_a_declaration(certifier, type_) {
        return Ok(None);
    }
    let Some(declared) = certifier.synth_neutral(type_)? else {
        return Ok(None);
    };

    Ok(match &*certifier.whnf(declared)? {
        Subterm::Sort(curios_core::Sort {
            quality: Quality::Prop,
            level,
        }) => Some(level.clone()),
        _ => None,
    })
}

/// The sort `type_` stands at where it is written as a reference to a definition stating the sort of its value ([`curios_core::Definition::value_sort`]), at the universes the reference states: this certifier checked the statement where it checked the definition, so it is read with nothing run.
fn published(certifier: &mut ByValue, type_: &Term) -> Result<Option<Sort>, Error> {
    let Subterm::Instance(Instance { name, universes }) = &**type_ else {
        return Ok(None);
    };
    let sort = match certifier.value_sort_at(name, universes) {
        Some(sort) => instantiate_universes(sort, universes)?,
        None => return Ok(None),
    };

    certifier.as_sort(&sort).map(Some)
}

/// Whether the spine `term` is headed by a reference to a name this certifier defined.
fn heads_a_declaration(certifier: &Certifier, term: &Term) -> bool {
    let mut head = term;
    while let Subterm::Apply(apply) = &**head {
        head = &apply.head;
    }

    match &**head {
        Subterm::Instance(Instance { name, .. }) => certifier.scheme_of(name).is_some(),
        _ => false,
    }
}

/// How a type former's parts are established before its universe is computed from them.
///
/// This is the *whole* of what the two roles in this module differ in, beside the receiver each runs over. [`ByValue::sort_of`] is a lookup — it classifies a type that something has already checked, in a reading — and passes itself. [`infer_sort`] is a judgment — it accepts a term *as* a type, over the certifier — and passes [`infer_type`]. Everything else, the Π and Σ rules included, is shared.
///
/// Naming the difference is what keeps a caller from inheriting the wrong role; the module documentation states what inheriting the lookup would admit.
type Establish<C> = fn(&mut C, &Term) -> Result<Sort, Error>;

/// What the Π and Σ rules ask of the receiver they walk a former's binders over: a bracket, and a binder minted and opened at each domain. A lookup walks them over a reading and a judgment over the certifier, so the rules are one statement for both roles.
trait Opens {
    fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T;

    fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free;
}

impl Opens for Certifier<'_> {
    fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        Certifier::scoped(self, walk)
    }

    fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        Certifier::advance_assumed(self, walk, domain)
    }
}

impl Opens for ByValue<'_, '_> {
    fn scoped<T>(&mut self, walk: impl FnOnce(&mut Self) -> T) -> T {
        ByValue::scoped(self, walk)
    }

    fn advance_assumed(&mut self, walk: &mut impl Advance, domain: &Term) -> Free {
        ByValue::advance_assumed(self, walk, domain)
    }
}

/// Σ: a record of nothing but propositions is a proposition; otherwise its level is the join of its fields'.
fn tuple_sort<C: Opens>(
    certifier: &mut C,
    telescope: Telescope<()>,
    establish: Establish<C>,
) -> Result<Sort, Error> {
    certifier.scoped(|certifier| {
        sort_of_binders(
            certifier,
            telescope,
            establish,
            |_, mut levels, proofs, ()| {
                Ok(match levels.is_empty() {
                    true => Sort::Prop(Level::max(proofs)),
                    false => {
                        levels.extend(proofs);
                        Sort::Type(Level::max(levels))
                    }
                })
            },
        )
    })
}

/// Π into a proposition is a proposition — that is what makes `(n : Nat) -> P(n)` erasable — and into a type a type, at the join of the domains' levels and the codomain's either way. A `Prop`-sorted domain counts at its own level: a proposition that quantifies over the sort it sits in could be instantiated at itself.
fn func_sort<C: Opens>(
    certifier: &mut C,
    telescope: Telescope<Term>,
    establish: Establish<C>,
) -> Result<Sort, Error> {
    certifier.scoped(|certifier| {
        sort_of_binders(
            certifier,
            telescope,
            establish,
            |certifier, mut levels, proofs, codomain| {
                levels.extend(proofs);
                Ok(match establish(certifier, &codomain)? {
                    Sort::Prop(output) => {
                        levels.push(output);
                        Sort::Prop(Level::max(levels))
                    }
                    Sort::Type(output) => {
                        levels.push(output);
                        Sort::Type(Level::max(levels))
                    }
                })
            },
        )
    })
}

/// The sort of `type_`, having **typed** every part a type former binds rather than classifying it — [`ByValue::sort_of`]'s judgment counterpart, and the one `infer` calls.
///
/// Only the two binder-carrying formers differ from the lookup, because only they hold a part nothing else establishes: a nominal occurrence's arguments are typed where the occurrence is inferred, an intrinsic former's element type by `infer_intrinsic`, and a neutral's sort is read off a binder that was assumed at a type its own telescope had checked. So every other shape delegates, on the *already reduced* term, and the memo makes that second reduction free.
pub fn infer_sort(certifier: &mut Certifier, type_: &Term) -> Result<Sort, Error> {
    let reduced = certifier.reduce_forced(type_.clone())?;

    match &*reduced {
        Subterm::TupleType(TupleType { telescope }) if !telescope.is_empty() => {
            tuple_sort(certifier, telescope.clone(), infer_type)
        }
        Subterm::FuncType(FuncType { telescope, .. }) => {
            func_sort(certifier, telescope.clone(), infer_type)
        }
        _ => Sort::of(certifier, &reduced),
    }
}

/// The sort of a neutral type: what [`ByValue::synth_neutral`] reads off its head *is* its sort.
fn sort_of_neutral(certifier: &mut ByValue, reduced: &Term) -> Result<Sort, Error> {
    let synthesized = certifier
        .synth_neutral(reduced)?
        .ok_or_else(|| Error::Unclassified(reduced.clone()))?;

    certifier.as_sort(&synthesized)
}

/// Walk a binder telescope, collecting the level each domain contributes, and hand the collected levels and the terminal to the clause that decides the former's own sort.
///
/// A domain that is itself a proposition contributes no level: `Prop` sits below the hierarchy rather than in it. Each binder joins the local scope carrying its own type before the walk descends, because a later domain may mention an earlier one — and opening with a variable nothing can type would leave [`ByValue::synth_neutral`] unable to classify every occurrence of it further in, which here is not imprecision but a refusal.
///
/// Σ and Π share every line of that and differ only in the terminal clause, which is exactly where the two rules genuinely differ: a record is a proposition when *all* its fields are, a function when its *codomain* is. Splitting there keeps that difference the only thing either caller states.
///
/// `establish` is the other axis, and it is orthogonal: it says whether a domain is *classified* or *typed*, which is the lookup/judgment split [`Establish`] documents. The rules above are written once and both roles run them.
fn sort_of_binders<C: Opens, B: Bound>(
    certifier: &mut C,
    telescope: Telescope<B>,
    establish: Establish<C>,
    terminal: impl FnOnce(&mut C, Vec<Level>, Vec<Level>, B) -> Result<Sort, Error>,
) -> Result<Sort, Error> {
    let mut levels = Vec::new();
    let mut proofs = Vec::new();
    let mut cursor = telescope.cursor();

    while let Some((_, domain)) = cursor.entry() {
        match establish(certifier, &domain)? {
            Sort::Type(level) => levels.push(level),
            Sort::Prop(level) => proofs.push(level),
        }
        certifier.advance_assumed(&mut cursor, &domain);
    }

    let body = cursor.body().expect("a cursor past every entry");
    terminal(certifier, levels, proofs, body)
}

impl ByValue<'_, '_> {
    /// The sort of an intrinsic type former.
    ///
    /// A closed intrinsic quantifies over nothing and sits at level 0. A parameterized one carries its parameter's level: `List : Type u -> Type u`, and pinning that at 0 would claim the type is smaller than it is — the unsound direction, and what would let a large type be stored in a small universe.
    ///
    /// Reachable from `infer_intrinsic` as well, which types these formers rather than restating the rule: a second copy reading the element's sort as the former's would type a list of proofs at `Prop`.
    pub(crate) fn sort_of_intrinsic(&mut self, intrinsic: &Intrinsic) -> Result<Sort, Error> {
        match intrinsic {
            Intrinsic::BoolType
            | Intrinsic::NatType
            | Intrinsic::ByteType
            | Intrinsic::IntType
            | Intrinsic::FltType
            | Intrinsic::BytesType
            | Intrinsic::HandleType
            | Intrinsic::CellType => Ok(Sort::Type(Level::zero())),

            // A list, box, or description *of* proofs is not itself a proposition: it has a length, an identity, or an effect, so its inhabitants are distinguishable and irrelevance does not apply. It lands in `Type`, and `Prop : Type 0`.
            //
            // For `Io` that is load-bearing rather than tidy. Erasure is sort-driven, so a `Prop`-sorted `Io(P)` would be dropped as proof content and its host effect would vanish with it.
            Intrinsic::ListType(element)
            | Intrinsic::BoxType(element)
            | Intrinsic::IoType(element) => {
                let element = element.clone();

                Ok(match self.sort_of(&element)? {
                    Sort::Type(level) => Sort::Type(level),
                    Sort::Prop(level) => Sort::Type(level),
                })
            }

            // An intrinsic *value* is not a type, and an operation stuck on its operands is a neutral of the type its signature produces, which typing read it at before this lookup was asked: one producing a sort — a read of a list of types, a fold whose result is a type — is a type at that sort, and one producing anything else classifies nothing.
            other => {
                let unclassified = || Error::Unclassified(Term::intrinsic(other.clone()));
                match other.signature(&self.syntax()).produced {
                    Produced::Fixed(produced) => match self.as_sort(&produced) {
                        Err(Error::NotASort(_)) => Err(unclassified()),
                        sorted => sorted,
                    },
                    Produced::Sort => Err(unclassified()),
                }
            }
        }
    }

    /// The type of a neutral spine, read off binders and declarations without checking anything.
    ///
    /// A stuck elimination is a neutral too, and its type is the result it states. `None` where the spine is not one this can type — a shape whose type would need a judgment rather than a lookup. Callers turn that into a refusal; nothing here guesses.
    ///
    /// This must never reach [`convert`](super::convert()): it is what breaks the cycle between conversion and inference, and it stays broken only because every arm below is a lookup, a substitution, or a reduction.
    pub(crate) fn synth_neutral(&mut self, term: &Term) -> Result<Option<Term>, Error> {
        // A projection's type is carried by its own group, so this is a read rather than a lookup and cannot re-enter the group it names.
        if let Some((group, index)) = term.as_rec_proj() {
            return Ok(Some(group.member_type(index)));
        }

        match &**term {
            // A bound index names no binder in scope, so it has no type to look up: a shape this cannot type, as every other is answered.
            Subterm::Var(var) => Ok(var
                .as_free()
                .and_then(|free| self.local_type(free))
                .cloned()),

            // A declaration at the levels this reference chose: the scheme read uninstantiated and substituted at those levels.
            Subterm::Instance(Instance { name, universes }) => {
                let Some((scheme, context)) = self.scheme_of(name) else {
                    return Ok(None);
                };
                let (scheme, context) = (scheme.clone(), context.clone());
                self.check_instance(&context, universes)?;

                Ok(Some(instantiate_universes(&scheme, universes)?))
            }

            Subterm::Apply(apply) => {
                let Some(head_type) = self.synth_neutral(&apply.head)? else {
                    return Ok(None);
                };

                let supplied = apply.arguments.len();
                match &*self.whnf(head_type)? {
                    Subterm::FuncType(FuncType { telescope, .. })
                        if telescope.len() == supplied =>
                    {
                        let refs = apply.params().collect::<Vec<_>>();
                        Ok(Some(telescope.open(&refs)))
                    }
                    // A partially applied spine still has a type: the residual function type, with the supplied arguments substituted into the entries that remain, each still under its own mark.
                    Subterm::FuncType(func_type) if func_type.telescope.len() > supplied => {
                        let params = apply.params().cloned().collect::<Vec<_>>();
                        let telescope = func_type.telescope.clone().open_params(&params);
                        Ok(Some(Subterm::FuncType(FuncType { telescope }).into()))
                    }
                    _ => Ok(None),
                }
            }

            // A slot read off what it is a slot of, as typing holds it (`infer`): a written form and a slot of anything else have no type here.
            Subterm::Proj(Proj { head, field }) => {
                let Some(head_type) = self.synth_neutral(head)? else {
                    return Ok(None);
                };

                match (&*self.whnf(head_type)?, field) {
                    (Subterm::TupleType(TupleType { telescope }), Field::Tuple(_)) => {
                        Ok(telescope.field_type_from(head, field))
                    }
                    // A nominal record's field types live on its declaration, instantiated first at the head's universes and then at its parameters. Concept dispatch is a projection out of a witness record, so this arm is what types a method call.
                    (
                        Subterm::StructType(StructType {
                            name,
                            universes,
                            params,
                        }),
                        Field::Struct(structure, _),
                    ) if name == structure => {
                        let Ok(at) = self.struct_at(name, universes, params) else {
                            return Ok(None);
                        };

                        at.field_type(self, head, field)
                    }
                    _ => Ok(None),
                }
            }

            // A stuck elimination's type is its goal as written. The elimination was checked against it where it was typed (`goal_sort`), so this reads what typing established, as a variable's type is read off its binder.
            Subterm::Match(Match { goal, .. }) => Ok(Some(goal.clone())),

            _ => Ok(None),
        }
    }
}
