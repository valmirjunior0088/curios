//! The term shape itself: the enum every [`Term`] derefs to, its structural queries, and its binder traversal.
//!
//! [`Term`] is the reference-counted handle and [`Subterm`] is what it points at, which is why the two are separate files rather than separate concepts. Everything that asks *what a term is* — its head, its children, whether it is a value — is answered here, and so is [`Bound`], the traversal that opens and closes every binder in the language exactly once per shape.

use {
    super::{Mentions, Union},
    crate::{
        Apply, Argument, Bound, Carrier, Cases, Foreign, Free, Func, FuncType, Global, InductType,
        Instance, Intrinsic, Let, LetBinding, Level, Match, Mention, Metavar, Method, Nat, Proj,
        Quality, Rec, RecGroup, Sort, Struct, StructType, Term, Transient, Tuple, TupleType,
        UniverseMetaId, Universes, Var, Variant, Visit,
    },
    curios_common::SyntaxRegistry,
    curios_num::{Floating, Integer},
    std::{collections::BTreeSet, sync::Arc},
};

/// The actual node of the core term language — one variant per term former. [`Term`] wraps a `Subterm` in an `Rc` with cached hash/reach and an optional extent, and `Deref`s here, so pattern matches are written against `Subterm` while construction goes through `Term`'s smart constructors. The final variant groups the elaboration-transient constructors under [`Transient`]: born in `into_core`, consumed by `elaborate`, never seen by reduce/convert/zonk/erase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Subterm {
    Sort(Sort),
    Intrinsic(Intrinsic),
    Foreign(Foreign),
    FuncType(FuncType),
    Func(Func),
    Apply(Apply),
    TupleType(TupleType),
    Tuple(Tuple),
    InductType(InductType),
    Variant(Variant),
    Match(Match),
    StructType(StructType),
    Struct(Struct),
    Proj(Proj),
    Let(Let),
    Rec(Rec),
    Instance(Instance),
    Var(Var),
    Metavar(Metavar),
    /// The elaboration-transient constructors, grouped so post-elaboration consumers dismiss the class with one arm.
    Transient(Transient),
}

/// One step a call's path takes toward its root ([`Subterm::inward`]).
pub enum Inward<'a> {
    /// An application, whose arguments the step applies.
    Applied(&'a Apply),
    /// An elaborated projection's slot.
    Field(usize),
}

impl Subterm {
    /// The group and index this term projects, when it is a member selection rather than a `rec` block with a tail of its own.
    ///
    /// On [`Subterm`] rather than [`Term`] so both reach it: a `Term` derefs here.
    pub fn as_rec_proj(&self) -> Option<(&RecGroup, usize)> {
        let Subterm::Rec(rec) = self else {
            return None;
        };

        rec.as_proj().map(|index| (rec.group(), index))
    }

    /// The step a call's path takes from this term toward its root, and the head it reaches: an application, whose arguments the step applies, or an elaborated projection's slot. `None` at the root; a written projection names no slot yet, and is a root.
    ///
    /// **The one statement of what a call is.** A member whose result is a function, applied past its own parameters, is `f(a)(b)` — the call `f(a)` applied to what is left — and a member whose result is a record is called through a field, `w(a).0(l)`, the way a recursive witness's method is. Read one level deep, at an application's immediate head, either is a neutral no demand unfolds and no blocker is read off, and a projection read as no part of the call demands its head and unfolds the call restuck. So every reader of a call follows these steps: the member it is of ([`called_member`](Self::called_member)), its arguments ([`call_arguments`](Self::call_arguments)), its root ([`Term::call_root`]), and totality's `call_path`, which collects them.
    pub fn inward(&self) -> Option<(Inward<'_>, &Term)> {
        match self {
            Subterm::Apply(apply) => Some((Inward::Applied(apply), &apply.head)),
            Subterm::Proj(Proj { head, field }) => {
                field.slot().map(|slot| (Inward::Field(slot), head))
            }
            _ => None,
        }
    }

    /// The group and index of the member this term is a folded call of: [`as_rec_proj`](Self::as_rec_proj) at the root of its path ([`inward`](Self::inward)), or of the term itself where it takes no step.
    pub fn called_member(&self) -> Option<(&RecGroup, usize)> {
        let mut term = self;
        while let Some((_, inner)) = term.inward() {
            term = inner;
        }

        term.as_rec_proj()
    }

    /// Every argument applied along this term's path ([`inward`](Self::inward)), the outermost first: what a folded call waits on where unfolding cannot choose an arm, since it does not wear its blocker on its head.
    pub fn call_arguments(&self) -> Vec<&Term> {
        let mut arguments = Vec::new();
        let mut term = self;
        while let Some((step, inner)) = term.inward() {
            if let Inward::Applied(apply) = step {
                arguments.extend(apply.params());
            }
            term = inner;
        }

        arguments
    }

    /// Whether this is a folded recursive spelling: a `rec` block, or a folded call ([`called_member`](Self::called_member)). It is the one weak-head value plain reduction leaves that a forced demand unfolds, so the one a forced reader must not be served.
    pub fn is_folded(&self) -> bool {
        self.called_member().is_some() || matches!(self, Subterm::Rec(_))
    }

    /// The family a nominal node is an instance or a value of.
    pub(super) fn nominal_name(&self) -> Option<&Global> {
        match self {
            Subterm::InductType(InductType { name, .. })
            | Subterm::StructType(StructType { name, .. })
            | Subterm::Variant(Variant { name, .. })
            | Subterm::Struct(Struct { name, .. }) => Some(name),
            _ => None,
        }
    }

    /// The global an instance node is of: a family's, or a definition's.
    pub(super) fn instance_name(&self) -> Option<&Global> {
        match self {
            Subterm::Instance(Instance { name, .. }) => Some(name),
            _ => None,
        }
    }

    pub(super) fn any_direct_universe_meta(
        &self,
        pred: &mut impl FnMut(UniverseMetaId) -> bool,
    ) -> bool {
        let mut level_matches = |level: &Level| level.metas().any(&mut *pred);
        match self {
            Subterm::Sort(Sort { level, .. }) => level_matches(level),
            Subterm::Intrinsic(intrinsic) => intrinsic
                .result_universes()
                .is_some_and(|universes| universes.levels.iter().any(level_matches)),
            Subterm::Instance(Instance { universes, .. }) => {
                universes.levels.iter().any(&mut level_matches)
            }
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                universes.levels.iter().any(level_matches)
            }
            // A group binds no universe, so a `Rec` node holds no context of its own: the levels its members spell are the enclosing declaration's, and the walk meets them in the member terms its scopes hold.
            _ => false,
        }
    }

    pub fn as_nat(&self) -> Option<Nat> {
        match self {
            Subterm::Intrinsic(Intrinsic::Nat(nat)) => Some(nat.clone()),
            _ => None,
        }
    }

    pub(crate) fn as_int(&self) -> Option<Integer> {
        match self {
            Subterm::Intrinsic(Intrinsic::Int(value)) => Some(value.clone()),
            _ => None,
        }
    }

    pub(crate) fn as_flt(&self) -> Option<Floating> {
        match self {
            Subterm::Intrinsic(Intrinsic::Flt(value)) => Some(*value),
            _ => None,
        }
    }

    pub fn as_bool(&self) -> Option<bool> {
        match self {
            Subterm::Intrinsic(Intrinsic::Bool(value)) => Some(*value),
            _ => None,
        }
    }

    /// What this subterm mentions, the declarations it references and the free variables it holds — the inherent-method spelling of [`Bound::mentions`], callable without importing the trait.
    pub fn mentions(&self) -> Mentions {
        <Subterm as Bound>::mentions(self)
    }

    /// Collect the head name of every inductive/struct *construction* and *type-former normal form* occurring in this subterm. These names are not references (they live in the registry, not the variable graph), so they do not appear in `mentions`; what follows a declaration's references, the lowering's unused-declaration lint, needs them as edges so a definition that *builds* a `Struct`/`Variant` (e.g. the string-literal meta-emitter's `/std/Str/Str`) keeps the backing type-former and field-type definitions alive even when no `Var` mentions them.
    pub fn construction_names(&self) -> BTreeSet<Global> {
        let mut names = BTreeSet::new();
        self.collect_construction_names(&mut names);
        names
    }

    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Subterm::Sort(_) | Subterm::Var(_) | Subterm::Instance(_) => {}
            Subterm::Transient(transient) => {
                transient
                    .subterms()
                    .for_each(|child| child.collect_construction_names(names));
            }
            Subterm::Metavar(Metavar { spine, .. }) => {
                spine
                    .iter()
                    .for_each(|t| t.collect_construction_names(names));
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.collect_construction_names(names),
            Subterm::Foreign(Foreign {
                arguments: args, ..
            }) => args
                .iter()
                .for_each(|arg| arg.collect_construction_names(names)),
            Subterm::Func(Func { telescope, .. }) => telescope.collect_construction_names(names),
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope.collect_construction_names(names)
            }
            Subterm::Apply(apply) => {
                apply.head.collect_construction_names(names);
                apply
                    .params()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::TupleType(TupleType { telescope, .. }) => {
                telescope.collect_construction_names(names)
            }
            Subterm::Tuple(Tuple { fields, .. }) => {
                fields
                    .iter()
                    .for_each(|f| f.collect_construction_names(names));
            }
            Subterm::Proj(Proj { head, .. }) => head.collect_construction_names(names),
            Subterm::InductType(InductType {
                name,
                params,
                indices,
                ..
            }) => {
                names.insert(*name);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                indices
                    .iter()
                    .for_each(|i| i.collect_construction_names(names));
            }
            Subterm::Variant(Variant {
                name,
                params,
                payload,
                ..
            }) => {
                names.insert(*name);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                payload
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::StructType(StructType { name, params, .. }) => {
                names.insert(*name);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::Struct(Struct {
                name,
                params,
                fields,
                ..
            }) => {
                names.insert(*name);
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                fields
                    .iter()
                    .for_each(|f| f.collect_construction_names(names));
            }
            Subterm::Match(Match { head, goal, cases }) => {
                head.collect_construction_names(names);
                goal.collect_construction_names(names);
                match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => {
                        false_case.collect_construction_names(names);
                        true_case.collect_construction_names(names);
                    }
                    Cases::Switch { keys, default } => {
                        keys.bodies()
                            .for_each(|b| b.collect_construction_names(names));
                        default.collect_construction_names(names);
                    }
                    Cases::Induct {
                        cases,
                        default,
                        stated,
                    } => {
                        cases
                            .iter()
                            .for_each(|(_, s)| s.body().body().collect_construction_names(names));
                        default
                            .iter()
                            .for_each(|d| d.collect_construction_names(names));
                        stated
                            .iter()
                            .for_each(|s| s.collect_construction_names(names));
                    }
                    Cases::FreeMonoid { carrier } => match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::Bytes {
                            empty_case,
                            cons_case,
                            ..
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::List {
                            elem,
                            empty_case,
                            cons_case,
                        } => {
                            elem.collect_construction_names(names);
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                    },
                }
            }
            Subterm::Let(block) => {
                let (bindings, tail) = (block.bindings(), block.tail());
                for binding in bindings {
                    binding.type_().collect_construction_names(names);
                    binding.value().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::Rec(block) => {
                let (group, tail) = (block.group(), block.tail());
                for member in group.iter() {
                    member.type_.body().collect_construction_names(names);
                    member.body.body().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
        }
    }

    /// Whether any direct child `Term` of this subterm satisfies `pred`, short-circuiting on the first hit — the shared structural walk under the cached `has_local_free`/`has_metavar` bits, which pass a child's own memoized accessor as `pred` so shared subterms are never re-walked. `Term::any_metavar` recurses over it too, so the metavariables a walk can reach are exactly the ones the cached bit knows about. Scope bodies are visited closed: binder occurrences are bound indices there, so binder labels stay invisible to any free-variable predicate.
    ///
    /// Also the descent `positivity` uses for the forms it cannot see through, with a `pred` that always returns `false` so the walk is exhaustive rather than short-circuiting. That reuse is deliberate: it is what keeps the positivity check from silently missing a recursive occurrence when a new term former is added.
    pub fn any_child_term<F: FnMut(&Term) -> bool>(&self, pred: &mut F) -> bool {
        match self {
            Subterm::Metavar(Metavar { spine, .. }) => spine.iter().any(&mut *pred),
            Subterm::Sort(_) | Subterm::Var(_) => false,
            // A head is this node's own data, as `Var`'s identity is, so an instance has no child term for a predicate to reach.
            Subterm::Instance(_) => false,
            Subterm::Transient(transient) => {
                let mut children = transient.subterms();
                children.any(&mut *pred)
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.any_term(pred),
            Subterm::Foreign(Foreign {
                arguments: args, ..
            }) => args.iter().any(&mut *pred),
            Subterm::Func(Func { telescope, .. }) => telescope.any_term(pred),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.any_term(pred),
            Subterm::Apply(apply) => pred(&apply.head) || apply.params().any(&mut *pred),
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.any_term(pred),
            Subterm::Tuple(Tuple { fields, .. }) => fields.iter().any(&mut *pred),
            Subterm::Proj(Proj { head, .. }) => pred(head),
            Subterm::InductType(InductType {
                params, indices, ..
            }) => params.iter().any(&mut *pred) || indices.iter().any(&mut *pred),
            Subterm::Variant(Variant {
                params, payload, ..
            }) => params.iter().any(&mut *pred) || payload.iter().any(&mut *pred),
            Subterm::StructType(StructType { params, .. }) => params.iter().any(&mut *pred),
            Subterm::Struct(Struct { params, fields, .. }) => {
                params.iter().any(&mut *pred) || fields.iter().any(&mut *pred)
            }
            Subterm::Match(Match { head, goal, cases }) => {
                pred(head)
                    || pred(goal)
                    || match cases {
                        Cases::Bool {
                            false_case,
                            true_case,
                        } => pred(false_case) || pred(true_case),
                        Cases::Switch { keys, default } => {
                            keys.bodies().any(&mut *pred) || pred(default)
                        }
                        Cases::Induct {
                            cases,
                            default,
                            stated,
                        } => {
                            cases.iter().any(|(_, s)| pred(s.body().body()))
                                || default.as_ref().is_some_and(&mut *pred)
                                || stated.as_ref().is_some_and(&mut *pred)
                        }
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => pred(empty_case) || pred(cons_case.body()),
                            Carrier::Bytes {
                                empty_case,
                                cons_case,
                                ..
                            } => pred(empty_case) || pred(cons_case.body()),
                            Carrier::List {
                                elem,
                                empty_case,
                                cons_case,
                            } => pred(elem) || pred(empty_case) || pred(cons_case.body()),
                        },
                    }
            }
            Subterm::Let(block) => {
                let (bindings, tail) = (block.bindings(), block.tail());
                bindings
                    .iter()
                    .any(|binding| pred(binding.type_()) || pred(binding.value()))
                    || pred(tail.body())
            }
            Subterm::Rec(block) => {
                let (group, tail) = (block.group(), block.tail());
                group
                    .iter()
                    .any(|member| pred(member.type_.body()) || pred(member.body.body()))
                    || pred(tail.body())
            }
        }
    }

    /// The newest binder occurring free in this subterm, by its index, or `None` where it holds no free variable — the uncached spelling of [`Term::newest_local`], which supplies the per-node memoization.
    pub(crate) fn newest_local(&self) -> Option<u64> {
        match self {
            Subterm::Var(var) => var.as_free().map(Free::index),
            Subterm::Metavar(Metavar { spine, .. }) => spine.newest_local(),
            _ => {
                let mut newest = None;
                self.any_child_term(&mut |child| {
                    newest = newest.max(child.newest_local());
                    false
                });
                newest
            }
        }
    }

    /// Whether a recursive group occurs in this subterm, as a `Rec` node — the uncached spelling of [`Term::has_group`], which supplies the per-node memoization.
    pub(crate) fn has_group(&self) -> bool {
        match self {
            Subterm::Rec(_) => true,
            Subterm::Metavar(Metavar { spine, .. }) => spine.has_group(),
            _ => self.any_child_term(&mut |t| t.has_group()),
        }
    }

    /// Whether any `Metavar` node occurs in this subterm — the uncached spelling of [`Term::has_metavar`], which supplies the per-node memoization.
    pub(crate) fn has_metavar(&self) -> bool {
        match self {
            Subterm::Metavar(_) => true,
            _ => self.any_child_term(&mut |t| t.has_metavar()),
        }
    }

    /// Whether this term, in weak-head normal form, is a type former: a function, record, struct or family type, or an intrinsic one ([`Intrinsic::is_type_former`]). A sort is none, being where types live and what each checker compares a child it has no type for at; neither is a neutral, which has not said what it is.
    ///
    /// Both checkers read it to refuse a literal's eta at a type that is not the literal's.
    pub fn is_type_former(&self, syntax: &SyntaxRegistry) -> bool {
        match self {
            Subterm::FuncType(_)
            | Subterm::TupleType(_)
            | Subterm::InductType(_)
            | Subterm::StructType(_) => true,
            Subterm::Intrinsic(intrinsic) => intrinsic.is_type_former(syntax),
            _ => false,
        }
    }

    /// Whether this term, in weak-head normal form, shows its type in its head: a sort, a type former, a lambda, a tuple or struct literal, a constructor's value, a host call, or an intrinsic that produces a sort or a carrier ([`Intrinsic::shows_its_type`]). Every other is a neutral, whose shape says nothing of its type: a variable, an application, a projection, a stuck elimination, a `rec` block, a universe instance, an intrinsic operation at a type its operands state.
    ///
    /// Both checkers read it to say which terms a literal's eta is taken against.
    pub fn shows_its_type(&self, syntax: &SyntaxRegistry) -> bool {
        match self {
            Subterm::Sort(_)
            | Subterm::Foreign(_)
            | Subterm::FuncType(_)
            | Subterm::Func(_)
            | Subterm::TupleType(_)
            | Subterm::Tuple(_)
            | Subterm::InductType(_)
            | Subterm::Variant(_)
            | Subterm::StructType(_)
            | Subterm::Struct(_) => true,
            Subterm::Intrinsic(intrinsic) => intrinsic.shows_its_type(syntax),
            Subterm::Apply(_)
            | Subterm::Match(_)
            | Subterm::Proj(_)
            | Subterm::Rec(_)
            | Subterm::Instance(_)
            | Subterm::Var(_) => false,
            // Reduced away before a head is compared.
            Subterm::Let(_) => false,
            // Elaboration's own: the certifier refuses them, and the elaborator solves or parks them before it compares heads.
            Subterm::Metavar(_) | Subterm::Transient(_) => false,
        }
    }

    /// Whether any elaboration-transient node occurs in this subterm — the uncached spelling of [`Term::has_transient`], which supplies the per-node memoization.
    pub(crate) fn has_transient(&self) -> bool {
        match self {
            Subterm::Transient(_) => true,
            Subterm::Metavar(Metavar { spine, .. }) => spine.has_transient(),
            _ => self.any_child_term(&mut |t| t.has_transient()),
        }
    }

    /// Whether a `Derive` transient occurs in this subterm: the mark of a derived witness body, which will reference its derivation's vocabulary without spelling it. The lowering's unused-declaration lint learns the references from the transient, since no `Var` carries them before elaboration writes the body.
    pub fn has_derive(&self) -> bool {
        match self {
            Subterm::Transient(Transient::Derive) => true,
            _ => self.any_child_term(&mut |t| t.has_derive()),
        }
    }

    pub(crate) fn has_universe_meta(&self) -> bool {
        let level_has_meta = |level: &Level| level.metas().next().is_some();
        match self {
            Subterm::Sort(Sort { quality, level }) => {
                level_has_meta(level) || matches!(quality, Quality::Meta(_))
            }
            Subterm::Intrinsic(intrinsic) => {
                intrinsic
                    .result_universes()
                    .is_some_and(Universes::has_meta)
                    || self.any_child_term(&mut |term| term.has_universe_meta())
            }
            Subterm::Instance(Instance { universes, .. })
            | Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                universes.has_meta() || self.any_child_term(&mut |term| term.has_universe_meta())
            }
            Subterm::Metavar(Metavar { spine, .. }) => spine.has_universe_meta(),
            _ => self.any_child_term(&mut |term| term.has_universe_meta()),
        }
    }

    pub(crate) fn has_universe_data(&self) -> bool {
        match self {
            // An open quality is universe data as a level is: a walk that instantiates or solves one visits the node.
            Subterm::Sort(Sort { quality, level }) => {
                level != &Level::zero() || !quality.is_ground()
            }
            Subterm::Intrinsic(intrinsic) => {
                intrinsic
                    .result_universes()
                    .is_some_and(|universes| !universes.is_empty())
                    || self.any_child_term(&mut |term| term.has_universe_data())
            }
            Subterm::Instance(Instance { universes, .. }) => !universes.is_empty(),
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                !universes.is_empty() || self.any_child_term(&mut |term| term.has_universe_data())
            }
            Subterm::Metavar(Metavar { spine, .. }) => spine.has_universe_data(),
            _ => self.any_child_term(&mut |term| term.has_universe_data()),
        }
    }

    /// This subterm's mentions, read off what its children have filled: a free `Var`'s is its own name, a reference's the declaration it names, a metavariable's is its spine's, and any other node's is its children's merged smaller into larger ([`Union`]), so a node whose children add nothing to its largest child's set holds that set itself. [`Term::get_or_init_mentions`] fills a deep spine with it bottom-up, each node paying for the names it adds.
    pub(super) fn mentions_from_children(&self) -> Mentions {
        if let Subterm::Var(var) = self
            && let Some(name) = var.as_free()
        {
            return Mentions::single(Mention::Local(*name));
        }
        // Neither an instance nor a written name has child terms to union, and a method named by its path reads its concept.
        if let Subterm::Instance(Instance { name, .. })
        | Subterm::Transient(Transient::Name(name))
        | Subterm::Transient(Transient::Method(Method { concept: name, .. })) = self
        {
            return Mentions::single(Mention::Declaration(*name));
        }
        if let Subterm::Metavar(Metavar { spine, .. }) = self {
            return spine.mentions();
        }
        let mut union = Union::default();
        self.any_child_term(&mut |child| {
            union.add(child.get_or_init_mentions());
            false
        });
        union.finish()
    }
}

impl Bound for Subterm {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Subterm::Sort(Sort { quality, level }) => Subterm::Sort(Sort {
                quality: visit.visit_quality(quality),
                level: visit.visit_level(level),
            }),
            Subterm::Intrinsic(intrinsic) => Subterm::Intrinsic(intrinsic.traverse(visit)),
            Subterm::Foreign(Foreign {
                function,
                declared,
                arguments: args,
            }) => Subterm::Foreign(Foreign {
                function: Arc::clone(function),
                declared: *declared,
                arguments: args.iter().map(|arg| visit.visit_subterm(arg)).collect(),
            }),
            Subterm::FuncType(FuncType { telescope }) => Subterm::FuncType(FuncType {
                telescope: telescope.traverse(visit),
            }),
            Subterm::Func(Func { telescope }) => Subterm::Func(Func {
                telescope: telescope.traverse(visit),
            }),
            Subterm::Transient(transient) => {
                Subterm::Transient(transient.map_subterms(&mut |child| visit.visit_subterm(child)))
            }
            Subterm::Apply(Apply { head, arguments }) => Subterm::Apply(Apply {
                head: visit.visit_subterm(head),
                arguments: arguments
                    .iter()
                    .map(|argument| Argument {
                        term: visit.visit_subterm(&argument.term),
                        plicity: argument.plicity,
                    })
                    .collect(),
            }),
            Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
                telescope: telescope.traverse(visit),
            }),
            Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                names: names.clone(),
            }),
            Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
                head: visit.visit_subterm(head),
                field: field.clone(),
            }),
            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => Subterm::InductType(InductType {
                name: *name,
                universes: visit.visit_universes(universes),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                indices: indices.iter().map(|i| visit.visit_subterm(i)).collect(),
            }),
            Subterm::Variant(Variant {
                name,
                universes,
                params,
                tag,
                payload,
            }) => Subterm::Variant(Variant {
                name: *name,
                universes: visit.visit_universes(universes),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                tag: tag.clone(),
                payload: payload.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => Subterm::StructType(StructType {
                name: *name,
                universes: visit.visit_universes(universes),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                entries,
            }) => Subterm::Struct(Struct {
                name: *name,
                universes: visit.visit_universes(universes),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                entries: entries.clone(),
            }),
            Subterm::Match(Match { head, goal, cases }) => Subterm::Match(Match {
                head: visit.visit_subterm(head),
                goal: visit.visit_subterm(goal),
                cases: match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => Cases::Bool {
                        false_case: visit.visit_subterm(false_case),
                        true_case: visit.visit_subterm(true_case),
                    },
                    Cases::Switch { keys, default } => Cases::Switch {
                        keys: keys.map(|body| visit.visit_subterm(body)),
                        default: visit.visit_subterm(default),
                    },
                    Cases::Induct {
                        cases,
                        default,
                        stated,
                    } => Cases::Induct {
                        cases: cases
                            .iter()
                            .map(|(atom, arm)| {
                                (atom.clone(), arm.with_body(visit.visit_scope(arm.body())))
                            })
                            .collect(),
                        // The default binds nothing — it lives in the enclosing scope, like `head`, and so does the instance the match states.
                        default: default.as_ref().map(|d| visit.visit_subterm(d)),
                        stated: stated.as_ref().map(|s| visit.visit_subterm(s)),
                    },
                    Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                        carrier: match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => Carrier::Nat {
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                            Carrier::Bytes {
                                empty_case,
                                cons_case,
                            } => Carrier::Bytes {
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                            Carrier::List {
                                elem,
                                empty_case,
                                cons_case,
                            } => Carrier::List {
                                elem: visit.visit_subterm(elem),
                                empty_case: visit.visit_subterm(empty_case),
                                cons_case: visit.visit_scope(cons_case),
                            },
                        },
                    },
                },
            }),
            Subterm::Let(block) => {
                let (bindings, tail) = (block.bindings(), block.tail());
                // Binding `i` sits under the `i` binders written before it, so bracket the visit at that depth; the enter/leave don't stack with `visit_scope(tail)`, which owns all the binders on its own. A forward loop over `bindings` is what a flat block buys over a nested chain — no native frame per binding.
                let bindings = bindings
                    .iter()
                    .enumerate()
                    .map(|(i, binding)| {
                        visit.enter_scope(i);
                        let out = LetBinding::new(
                            visit.visit_subterm(binding.type_()),
                            visit.visit_subterm(binding.value()),
                        );
                        visit.leave_scope(i);
                        out
                    })
                    .collect();

                Subterm::Let(Let::new(bindings, visit.visit_scope(tail)))
            }
            Subterm::Rec(block) => Subterm::Rec(Rec::new(
                block.group().traverse(visit),
                visit.visit_scope(block.tail()),
            )),
            // A reference holds no variable for the hook: only its levels are visited.
            Subterm::Instance(Instance { name, universes }) => Subterm::Instance(Instance {
                name: *name,
                universes: visit.visit_universes(universes),
            }),
            Subterm::Var(var) => visit.call(var).unwrap_or_else(|| Subterm::Var(var.clone())),
            // The spine is ordinary term content: visiting it is what keeps the delayed substitution aligned through `close`/`open`. Spines are wide (one entry per birth binder) and overwhelmingly identity (bare variables a visit does not touch), so an untouched entry is kept as it stands and an entirely untouched spine is the spine itself, identity and all.
            // A run of entries the visit would leave as they are is passed over by what the spine keeps of it, as a term is by its scalars ([`Visit::passes_over`]): a capture of the binders just opened reads the entries holding them.
            // The metavariables born under one scope share its spine, so a spine already rewritten at this depth is not rewritten again ([`Visit::spine_get`]).
            Subterm::Metavar(Metavar { id, spine, origin }) => {
                let rewritten = match visit.spine_get(spine) {
                    Some(remembered) => remembered,
                    None => {
                        let pass_over = visit.pass_over();
                        let rewritten = spine.rewritten(
                            |reach, newest_local| pass_over.passes(reach, newest_local),
                            |entry| match &**entry {
                                Subterm::Var(var) => visit.call(var).map(Term::from),
                                _ => {
                                    let rebuilt = visit.visit_subterm(entry);
                                    (rebuilt != *entry).then_some(rebuilt)
                                }
                            },
                        );
                        visit.spine_put(spine, rewritten.clone());
                        rewritten
                    }
                };
                Subterm::Metavar(Metavar {
                    id: *id,
                    spine: rewritten.unwrap_or_else(|| spine.clone()),
                    origin: origin.clone(),
                })
            }
        }
    }

    fn reach(&self) -> usize {
        match self {
            Subterm::Sort(_) => 0,
            Subterm::Transient(transient) => transient
                .subterms()
                .map(|child| child.reach())
                .fold(0, usize::max),
            Subterm::Metavar(Metavar { spine, .. }) => spine.reach(),
            // A global binds nothing a scope could reach.
            Subterm::Instance(_) => 0,
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Intrinsic(intrinsic) => intrinsic.reach(),
            Subterm::Foreign(Foreign {
                arguments: args, ..
            }) => max_reach(args),
            Subterm::Func(Func { telescope, .. }) => telescope.reach(),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.reach(),
            Subterm::Apply(apply) => apply.head.reach().max(max_reach(apply.params())),
            Subterm::TupleType(TupleType { telescope, .. }) => telescope.reach(),
            Subterm::Tuple(Tuple { fields, .. }) => max_reach(fields),
            Subterm::Proj(Proj { head, .. }) => head.reach(),
            Subterm::InductType(InductType {
                params, indices, ..
            }) => max_reach(params).max(max_reach(indices)),
            Subterm::Variant(Variant {
                params, payload, ..
            }) => max_reach(params).max(max_reach(payload)),
            Subterm::StructType(StructType { params, .. }) => max_reach(params),
            Subterm::Struct(Struct { params, fields, .. }) => {
                max_reach(params).max(max_reach(fields))
            }
            Subterm::Match(Match { head, goal, cases }) => {
                head.reach().max(goal.reach()).max(match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => false_case.reach().max(true_case.reach()),
                    Cases::Switch { keys, default } => {
                        max_reach(keys.bodies()).max(default.reach())
                    }
                    Cases::Induct {
                        cases,
                        default,
                        stated,
                    } => cases
                        .iter()
                        .map(|(_, s)| s.reach())
                        .max()
                        .unwrap_or(0)
                        .max(default.as_ref().map_or(0, |d| d.reach()))
                        .max(stated.as_ref().map_or(0, |s| s.reach())),
                    Cases::FreeMonoid { carrier } => match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => empty_case.reach().max(cons_case.reach()),
                        Carrier::Bytes {
                            empty_case,
                            cons_case,
                            ..
                        } => empty_case.reach().max(cons_case.reach()),
                        Carrier::List {
                            elem,
                            empty_case,
                            cons_case,
                        } => elem.reach().max(empty_case.reach()).max(cons_case.reach()),
                    },
                })
            }
            // Binding `i` sits under `i` binders, so its reach past the block boundary is `reach - i`; `Scope::reach` handles the tail's own arity. A flat forward max — no inner-to-outer unwind — because the block is flat, not a nest of arity-subtracting scopes.
            Subterm::Let(block) => {
                let (bindings, tail) = (block.bindings(), block.tail());
                let mut reach = tail.reach();

                for (i, binding) in bindings.iter().enumerate() {
                    reach = reach
                        .max(binding.type_().reach().saturating_sub(i))
                        .max(binding.value().reach().saturating_sub(i));
                }

                reach
            }
            Subterm::Rec(block) => block.group().reach().max(block.tail().reach()),
        }
    }

    fn has_metavar(&self) -> bool {
        Subterm::has_metavar(self)
    }

    fn has_transient(&self) -> bool {
        Subterm::has_transient(self)
    }

    fn has_universe_meta(&self) -> bool {
        Subterm::has_universe_meta(self)
    }

    fn mentions(&self) -> Mentions {
        self.mentions_from_children()
    }
}

fn max_reach<'a>(terms: impl IntoIterator<Item = &'a Term>) -> usize {
    terms
        .into_iter()
        .map(|term| term.reach())
        .max()
        .unwrap_or(0)
}
