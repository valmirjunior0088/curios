//! The term shape itself: the enum every [`Term`] derefs to, its structural queries, and its binder traversal.
//!
//! [`Term`] is the reference-counted handle and [`Subterm`] is what it points at, which is why the two are separate files rather than separate concepts. Everything that asks *what a term is* — its head, its children, whether it is a value — is answered here, and so is [`Bound`], the traversal that opens and closes every binder in the language exactly once per shape.

use super::*;

/// The actual node of the core term language — one variant per term former. [`Term`] wraps a `Subterm` in an `Rc` with cached hash/reach and an optional span, and `Deref`s here, so pattern matches are written against `Subterm` while construction goes through `Term`'s smart constructors. The final variant groups the elaboration-transient constructors under [`Transient`]: born in `into_core`, consumed by `elaborate`, never seen by reduce/convert/zonk/erase.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub enum Subterm {
    Type(Level),
    Prop,
    Intrinsic(Intrinsic),
    /// A store-described host call: the row's [`WireSignature`](curios_abi::WireSignature) fixes the operand types checked at elaboration and the result shape (unit, bare value, or named record). Effectful, so reducing one at the type level is an error; it becomes a host call only at erasure.
    ///
    /// A term former rather than a [`Intrinsic`] variant, because it is the one construct here whose meaning is *not* fixed by the enum that holds it: every intrinsic has a signature this crate spells, while a foreign call reads its own off the ABI row it carries. Nothing about it is closed, so it does not belong in a closed set.
    Foreign(Arc<ForeignFunction>, Vec<Term>),
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

impl Subterm {
    /// The group and index this term projects, when it is a member selection rather than a `rec` block with a tail of its own.
    ///
    /// On [`Subterm`] rather than [`Term`] so both reach it: a `Term` derefs here.
    pub fn as_rec_proj(&self) -> Option<(&RecGroup, usize)> {
        let Subterm::Rec(rec) = self else {
            return None;
        };

        rec.as_proj().map(|index| (&rec.group, index))
    }

    pub(super) fn any_direct_universe_meta(
        &self,
        pred: &mut impl FnMut(UniverseMetaId) -> bool,
    ) -> bool {
        let mut level_matches = |level: &Level| level.metas().any(&mut *pred);
        let context_matches =
            |context: &UniverseContext, level_matches: &mut dyn FnMut(&Level) -> bool| {
                context.constraints.iter().any(|constraint| {
                    level_matches(&constraint.lower) || level_matches(&constraint.upper)
                })
            };
        match self {
            Subterm::Type(level) => level_matches(level),
            // A projection head's group context is this node's own data now that the head is typed rather than a child term, so its constraints are direct here exactly as `Rec`'s are below.
            Subterm::Instance(Instance { head, levels }) => {
                levels.iter().any(&mut level_matches)
                    || match head {
                        InstanceHead::Var(_) => false,
                        InstanceHead::RecProj(group, _) => {
                            context_matches(group.universe_context(), &mut level_matches)
                        }
                    }
            }
            Subterm::InductType(InductType {
                universes: levels, ..
            })
            | Subterm::Variant(Variant {
                universes: levels, ..
            })
            | Subterm::StructType(StructType {
                universes: levels, ..
            })
            | Subterm::Struct(Struct {
                universes: levels, ..
            }) => levels.iter().any(level_matches),
            Subterm::Rec(Rec { group, .. }) => {
                context_matches(group.universe_context(), &mut level_matches)
            }
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

    /// The free-variable identities occurring in this subterm — the inherent-method spelling of [`Bound::free_vars`], callable without importing the trait.
    pub fn free_vars(&self) -> BTreeSet<Free> {
        <Subterm as Bound>::free_vars(self)
    }

    /// Collect every infix operator occurring in this subterm. Like [`Subterm::construction_names`], this feeds `order_flat_items`' edges: an operator dispatches through an anonymous witness of its `/syn` concept, so no `Var` can spell the dependency, and the scheduler must learn it from the operator itself. `Infix` nodes are elaboration-transient — born in `into_core`, consumed by `elaborate` — so only lowered, pre-elaboration terms have any to collect, which is exactly where the scheduler walks.
    pub fn infix_ops(&self) -> HashSet<InfixOp> {
        let mut ops = HashSet::new();
        self.collect_infix_ops(&mut ops);
        ops
    }

    fn collect_infix_ops(&self, ops: &mut HashSet<InfixOp>) {
        if let Subterm::Transient(Transient::Infix(Infix { op, .. })) = self {
            ops.insert(*op);
        }
        self.any_child_term(&mut |child| {
            child.collect_infix_ops(ops);
            false
        });
    }

    /// Collect the head name of every inductive/struct *construction* and *type-former normal form* occurring in this subterm. These names are not `Var`s (they live in the registry, not the variable graph), so they do not appear in `free_vars`; the reachability prune (`order_flat_items`) needs them as edges so a definition that *builds* a `Struct`/`Variant` (e.g. the string-literal meta-emitter's `/syn/Str/Str`) keeps the backing type-former and field-type definitions alive even when no `Var` mentions them.
    pub fn construction_names(&self) -> BTreeSet<Global> {
        let mut names = BTreeSet::new();
        self.collect_construction_names(&mut names);
        names
    }

    pub(crate) fn collect_construction_names(&self, names: &mut BTreeSet<Global>) {
        match self {
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => {}
            Subterm::Instance(Instance { head, .. }) => match head {
                InstanceHead::Var(_) => {}
                InstanceHead::RecProj(group, _) => {
                    for member in group.iter() {
                        member.type_.body().collect_construction_names(names);
                        member.body.body().collect_construction_names(names);
                    }
                }
            },
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
            Subterm::Foreign(_, args) => args
                .iter()
                .for_each(|arg| arg.collect_construction_names(names)),
            Subterm::Func(Func { telescope, .. }) => telescope.collect_construction_names(names),
            Subterm::FuncType(FuncType { telescope, .. }) => {
                telescope.collect_construction_names(names)
            }
            Subterm::Apply(Apply { head, params, .. }) => {
                head.collect_construction_names(names);
                params
                    .iter()
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
                names.insert(name.clone());
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
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                payload
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
            }
            Subterm::StructType(StructType { name, params, .. }) => {
                names.insert(name.clone());
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
                names.insert(name.clone());
                params
                    .iter()
                    .for_each(|p| p.collect_construction_names(names));
                fields
                    .iter()
                    .for_each(|f| f.collect_construction_names(names));
            }
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                head.collect_construction_names(names);
                motive.body().collect_construction_names(names);
                match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => {
                        false_case.collect_construction_names(names);
                        true_case.collect_construction_names(names);
                    }
                    Cases::Switch { cases, default } => {
                        cases
                            .values()
                            .for_each(|b| b.collect_construction_names(names));
                        default.collect_construction_names(names);
                    }
                    Cases::Induct { cases, default } => {
                        cases
                            .iter()
                            .for_each(|(_, s)| s.body.body().collect_construction_names(names));
                        default
                            .iter()
                            .for_each(|d| d.collect_construction_names(names));
                    }
                    Cases::FreeMonoid { carrier } => match carrier {
                        Carrier::Nat {
                            empty_case,
                            cons_case,
                        } => {
                            empty_case.collect_construction_names(names);
                            cons_case.body().collect_construction_names(names);
                        }
                        Carrier::Bin {
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
            Subterm::Let(Let { bindings, tail, .. }) => {
                for binding in bindings {
                    binding.type_().collect_construction_names(names);
                    binding.value().collect_construction_names(names);
                }
                tail.body().collect_construction_names(names);
            }
            Subterm::Rec(Rec { group, tail }) => {
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
            Subterm::Type(_) | Subterm::Prop | Subterm::Var(_) => false,
            // A variable head is this node's own data, like `Var`'s identity; a projection head's children are its group's scope bodies, exactly `Rec`'s minus the tail (a projection's tail is a bare member variable, which contributes to no child predicate).
            Subterm::Instance(Instance { head, .. }) => match head {
                InstanceHead::Var(_) => false,
                InstanceHead::RecProj(group, _) => group
                    .iter()
                    .any(|member| pred(member.type_.body()) || pred(member.body.body())),
            },
            Subterm::Transient(transient) => {
                let mut children = transient.subterms();
                children.any(&mut *pred)
            }
            Subterm::Intrinsic(intrinsic) => intrinsic.any_term(pred),
            Subterm::Foreign(_, args) => args.iter().any(&mut *pred),
            Subterm::Func(Func { telescope, .. }) => telescope.any_term(pred),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.any_term(pred),
            Subterm::Apply(Apply { head, params, .. }) => {
                pred(head) || params.iter().any(&mut *pred)
            }
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
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => {
                pred(head)
                    || pred(motive.body())
                    || match cases {
                        Cases::Bool {
                            false_case,
                            true_case,
                        } => pred(false_case) || pred(true_case),
                        Cases::Switch { cases, default } => {
                            cases.values().any(&mut *pred) || pred(default)
                        }
                        Cases::Induct { cases, default } => {
                            cases.iter().any(|(_, s)| pred(s.body.body()))
                                || default.as_ref().is_some_and(&mut *pred)
                        }
                        Cases::FreeMonoid { carrier } => match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => pred(empty_case) || pred(cons_case.body()),
                            Carrier::Bin {
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
            Subterm::Let(Let { bindings, tail, .. }) => {
                bindings
                    .iter()
                    .any(|binding| pred(binding.type_()) || pred(binding.value()))
                    || pred(tail.body())
            }
            Subterm::Rec(Rec { group, tail }) => {
                group
                    .iter()
                    .any(|member| pred(member.type_.body()) || pred(member.body.body()))
                    || pred(tail.body())
            }
        }
    }

    /// Whether any free variable in this subterm is a binder rather than a top-level definition — the uncached spelling of [`Term::has_local_free`], which supplies the per-node memoization.
    ///
    /// A local is a [`Free::Local`], so this is a discriminant test. It used to be a search for a marker character in the spelling, which a compiler-made *global* could set by accident — and once did.
    pub(crate) fn has_local_free(&self) -> bool {
        match self {
            Subterm::Var(var)
            | Subterm::Instance(Instance {
                head: InstanceHead::Var(var),
                ..
            }) => var.as_free().is_some_and(Free::is_local),
            _ => self.any_child_term(&mut |t| t.has_local_free()),
        }
    }

    /// Whether any `Metavar` node occurs in this subterm — the uncached spelling of [`Term::has_metavar`], which supplies the per-node memoization.
    pub(crate) fn has_metavar(&self) -> bool {
        match self {
            Subterm::Metavar(_) => true,
            _ => self.any_child_term(&mut |t| t.has_metavar()),
        }
    }

    /// Whether any elaboration-transient node occurs in this subterm — the uncached spelling of [`Term::has_transient`], which supplies the per-node memoization.
    pub(crate) fn has_transient(&self) -> bool {
        match self {
            Subterm::Transient(_) => true,
            _ => self.any_child_term(&mut |t| t.has_transient()),
        }
    }

    pub(crate) fn has_universe_meta(&self) -> bool {
        let level_has_meta = |level: &Level| level.metas().next().is_some();
        match self {
            Subterm::Type(level) => level_has_meta(level),
            Subterm::Instance(Instance { levels, .. }) => {
                levels.iter().any(level_has_meta)
                    || self.any_child_term(&mut |term| term.has_universe_meta())
            }
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                universes.iter().any(level_has_meta)
                    || self.any_child_term(&mut |term| term.has_universe_meta())
            }
            _ => self.any_child_term(&mut |term| term.has_universe_meta()),
        }
    }

    pub(crate) fn has_universe_data(&self) -> bool {
        match self {
            Subterm::Type(level) => level != &Level::zero(),
            Subterm::Instance(_) => true,
            Subterm::InductType(InductType { universes, .. })
            | Subterm::Variant(Variant { universes, .. })
            | Subterm::StructType(StructType { universes, .. })
            | Subterm::Struct(Struct { universes, .. }) => {
                !universes.is_empty() || self.any_child_term(&mut |term| term.has_universe_data())
            }
            Subterm::Rec(Rec { group, .. }) => {
                group.universe_context() != &UniverseContext::empty()
                    || self.any_child_term(&mut |term| term.has_universe_data())
            }
            _ => self.any_child_term(&mut |term| term.has_universe_data()),
        }
    }

    /// This subterm's free-variable set as its own identity (if it is a free `Var`) unioned with its children's already-memoized sets — the child-combining spelling that lets [`Term::get_or_init_free_vars`] fill a deep spine bottom-up in O(children) per node instead of re-walking the subtree. Equivalent to the whole-subtree `Bound::free_vars` walk, since a free name occurs free in exactly the nodes whose subtrees contain it.
    ///
    /// A node that adds no identity of its own and whose free variables all arrive through one child shares that child's allocation ([`FreeVars::Shared`]) instead of copying it: on a chain-shaped term every link above the one free occurrence carries the same set, and copying it per link would cost O(set) where the pass-through costs O(1). The union only materializes once a second carrying child appears.
    pub(super) fn free_vars_from_children(&self) -> FreeVars {
        // An instance's variable head is this node's own identity exactly as a bare `Var`'s is, and in both shapes a variable head means there are no child terms to union.
        if let Subterm::Var(var)
        | Subterm::Instance(Instance {
            head: InstanceHead::Var(var),
            ..
        }) = self
            && let Some(name) = var.as_free()
        {
            return FreeVars::Owned(BTreeSet::from([name.clone()]));
        }
        let mut carrier: Option<Rc<BTreeSet<Free>>> = None;
        let mut union: Option<BTreeSet<Free>> = None;
        self.any_child_term(&mut |child| {
            let frees = child.get_or_init_free_vars();
            if frees.is_empty() {
                return false;
            }
            match (&carrier, &mut union) {
                (None, _) => carrier = Some(Rc::clone(frees)),
                (Some(first), None) => {
                    let mut merged = (**first).clone();
                    merged.extend(frees.iter().cloned());
                    union = Some(merged);
                }
                (Some(_), Some(merged)) => merged.extend(frees.iter().cloned()),
            }
            false
        });
        match (carrier, union) {
            (_, Some(merged)) => FreeVars::Owned(merged),
            (Some(shared), None) => FreeVars::Shared(shared),
            (None, None) => FreeVars::Owned(BTreeSet::new()),
        }
    }
}

impl fmt::Display for Subterm {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        run_printer(
            print_term(self.clone().into(), &Rc::new(Spelling::default())),
            formatter,
            4,
        )
    }
}

impl Bound for Subterm {
    fn traverse<F>(&self, visit: &mut Visit<F>) -> Self
    where
        F: FnMut(usize, &Var) -> Option<Subterm>,
    {
        match self {
            Subterm::Type(level) => Subterm::Type(visit.visit_level(level)),
            Subterm::Prop => Subterm::Prop,
            Subterm::Intrinsic(intrinsic) => Subterm::Intrinsic(intrinsic.traverse(visit)),
            Subterm::Foreign(function, args) => Subterm::Foreign(
                Arc::clone(function),
                args.iter().map(|arg| visit.visit_subterm(arg)).collect(),
            ),
            Subterm::FuncType(FuncType {
                telescope,
                plicities,
            }) => Subterm::FuncType(FuncType {
                telescope: telescope.traverse(visit),
                plicities: plicities.clone(),
            }),
            Subterm::Func(Func {
                telescope,
                plicities,
            }) => Subterm::Func(Func {
                telescope: telescope.traverse(visit),
                plicities: plicities.clone(),
            }),
            Subterm::Transient(transient) => {
                Subterm::Transient(transient.map_subterms(&mut |child| visit.visit_subterm(child)))
            }
            Subterm::Apply(Apply {
                head,
                params,
                plicities,
            }) => Subterm::Apply(Apply {
                head: visit.visit_subterm(head),
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                plicities: plicities.clone(),
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
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
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
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                tag: tag.clone(),
                payload: payload.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => Subterm::StructType(StructType {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
            }),
            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                entries,
            }) => Subterm::Struct(Struct {
                name: name.clone(),
                universes: if visit.erases_universes() {
                    Vec::new()
                } else {
                    universes
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect()
                },
                params: params.iter().map(|p| visit.visit_subterm(p)).collect(),
                fields: fields.iter().map(|f| visit.visit_subterm(f)).collect(),
                entries: entries.clone(),
            }),
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => Subterm::Match(Match {
                head: visit.visit_subterm(head),
                motive: visit.visit_scope(motive),
                cases: match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => Cases::Bool {
                        false_case: visit.visit_subterm(false_case),
                        true_case: visit.visit_subterm(true_case),
                    },
                    Cases::Switch { cases, default } => Cases::Switch {
                        cases: cases
                            .iter()
                            .map(|(&n, body)| (n, visit.visit_subterm(body)))
                            .collect(),
                        default: visit.visit_subterm(default),
                    },
                    Cases::Induct { cases, default } => Cases::Induct {
                        cases: cases
                            .iter()
                            .map(|(atom, arm)| {
                                (atom.clone(), arm.with_body(visit.visit_scope(&arm.body)))
                            })
                            .collect(),
                        // The default binds nothing — it lives in the enclosing scope, like `head`.
                        default: default.as_ref().map(|d| visit.visit_subterm(d)),
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
                            Carrier::Bin {
                                grain,
                                empty_case,
                                cons_case,
                            } => Carrier::Bin {
                                grain: *grain,
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
            Subterm::Let(Let { bindings, tail }) => {
                // Binding `i` sits under the `i` binders written before it, so bracket the visit at that depth; the enter/leave don't stack with `visit_scope(tail)`, which owns all the binders on its own. A forward loop over `bindings` is what a flat block buys over the old nested chain — no native frame per binding.
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

                Subterm::Let(Let {
                    bindings,
                    tail: visit.visit_scope(tail),
                })
            }
            Subterm::Rec(Rec { group, tail }) => Subterm::Rec(Rec {
                group: group.traverse(visit),
                tail: visit.visit_scope(tail),
            }),
            // Erasure unwraps to the head's own spelling: the variable takes the ordinary `Var` route through the hook, and a projection rebuilds the `Rec` it abbreviates over the erased group. The projection's tail is a member variable below every hook's depth, so building it unvisited is the same as visiting it.
            Subterm::Instance(Instance { head, .. }) if visit.erases_universes() => match head {
                InstanceHead::Var(var) => {
                    visit.call(var).unwrap_or_else(|| Subterm::Var(var.clone()))
                }
                InstanceHead::RecProj(group, index) => {
                    (*Term::rec_proj(group.traverse(visit), *index)).clone()
                }
            },
            Subterm::Instance(Instance { head, levels }) => {
                let head = match head {
                    InstanceHead::Var(var) => match visit.call(var) {
                        None => InstanceHead::Var(var.clone()),
                        Some(replacement) => match InstanceHead::from_subterm(&replacement) {
                            Some(head) => head,
                            // A replacement that is not a head shape can only arrive through a binder no scheme governs — a crafted module's `let` or lambda, never an elaborated term, since locals are never generalized. The kernel types a local-headed instance as its bare head with the levels inert (`curios-cert`'s sort fixtures pin this), so substitution resolves it the same way: the instance dissolves to the replacement. Anything but a total answer here would let a hand-built module abort a reducer that promises totality on arbitrary terms.
                            None => return replacement,
                        },
                    },
                    InstanceHead::RecProj(group, index) => {
                        InstanceHead::RecProj(group.traverse(visit), *index)
                    }
                };
                Subterm::Instance(Instance {
                    head,
                    levels: levels
                        .iter()
                        .map(|level| visit.visit_level(level))
                        .collect(),
                })
            }
            Subterm::Var(var) => visit.call(var).unwrap_or_else(|| Subterm::Var(var.clone())),
            // The spine is ordinary term content: visiting it is what keeps the delayed substitution aligned through `close`/`open`. Spines are wide (one entry per birth binder) and overwhelmingly identity (bare variables a visit does not touch), so entries are copy-on-write — an untouched `Var` is an `Rc` bump, never a rebuild — and an entirely untouched spine reuses its shared allocation. This is what keeps per-traversal cost flat for the common meta instead of O(|Γ|) allocations.
            Subterm::Metavar(Metavar { id, spine, origin }) => {
                let mut touched = false;
                let visited = spine
                    .iter()
                    .map(|t| match &**t {
                        Subterm::Var(var) => match visit.call(var) {
                            Some(rewritten) => {
                                touched = true;
                                Term::from(rewritten)
                            }
                            None => t.clone(),
                        },
                        _ => {
                            let rebuilt = visit.visit_subterm(t);
                            touched = touched || rebuilt != *t;
                            rebuilt
                        }
                    })
                    .collect::<Vec<_>>();
                Subterm::Metavar(Metavar {
                    id: *id,
                    spine: match touched {
                        true => Rc::new(visited),
                        false => spine.clone(),
                    },
                    origin: origin.clone(),
                })
            }
        }
    }

    fn reach(&self) -> usize {
        match self {
            Subterm::Type(_) => 0,
            Subterm::Prop => 0,
            Subterm::Transient(transient) => transient
                .subterms()
                .map(|child| child.reach())
                .fold(0, usize::max),
            Subterm::Metavar(Metavar { spine, .. }) => max_reach(spine.as_slice()),
            // A variable head reaches like a bare `Var`; a projection head like the `Rec` it abbreviates, whose tail — a member variable under the group's own binders — contributes nothing past the block boundary.
            Subterm::Instance(Instance { head, .. }) => match head {
                InstanceHead::Var(var) => match var.as_bound() {
                    Some(index) => index + 1,
                    None => 0,
                },
                InstanceHead::RecProj(group, _) => group.reach(),
            },
            Subterm::Var(var) => match var.as_bound() {
                Some(index) => index + 1,
                None => 0,
            },
            Subterm::Intrinsic(intrinsic) => intrinsic.reach(),
            Subterm::Foreign(_, args) => max_reach(args),
            Subterm::Func(Func { telescope, .. }) => telescope.reach(),
            Subterm::FuncType(FuncType { telescope, .. }) => telescope.reach(),
            Subterm::Apply(Apply { head, params, .. }) => head.reach().max(max_reach(params)),
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
            Subterm::Match(Match {
                head,
                motive,
                cases,
            }) => head.reach().max(motive.reach()).max(match cases {
                Cases::Bool {
                    false_case,
                    true_case,
                } => false_case.reach().max(true_case.reach()),
                Cases::Switch { cases, default } => max_reach(cases.values()).max(default.reach()),
                Cases::Induct { cases, default } => cases
                    .iter()
                    .map(|(_, s)| s.reach())
                    .max()
                    .unwrap_or(0)
                    .max(default.as_ref().map_or(0, |d| d.reach())),
                Cases::FreeMonoid { carrier } => match carrier {
                    Carrier::Nat {
                        empty_case,
                        cons_case,
                    } => empty_case.reach().max(cons_case.reach()),
                    Carrier::Bin {
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
            }),
            // Binding `i` sits under `i` binders, so its reach past the block boundary is `reach - i`; `Scope::reach` handles the tail's own arity. A flat forward max — no inner-to-outer unwind — because the block is flat, not a nest of arity-subtracting scopes.
            Subterm::Let(Let { bindings, tail, .. }) => {
                let mut reach = tail.reach();

                for (i, binding) in bindings.iter().enumerate() {
                    reach = reach
                        .max(binding.type_().reach().saturating_sub(i))
                        .max(binding.value().reach().saturating_sub(i));
                }

                reach
            }
            Subterm::Rec(Rec { group, tail }) => group.reach().max(tail.reach()),
        }
    }

    fn has_metavar(&self) -> bool {
        Subterm::has_metavar(self)
    }

    fn has_transient(&self) -> bool {
        Subterm::has_transient(self)
    }
}

fn max_reach<'a>(terms: impl IntoIterator<Item = &'a Term>) -> usize {
    terms
        .into_iter()
        .map(|term| term.reach())
        .max()
        .unwrap_or(0)
}
