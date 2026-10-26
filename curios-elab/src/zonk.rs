#[cfg(test)]
mod tests;

use {
    super::{
        BirthTelescope, Context, Error, GoalReport, ReaderScope, Suggestions, callee,
        contract_redexes, refold_recs, settle_qualities, suggest_candidates,
        validate_entry_universes, written,
    },
    curios_core::{
        Apply, Argument, Bound, Carrier, Cases, ConceptDecl, Definition, Entrypoint, Extent, Func,
        FuncType, Global, InductDecl, InductParam, InductType, Instance, InstanceHead, Intrinsic,
        Item, Let, LetBinding, Match, Metavar, MetavarId, MetavarOrigin, Module, NodeMemo, Proj,
        Rec, RecGroup, RecItem, RecMemberScopes, Struct, StructDecl, StructType, Subterm,
        Telescope, Term, Tuple, TupleType, Universes, Var, Variant, Visit,
        project_erased_universes,
    },
    curios_utilities::recurse,
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        mem,
        rc::Rc,
        sync::Arc,
    },
};

/// Substitute every solved metavariable in `term` by its (recursively zonked) solution, yielding a meta-free term. An unsolved metavariable is a residual hole the elaborator never pinned down — reported as `Error::CannotInfer` at its occurrence extent. The result flows downstream to `erase`, which is then guaranteed never to meet a `Subterm::Metavar`.
///
/// Substitution replaces a metavariable node by its solution. A solution is spelled with the birth telescope's names and is *not* in general a closed term; the occurrence's spine (its delayed substitution) records what each birth binder corresponds to at the splice site, so `zonk_term` resolves by rewriting the solution through it. Every solved occurrence carries its spine — `elaborate_apply` opens telescopes with rebuilt arguments, so no bare copy of a birthed hole survives to be spliced.
pub fn zonk(context: &Context, term: &Term) -> Result<Term, Error> {
    Zonk::new(context).zonk_term(term)
}

/// One strict zonk: the context it reads, and every node it has zonked.
///
/// **A node is zonked once per pass.** What a node zonks to is a function of the node and of the context's solutions and universe solver, which the pass holds immutably, so a second occurrence of a node may take the first one's answer. A solution is spliced at every occurrence of its metavariable, and a solution stored as a reduct is a graph whose tree can be exponential in its depth: remembered by node, each is zonked in its own size and once per module rather than once per occurrence. The memo is a [`NodeMemo`], whose extent rule hands each occurrence its own extent; a refusal is not remembered, and propagates.
///
/// It derefs to the [`Context`], so the walk below reads the context through it, and hands it to anything that takes one.
struct Zonk<'a> {
    context: &'a Context<'a>,
    zonked: RefCell<NodeMemo>,
}

impl<'a> Zonk<'a> {
    fn new(context: &'a Context<'a>) -> Self {
        Self {
            context,
            zonked: RefCell::new(NodeMemo::default()),
        }
    }

    fn zonk_induct_decl(&self, induct_decl: &InductDecl) -> Result<InductDecl, Error> {
        Ok(InductDecl {
            universe_context: induct_decl.universe_context.clone(),
            arity: self.zonk_arity_within(&induct_decl.arity)?,
            constructors: induct_decl
                .constructors
                .iter()
                .map(|(tag, param)| {
                    Ok((
                        tag.clone(),
                        InductParam {
                            telescope: self.zonk_signature(&param.telescope)?,
                        },
                    ))
                })
                .collect::<Result<_, Error>>()?,
            result_sort: self.zonk_term(&induct_decl.result_sort)?,
            module: induct_decl.module,
            rep_public: induct_decl.rep_public,
            polarities: induct_decl.polarities.clone(),
            variances: induct_decl.variances.clone(),
        })
    }

    fn zonk_struct_decl(&self, struct_decl: &StructDecl) -> Result<StructDecl, Error> {
        Ok(StructDecl {
            universe_context: struct_decl.universe_context.clone(),
            arity: self.zonk_arity_within(&struct_decl.arity)?,
            result_sort: self.zonk_term(&struct_decl.result_sort)?,
            module: struct_decl.module,
            rep_public: struct_decl.rep_public,
            polarities: struct_decl.polarities.clone(),
            variances: struct_decl.variances.clone(),
        })
    }

    fn zonk_concept_decl(&self, concept: &ConceptDecl) -> Result<ConceptDecl, Error> {
        Ok(ConceptDecl {
            universe_context: concept.universe_context.clone(),
            params: self.zonk_field_telescope(&concept.params)?,
            fields: concept.fields.clone(),
            supers: concept.supers.clone(),
        })
    }

    fn zonk_item(&self, item: &Item) -> Result<Item, Error> {
        match item {
            Item::Let(def) => Ok(Item::Let(Box::new(self.zonk_definition(def)?))),
            Item::Rec(rec) => Ok(Item::Rec(RecItem::try_new(
                rec.definitions()
                    .iter()
                    .map(|def| self.zonk_definition(def))
                    .collect::<Result<Vec<_>, Error>>()?,
            )?)),
        }
    }

    fn zonk_definition(&self, def: &Definition) -> Result<Definition, Error> {
        let type_ = self.zonk_term(&def.type_)?;
        let body = self.zonk_term(&def.body)?;
        Ok(Definition {
            name: def.name,
            kind: def.kind.clone(),
            universe_context: def.universe_context.clone(),
            island: def.island,
            totality: def.totality,
            proof: def.proof,
            variances: def.variances.clone(),
            field_variances: def.field_variances.clone(),
            parameter_field: def.parameter_field,
            value_sort: def.value_sort.clone(),
            type_,
            body,
        })
    }

    /// The report a written goal `?` errors out with: the local scope frozen at its birth, the goal's type, and the solution elaboration committed (if any) — each zonked for *display*, keeping the raw spelling where unsolved holes survive (the same tolerance the no-witness report uses).
    fn goal_report(&self, id: MetavarId) -> Error {
        let display = |term: &Term| {
            settle_qualities(self, &self.zonk_term(term).unwrap_or_else(|_| term.clone()))
        };
        match self.metavar_entry(id) {
            Some(entry) => Error::goal(
                entry
                    .telescope()
                    .iter()
                    .map(|(name, type_)| (Term::free_var(name), display(type_)))
                    .collect(),
                display(entry.result()),
                self.metavar_solution(id).map(display),
            )
            .in_scope(ReaderScope::held(
                entry.frames().witnesses().map(|(name, _)| *name).collect(),
                entry.frames().clone(),
            )),
            // A goal elaboration never reached was never birthed, so there is no scope or type to report — unreachable in practice, since every kept item elaborates.
            None => Error::CannotInfer,
        }
    }

    fn zonk_term(&self, term: &Term) -> Result<Term, Error> {
        // A subtree with neither term nor universe metavariables is already fully zonked — `zonk_level`'s fast path, read before the memo so that the memo holds only what took work.
        if !term.has_metavar() && !term.has_universe_meta() {
            return Ok(term.clone());
        }
        if let Some(zonked) = self.zonked.borrow().get(term) {
            return Ok(zonked);
        }

        // The level itself, charged when it is a new peak, exactly as `reduce` charges its own bracket — see [`Context::enter_level`] and [`Cost::FRAME`], whose documentation states why the row exists: `recurse` grows the native stack rather than aborting, and nothing else bounds total depth.
        //
        // Substitution is a route into unbounded computation like any other. A metavariable whose solution reaches the metavariable again sends this walk down forever, and every level it takes is memory `recurse` asks the allocator for; uncharged, the compilation would die by exhausting the machine instead of refusing the program. Charged, the declaration's own budget decides, and the answer is a fact about the program rather than about how much memory the host had.
        self.enter_level().map_err(|error| {
            Error::from_reduce(error, |refusal| {
                Error::reduce_exhausted(term.clone(), refusal)
            })
        })?;
        // A level under a binder is read where it stands, its binder's occurrence a loose index, and a refusal there reports that term, which the printer has no name for. So each level on the way out names itself in the report instead, until one holds no loose index: the nearest term a reader can be shown.
        let zonked = self.zonk_level(term).map_err(|error| match error {
            Error::ReduceExhausted {
                term: reported,
                refusal,
            } if !reported.closed() => Error::reduce_exhausted(term.clone(), refusal),
            error => error,
        });
        self.leave_level();

        if let Ok(zonked) = &zonked {
            self.zonked.borrow_mut().put(term, zonked.clone());
        }
        zonked
    }

    /// The one choke point of the zonk walk's mutual recursion, guarded so a deeply nested term — a long sequencing chain's elaborated tail — buys depth with stack instead of overflowing the default test thread.
    fn zonk_level(&self, term: &Term) -> Result<Term, Error> {
        recurse(|| {
            // A metavariable node *is* the substitution site: replace it by its solution, recursively zonked (the solution may itself mention solved metavariables).
            if let Subterm::Metavar(Metavar { id, spine, origin }) = &**term {
                // A written goal `?` never splices — the whole point of writing it was the report. Solved or not, error with what elaboration determined: the frozen scope, the goal's type, and the solution when one landed.
                if matches!(origin, MetavarOrigin::Goal) {
                    return Err(self.goal_report(*id).at_opt(term.extent()));
                }

                // A filled recursive slot is spelled by its member's name, for the reason the tolerant walk above gives: the body reaches this very occurrence again, and the name is what the group's binder captures.
                if let Some(name) = self.rec_slot_name(*id) {
                    let member = Term::free_var(name);
                    return Ok(match term.extent() {
                        Some(extent) => member.with_extent(extent),
                        None => member,
                    });
                }

                let solution = self.metavar_solution(*id).ok_or_else(|| {
                    // An unsolved metavariable the *elaborator* minted (an omitted implicit or witness argument) is reported by the slot it filled — its binder, or its role to the callee — the provenance riding on the node itself, not as a bare hole: the user never wrote this metavariable, so a generic "cannot infer" would point at nothing they can see.
                    let error = match origin {
                        MetavarOrigin::Implicit(origin) => {
                            // The birth record's `result` is the binder's instantiated type — the bound nothing discharged, or the type nothing determined, which the record decided at the mint since this walk cannot ask; display it through whatever solutions landed, keeping the raw spelling if holes survive, exactly as the witness branch below does with its goal.
                            let entry = self.metavar_entry(*id);
                            let bound = entry
                                .map(|entry| entry.result().clone())
                                .unwrap_or_else(Term::type_ground);
                            let bound = self.zonk_term(&bound).unwrap_or(bound);
                            // Spelled for the scope the hole was born in, as a goal report's terms are, so a concept method projected off a witness reads as the method's call rather than as the witness's minted name.
                            let unsolved = Error::uninferred_implicit(
                                callee(self, &origin.func),
                                origin.binder.clone(),
                                bound,
                                entry.is_some_and(|entry| entry.proposition()),
                                entry.and_then(|entry| entry.reduct().cloned()),
                                entry.and_then(|entry| entry.refusal().cloned()),
                            );
                            // Minted in an arm no value reaches — one its scrutinee's indices rule out, or one a guard says is never taken, which was asked where the bound was given up — the hole is one no value reaches, which the report says beside it: the arm closed before this walk.
                            let unsolved = match entry
                                .and_then(|entry| entry.frames().dead_arm().or(entry.dead_arm()))
                            {
                                Some(dead) => unsolved.in_dead_arm(dead),
                                None => unsolved,
                            };
                            match entry {
                                Some(entry) => unsolved.in_scope(ReaderScope::held(
                                    entry.frames().witnesses().map(|(name, _)| *name).collect(),
                                    entry.frames().clone(),
                                )),
                                None => unsolved,
                            }
                        }
                        MetavarOrigin::Witness(origin) => {
                            // The birth record's `result` is the goal type; display it through whatever solutions landed, keeping the raw spelling if holes survive.
                            let goal = self
                                .metavar_entry(*id)
                                .map(|entry| entry.result().clone())
                                .unwrap_or_else(Term::type_ground);
                            let goal = self.zonk_term(&goal).unwrap_or(goal);
                            // No embedding or shape diagnosis on this path: both read the witness table through a mutable context to reduce, zonk holds it immutably, and a goal that survives to the splice report has already been reported richer by the resolution drains.
                            Error::no_witness(
                                goal,
                                callee(self, &origin.func),
                                origin.role.clone(),
                                None,
                                None,
                            )
                        }
                        MetavarOrigin::Hole => Error::CannotInfer,
                        MetavarOrigin::Domain(binder) => {
                            Error::domain_never_determined(binder.clone())
                        }
                        // Handled by the unconditional report above.
                        MetavarOrigin::Goal => unreachable!("a goal never reaches the splice path"),
                    };
                    // At the term the hole was minted for, where its birth was handed one: the occurrence met here may be one a substitution rebuilt, which carries no place.
                    let written = self.metavar_entry(*id).and_then(|entry| entry.written());
                    match written.or(term.extent()) {
                        Some(extent) => error.at(extent),
                        None => error,
                    }
                })?;

                // Resolve the solution in its own (named) frame first: nested solved metavariables are substituted before the spine splice below.
                let resolved = self.zonk_term(solution)?;

                // A solution that names no binder reads nothing of the spine, so the occurrence is the solution: the spine, one entry for each binder the metavariable was born under, is left unwalked.
                if !resolved.has_local_free() && resolved.reach() == 0 {
                    return Ok(match term.extent() {
                        Some(extent) => resolved.with_extent(extent),
                        None => resolved,
                    });
                }

                // A contextual occurrence: the spine records, in this site's own (already de-Bruijn-correct) form, what each birth binder corresponds to here. Zonk the spine entries (they may embed solved metavariables), then splice the solution through them — birth names captured, spine terms released.
                let entry = self
                    .metavar_entry(*id)
                    .expect("a solved metavariable has a birth entry");
                assert_eq!(
                    spine.len(),
                    entry.telescope().len(),
                    "a solved metavariable's occurrence carries its full spine"
                );
                // An identity spine reads every binder as itself, and the solution is the occurrence's as it stands.
                let zonked = match spine.is_identity() {
                    true => resolved,
                    false => {
                        let binders = entry
                            .telescope()
                            .iter()
                            .map(|(name, _)| name)
                            .collect::<Vec<_>>();
                        let spine = self.zonk_terms(&spine.iter().cloned().collect::<Vec<_>>())?;
                        let refs = spine.iter().collect::<Vec<_>>();
                        resolved.capture(&binders).release(&refs)
                    }
                };

                // Carry the hole's extent only if the solution carries none of its own.
                return Ok(match term.extent() {
                    Some(extent) => zonked.with_extent(extent),
                    None => zonked,
                });
            }

            // Fast path: a subtree with neither term nor universe metavariables is already fully zonked. A solved term metavariable's solution can be term-meta-free while still carrying levels that declaration finalization solved, so universe metas must keep descending.
            //
            // Both halves read cached per-node bits. Testing the metavariable *set* for emptiness instead answers the same question — the set is empty exactly when the bit is false — but re-walks the whole subtree at every level on the way down, which is quadratic on a deep spine.
            if !term.has_metavar() && !term.has_universe_meta() {
                return Ok(term.clone());
            }

            let inner = self.zonk_subterm(term)?;

            Ok(match term.extent() {
                Some(extent) => Term::placed(extent, inner),
                None => Term::from(inner),
            })
        })
    }

    fn zonk_terms(&self, terms: &[Term]) -> Result<Vec<Term>, Error> {
        terms.iter().map(|t| self.zonk_term(t)).collect()
    }

    /// An occurrence's instance with each level zonked and each quality read through what solved it.
    fn zonk_universes(&self, universes: &Universes) -> Result<Universes, Error> {
        Ok(Universes {
            levels: universes
                .levels
                .iter()
                .map(|level| self.universes().zonk(level).map_err(Error::from))
                .collect::<Result<_, _>>()?,
            qualities: universes
                .qualities
                .iter()
                .map(|quality| self.universes().quality(*quality))
                .collect(),
        })
    }

    fn zonk_subterm(&self, term: &Term) -> Result<Subterm, Error> {
        Ok(match &**term {
            Subterm::Sort(quality, level) => Subterm::Sort(
                self.universes().quality(*quality),
                self.universes().zonk(level).map_err(Error::from)?,
            ),
            Subterm::Foreign(function, declared, args) => {
                Subterm::Foreign(Arc::clone(function), *declared, self.zonk_terms(args)?)
            }
            Subterm::Var(var) => Subterm::Var(var.clone()),
            Subterm::Instance(instance) => Subterm::Instance(Instance {
                // A variable head holds nothing to zonk; a projection head's group is zonked through the term it abbreviates and re-classified — a zonked projection is still a projection.
                head: match &instance.head {
                    InstanceHead::Var(var) => InstanceHead::Var(var.clone()),
                    head @ InstanceHead::RecProj(..) => {
                        let zonked = self.zonk_term(&head.to_term())?;
                        InstanceHead::from_subterm(&zonked)
                            .expect("zonking a projection head yields a projection")
                    }
                },
                universes: self.zonk_universes(&instance.universes)?,
            }),

            // Transients are consumed by `elaborate` before zonk ever runs.
            Subterm::Transient(_) => unreachable!("transient node survived elaboration into zonk"),

            Subterm::Intrinsic(intrinsic) => Subterm::Intrinsic(self.zonk_intrinsic(intrinsic)?),

            Subterm::Func(func) => Subterm::Func(Func {
                telescope: self.zonk_telescope(&func.telescope)?,
            }),

            Subterm::FuncType(func_type) => Subterm::FuncType(FuncType {
                telescope: self.zonk_telescope(&func_type.telescope)?,
            }),

            Subterm::Apply(Apply { head, arguments }) => Subterm::Apply(Apply {
                head: self.zonk_term(head)?,
                arguments: arguments
                    .iter()
                    .map(|argument| {
                        Ok(Argument {
                            term: self.zonk_term(&argument.term)?,
                            plicity: argument.plicity,
                        })
                    })
                    .collect::<Result<_, Error>>()?,
            }),

            Subterm::TupleType(TupleType { telescope }) => Subterm::TupleType(TupleType {
                telescope: self.zonk_field_telescope(telescope)?,
            }),

            Subterm::Tuple(Tuple { fields, names }) => Subterm::Tuple(Tuple {
                fields: self.zonk_terms(fields)?,
                names: names.clone(),
            }),

            Subterm::Proj(Proj { head, field }) => Subterm::Proj(Proj {
                head: self.zonk_term(head)?,
                field: field.clone(),
            }),

            Subterm::InductType(InductType {
                name,
                universes,
                params,
                indices,
            }) => Subterm::InductType(InductType {
                name: *name,
                universes: self.zonk_universes(universes)?,
                params: self.zonk_terms(params)?,
                indices: self.zonk_terms(indices)?,
            }),

            Subterm::Variant(Variant {
                name,
                universes,
                params,
                tag,
                payload,
            }) => Subterm::Variant(Variant {
                name: *name,
                universes: self.zonk_universes(universes)?,
                params: self.zonk_terms(params)?,
                tag: tag.clone(),
                payload: self.zonk_terms(payload)?,
            }),

            Subterm::StructType(StructType {
                name,
                universes,
                params,
            }) => Subterm::StructType(StructType {
                name: *name,
                universes: self.zonk_universes(universes)?,
                params: self.zonk_terms(params)?,
            }),

            Subterm::Struct(Struct {
                name,
                universes,
                params,
                fields,
                entries,
            }) => Subterm::Struct(Struct {
                name: *name,
                universes: self.zonk_universes(universes)?,
                params: self.zonk_terms(params)?,
                fields: self.zonk_terms(fields)?,
                entries: entries.clone(),
            }),

            Subterm::Match(Match {
                head,
                result,
                cases,
            }) => Subterm::Match(Match {
                head: self.zonk_term(head)?,
                result: result.try_map(|b| self.zonk_term(b))?,
                cases: match cases {
                    Cases::Bool {
                        false_case,
                        true_case,
                    } => Cases::Bool {
                        false_case: self.zonk_term(false_case)?,
                        true_case: self.zonk_term(true_case)?,
                    },
                    Cases::FreeMonoid { carrier } => Cases::FreeMonoid {
                        carrier: match carrier {
                            Carrier::Nat {
                                empty_case,
                                cons_case,
                            } => Carrier::Nat {
                                empty_case: self.zonk_term(empty_case)?,
                                cons_case: cons_case.try_map_body(|b| self.zonk_term(b))?,
                            },
                            Carrier::Bytes {
                                empty_case,
                                cons_case,
                            } => Carrier::Bytes {
                                empty_case: self.zonk_term(empty_case)?,
                                cons_case: cons_case.try_map_body(|b| self.zonk_term(b))?,
                            },
                            Carrier::List {
                                elem,
                                empty_case,
                                cons_case,
                            } => Carrier::List {
                                elem: self.zonk_term(elem)?,
                                empty_case: self.zonk_term(empty_case)?,
                                cons_case: cons_case.try_map_body(|b| self.zonk_term(b))?,
                            },
                        },
                    },
                    Cases::Switch { cases, default } => Cases::Switch {
                        cases: cases
                            .iter()
                            .map(|(n, body)| Ok((n.clone(), self.zonk_term(body)?)))
                            .collect::<Result<_, Error>>()?,
                        default: self.zonk_term(default)?,
                    },
                    Cases::Induct {
                        cases,
                        default,
                        stated,
                    } => Cases::Induct {
                        cases: cases
                            .iter()
                            .map(|(atom, arm)| {
                                Ok((
                                    atom.clone(),
                                    arm.with_body(arm.body.try_map_body(|b| self.zonk_term(b))?),
                                ))
                            })
                            .collect::<Result<_, Error>>()?,
                        default: default.as_ref().map(|d| self.zonk_term(d)).transpose()?,
                        stated: stated.as_ref().map(|s| self.zonk_term(s)).transpose()?,
                    },
                },
            }),

            Subterm::Let(Let { bindings, tail }) => Subterm::Let(Let {
                bindings: bindings
                    .iter()
                    .map(|binding| {
                        Ok(LetBinding::new(
                            self.zonk_term(binding.type_())?,
                            self.zonk_term(binding.value())?,
                        ))
                    })
                    .collect::<Result<_, Error>>()?,
                tail: tail.try_map_body(|b| self.zonk_term(b))?,
            }),

            Subterm::Rec(Rec { group, tail }) => Subterm::Rec(Rec {
                group: RecGroup::new(
                    group
                        .iter()
                        .map(|member| {
                            Ok(RecMemberScopes {
                                type_: member.type_.try_map_body(|t| self.zonk_term(t))?,
                                body: member.body.try_map_body(|b| self.zonk_term(b))?,
                            })
                        })
                        .collect::<Result<_, Error>>()?,
                )
                .with_universe_context(group.universe_context().clone()),
                tail: tail.try_map_body(|b| self.zonk_term(b))?,
            }),

            // Handled in `zonk_term` before dispatch.
            Subterm::Metavar(_) => unreachable!("metavariable handled by `zonk_term`"),
        })
    }

    /// Zonk an intrinsic's term operands: collect them through the masking [`Visit`], substitute fallibly, and rebuild the node through the rewriting one. Both passes are [`Intrinsic::traverse`], the single definition of an intrinsic's operands — so the collection and the rebuild agree positionally by construction, and a new operation (or a new operand on an existing one) is covered the moment it is representable, with no per-operation arm to forget. Non-term payload (a literal, `Nat`'s spine floor) is preserved by the traversal's shape rebuild.
    fn zonk_intrinsic(&self, intrinsic: &Intrinsic) -> Result<Intrinsic, Error> {
        let mut masking = Visit::masking(|_, _: &Var| None, Term::type_ground());
        intrinsic.traverse(&mut masking);

        let zonked = masking
            .take_masked_children()
            .iter()
            .map(|operand| self.zonk_term(operand))
            .collect::<Result<Vec<_>, Error>>()?;

        let mut ready = zonked.into_iter();
        Ok(intrinsic.traverse(&mut Visit::rewriting(
            |_, _: &Var| None,
            Box::new(move |_, _| ready.next()),
        )))
    }

    /// Like [`Self::zonk_telescope`], for a constructor signature: the payload domains, and the index targets it terminates in.
    fn zonk_signature(
        &self,
        telescope: &Telescope<Vec<Term>>,
    ) -> Result<Telescope<Vec<Term>>, Error> {
        match telescope {
            Telescope::Done(targets) => Ok(Telescope::Done(Box::new(
                targets
                    .iter()
                    .map(|target| self.zonk_term(target))
                    .collect::<Result<_, Error>>()?,
            ))),
            Telescope::Cons(mark, ty, rest, ..) => Ok(Telescope::cons(
                *mark,
                self.zonk_term(ty)?,
                rest.try_map_body(|inner| self.zonk_signature(inner))?,
            )),
        }
    }

    /// Zonk a function/Π telescope (`Func`/`FuncType`): its parameter types and its trailing body/return type, which is a real term to recurse into.
    ///
    /// The pass's method rather than an inherent method on [`Telescope`]: the telescope is representation, this is the elaborator's metavariable machinery, and only the latter may name [`Context`].
    fn zonk_telescope(&self, telescope: &Telescope<Term>) -> Result<Telescope<Term>, Error> {
        match telescope {
            Telescope::Done(body) => Ok(Telescope::Done(self.zonk_term(body)?.into())),
            Telescope::Cons(mark, ty, rest, ..) => Ok(Telescope::cons(
                *mark,
                self.zonk_term(ty)?,
                rest.try_map_body(|inner| self.zonk_telescope(inner))?,
            )),
        }
    }

    fn zonk_arity_within(
        &self,
        arity: &Telescope<Telescope<()>>,
    ) -> Result<Telescope<Telescope<()>>, Error> {
        match arity {
            Telescope::Done(indices) => Ok(Telescope::Done(Box::new(
                self.zonk_field_telescope(indices)?,
            ))),
            Telescope::Cons(mark, ty, rest, ..) => Ok(Telescope::cons(
                *mark,
                self.zonk_term(ty)?,
                rest.try_map_body(|inner| self.zonk_arity_within(inner))?,
            )),
        }
    }

    /// Zonk a Σ telescope (`TupleType`): only its field types — its `Done` body is `()`, which carries no metavariables and is rebuilt as-is. The companion of [`Self::zonk_telescope`], and the pass's for the same reason.
    fn zonk_field_telescope(&self, telescope: &Telescope<()>) -> Result<Telescope<()>, Error> {
        match telescope {
            Telescope::Done(_) => Ok(Telescope::Done(Box::new(()))),
            Telescope::Cons(mark, ty, rest, ..) => Ok(Telescope::cons(
                *mark,
                self.zonk_term(ty)?,
                rest.try_map_body(|inner| self.zonk_field_telescope(inner))?,
            )),
        }
    }
}

impl<'a> std::ops::Deref for Zonk<'a> {
    type Target = Context<'a>;

    fn deref(&self) -> &Context<'a> {
        self.context
    }
}

/// One strict zonk a caller holds across many terms, so a node any of them shares is zonked once however many hold it: a declaration's recorded terms are each other's subterms, and zonked apart they cost the sum of their sizes.
///
/// Sound only while the solutions and the universe solver stand as they did at the first term, which the caller states where it holds one.
#[derive(Default)]
pub struct ZonkPass {
    zonked: NodeMemo,
}

impl ZonkPass {
    /// `term` zonked within this pass, as [`zonk`] zonks it alone.
    pub fn zonk(&mut self, context: &Context, term: &Term) -> Result<Term, Error> {
        let pass = Zonk {
            context,
            zonked: RefCell::new(mem::take(&mut self.zonked)),
        };
        let zonked = pass.zonk_term(term);
        self.zonked = pass.zonked.into_inner();
        zonked
    }
}

/// Materialize the solutions already committed for term metavariables without rejecting holes that remain legitimately deferred. Universe levels are preserved verbatim: declaration finalization rewrites them only after this pass has exposed the levels hidden in solved term-meta solutions.
pub fn zonk_solved_term_metas<B: Bound>(context: &Context, value: &B) -> B {
    if !value.has_metavar() {
        return value.clone();
    }

    type Solution = (BirthTelescope, Term);
    /// The metavariables whose solutions this walk is inside right now.
    ///
    /// Substitution cannot expand a cyclic solution graph, and elaborating a `rec` group produces one: a member's slot is filled with the body that other solutions already mention, so materializing either reaches the other forever. Left unguarded the walk simply descends, and `curios_utilities::recurse` answers a deepening walk by asking the allocator for stack — so the compilation would die by exhausting the machine rather than by refusing anything, on a route no budget prices ([`Cost::FRAME`]'s documentation states the gap: `recurse` grows the stack and nothing else bounds total depth).
    ///
    /// Leaving the re-entered occurrence unexpanded is this pass's own contract rather than a concession: it materializes what is committed and *keeps unresolved holes visible*, and a metavariable substitution cannot resolve is exactly such a hole. The strict walk, which owes a meta-free term, charges its depth instead and lets the budget refuse.
    type Active = Rc<RefCell<BTreeSet<MetavarId>>>;

    fn materialize<B: Bound>(
        value: &B,
        solutions: Rc<BTreeMap<MetavarId, Solution>>,
        active: Active,
    ) -> B {
        let rewrite_solutions = Rc::clone(&solutions);
        let rewrite_active = Rc::clone(&active);
        // Memoized on node identity, so a graph is materialized once per node and stays a graph: the candidates `convert`'s solver materializes are reducts, whose trees can be exponential in their depth, and unmemoized this walk would pay the tree. The memo is sound because the hook answers from the node alone within one visit: it reads no depth, and the active set it consults is the same at every call, since each nested materialization restores it before returning. The memo is one walk's: a solution and each element of a spine are materialized by walks of their own, so a spine that held a term, where `Term::substitute` leaves it a bare variable, would be walked once for each path to it.
        let mut visit = Visit::rewriting_shared(
            |_, _| None,
            Box::new(move |_, term| {
                // A subterm that holds no metavariable materializes to itself, and is handed back unwalked: a body closed over a lambda that still holds one metavariable deep inside costs the path to it, where each enclosing close would otherwise walk all of it again.
                if !term.has_metavar() {
                    return Some(term.clone());
                }
                let Subterm::Metavar(Metavar { id, spine, origin }) = &**term else {
                    return None;
                };
                if matches!(origin, MetavarOrigin::Goal) {
                    return None;
                }
                // An unsolved occurrence whose spine holds no metavariable has nothing to materialize: kept as it stands rather than walked an entry per binder in scope.
                let Some((labels, solution)) = rewrite_solutions.get(id) else {
                    return (!spine.has_metavar()).then(|| term.clone());
                };
                assert_eq!(
                    spine.len(),
                    labels.len(),
                    "a solved metavariable's occurrence carries its full spine"
                );
                if !rewrite_active.borrow_mut().insert(*id) {
                    return None;
                }
                let resolved = materialize(
                    solution,
                    Rc::clone(&rewrite_solutions),
                    Rc::clone(&rewrite_active),
                );
                // A solution that names no binder is the occurrence as it stands, its spine unread, as the strict zonk takes one; and so is one read through an identity spine, which reads every binder as itself.
                if spine.is_identity() || !resolved.has_local_free() && resolved.reach() == 0 {
                    rewrite_active.borrow_mut().remove(id);
                    return Some(match term.extent() {
                        Some(extent) => resolved.with_extent(extent),
                        None => resolved,
                    });
                }
                let spine = spine
                    .iter()
                    .map(|term| {
                        materialize(
                            term,
                            Rc::clone(&rewrite_solutions),
                            Rc::clone(&rewrite_active),
                        )
                    })
                    .collect::<Vec<_>>();
                rewrite_active.borrow_mut().remove(id);
                let labels = labels.iter().map(|(name, _)| name).collect::<Vec<_>>();
                let spine = spine.iter().collect::<Vec<_>>();
                let zonked = resolved.capture(&labels).release(&spine);
                Some(match term.extent() {
                    Some(extent) => zonked.with_extent(extent),
                    None => zonked,
                })
            }),
        );
        value.traverse(&mut visit)
    }

    let found = Rc::new(RefCell::new(BTreeSet::new()));
    let collected = Rc::clone(&found);
    let mut visit = Visit::rewriting(
        |_, _| None,
        // An entry's `metavars` already covers everything under it, each shared node once, so the walk stops at the entries it is handed; asked again at every node below, it would be asked once per path.
        Box::new(move |_, term| {
            collected.borrow_mut().extend(term.metavars());
            Some(term.clone())
        }),
    );
    let _: B = value.traverse(&mut visit);
    let mut solutions = BTreeMap::new();
    let mut seen = BTreeSet::new();
    let mut pending = found.borrow().iter().copied().collect::<Vec<_>>();
    while let Some(id) = pending.pop() {
        if !seen.insert(id) {
            continue;
        }
        let Some(entry) = context.metavar_entry(id) else {
            continue;
        };
        let Some(solution) = entry.solution() else {
            continue;
        };
        // A filled recursive slot stands for its member, and the member is spelled by its name: substituting the *body* is what closes the cycle, since the body reaches the very solution being materialized. The name is what `RecItem::try_new` captures, so this is also the spelling the group's binder is waiting for. Its body is deliberately not walked — nothing in it needs materializing here, and walking it is the divergence.
        if let Some(name) = context.rec_slot_name(id) {
            // The birth telescope is kept so the occurrence's spine still matches, as every entry here must. A member's name names no binder, so the spine is never read: a member's value does not depend on the slot's spine.
            solutions.insert(id, (entry.telescope().clone(), Term::free_var(name)));
            continue;
        }
        pending.extend(solution.metavars());
        solutions.insert(id, (entry.telescope().clone(), solution.clone()));
    }
    if solutions.is_empty() {
        return value.clone();
    }
    materialize(
        value,
        Rc::new(solutions),
        Rc::new(RefCell::new(BTreeSet::new())),
    )
}

/// What one finished item leaves for every other declaration to read, meta-free: the item, and the registry entries its names declare.
pub struct Published {
    pub item: Item,
    pub induct_decls: Vec<(Global, InductDecl)>,
    pub struct_decls: Vec<(Global, StructDecl)>,
    pub concepts: Vec<(Global, ConceptDecl)>,
}

/// Zonk one finished item and the registry entries it declares: strictly, so what is published holds no metavariable of the declaration that wrote it, and a hole the declaration left unsolved is refused where it stands rather than once every item has elaborated.
pub fn zonk_published(context: &Context, item: &Item) -> Result<Published, Error> {
    let zonk = &Zonk::new(context);
    let published = zonk.zonk_item(item)?;
    let mut induct_decls = Vec::new();
    let mut struct_decls = Vec::new();
    let mut concepts = Vec::new();
    for name in item.declared_names() {
        if let Some(declaration) = context.induct_decl(name) {
            induct_decls.push((*name, zonk.zonk_induct_decl(declaration)?));
        }
        if let Some(declaration) = context.struct_decl(name) {
            struct_decls.push((*name, zonk.zonk_struct_decl(declaration)?));
        }
        if let Some(concept) = context.concept(name) {
            concepts.push((*name, zonk.zonk_concept_decl(concept)?));
        }
    }

    Ok(Published {
        item: published,
        induct_decls,
        struct_decls,
        concepts,
    })
}

/// Zonk a program's entry — its body and the type it was judged at — and validate its universes against the schemes `module`, the program's zonked module, declares.
pub fn zonk_entry(
    context: &Context,
    module: &Module,
    entry: &Entrypoint,
) -> Result<Entrypoint, Error> {
    let context = &Zonk::new(context);
    let entry = Entrypoint {
        body: context.zonk_term(&entry.body)?,
        type_: entry
            .type_
            .as_ref()
            .map(|type_| context.zonk_term(type_))
            .transpose()?,
    };
    validate_entry_universes(module, &entry)?;
    Ok(entry)
}

/// Where a written goal was met: its id, its first occurrence's extent, and its owning definition when it sits inside one (the suggestion pools exclude the owner).
pub type GoalSite = (MetavarId, Option<Extent>, Option<Global>);

/// Find every written goal one successful elaboration reached — the same set strict zonking would meet — in discovery order.
///
/// The walk covers what strict zonking covers, in its order (items in declaration order, then the entrypoint body and annotation, then the registry telescopes that flow into erase), recording each `Goal`-origin metavariable once with its first occurrence's extent. Goals can also hide inside committed solutions of ordinary metavariables the module references — strict zonk would splice through them — so referenced solutions are scanned transitively afterwards, in discovery order.
pub fn goal_sites(context: &Context, module: &Module, entry: Option<&Entrypoint>) -> Vec<GoalSite> {
    /// The sites collected so far, shared into the scan closures.
    type GoalSites = Rc<RefCell<Vec<GoalSite>>>;

    let goals: GoalSites = Rc::new(RefCell::new(Vec::new()));
    let seen_goals = Rc::new(RefCell::new(BTreeSet::new()));
    // Append-only discovery log of ordinary metavariables, drained below by index so solution scans that append more keep a stable order.
    let referenced: Rc<RefCell<Vec<MetavarId>>> = Rc::new(RefCell::new(Vec::new()));

    fn scan<B: Bound>(
        value: &B,
        owner: Option<&Global>,
        goals: &GoalSites,
        seen_goals: &Rc<RefCell<BTreeSet<MetavarId>>>,
        referenced: &Rc<RefCell<Vec<MetavarId>>>,
    ) {
        let goals = Rc::clone(goals);
        let seen_goals = Rc::clone(seen_goals);
        let referenced = Rc::clone(referenced);
        let owner = owner.cloned();
        // Memoized on node identity: a goal is latched by its first occurrence, which a memoized walk still reaches first, and the log of the rest is drained through `scanned`, so logging each node once rather than each path through it changes nothing it yields. A definition can hold a solution stored as a reduct, whose tree can be exponential in its depth.
        let mut visit = Visit::rewriting_shared(
            |_, _| None,
            Box::new(move |_, term| {
                if let Subterm::Metavar(Metavar { id, origin, .. }) = &**term {
                    match origin {
                        MetavarOrigin::Goal => {
                            if seen_goals.borrow_mut().insert(*id) {
                                goals.borrow_mut().push((*id, term.extent(), owner));
                            }
                        }
                        _ => referenced.borrow_mut().push(*id),
                    }
                }
                None
            }),
        );
        let _: B = value.traverse(&mut visit);
    }

    let mut scan_definition = |def: &Definition| {
        scan(
            &def.type_,
            Some(&def.name),
            &goals,
            &seen_goals,
            &referenced,
        );
        scan(&def.body, Some(&def.name), &goals, &seen_goals, &referenced);
    };
    for item in &module.items {
        match item {
            Item::Let(def) => scan_definition(def),
            Item::Rec(rec) => rec.definitions().iter().for_each(&mut scan_definition),
        }
    }
    if let Some(entry) = entry {
        scan(&entry.body, None, &goals, &seen_goals, &referenced);
        if let Some(type_) = &entry.type_ {
            scan(type_, None, &goals, &seen_goals, &referenced);
        }
    }
    for induct_decl in module.induct_decls().values() {
        scan(&induct_decl.arity, None, &goals, &seen_goals, &referenced);
        for (_, param) in &induct_decl.constructors {
            scan(&param.telescope, None, &goals, &seen_goals, &referenced);
        }
        scan(
            &induct_decl.result_sort,
            None,
            &goals,
            &seen_goals,
            &referenced,
        );
    }
    for struct_decl in module.struct_decls().values() {
        scan(&struct_decl.arity, None, &goals, &seen_goals, &referenced);
        scan(
            &struct_decl.result_sort,
            None,
            &goals,
            &seen_goals,
            &referenced,
        );
    }
    for concept in module.concepts().values() {
        scan(&concept.params, None, &goals, &seen_goals, &referenced);
    }

    let mut scanned = BTreeSet::new();
    let mut index = 0;
    loop {
        let id = {
            let referenced = referenced.borrow();
            match referenced.get(index) {
                Some(id) => *id,
                None => break,
            }
        };
        index += 1;
        if !scanned.insert(id) {
            continue;
        }
        if let Some(solution) = context.metavar_solution(id) {
            scan(solution, None, &goals, &seen_goals, &referenced);
        }
    }

    goals.take()
}

/// Report the goals met at `sites` ([`goal_sites`]) as display-ready reports, in the state they were written in — the only one that knows them — each with the candidates that fit it, drawn from `items`, the unit's items as they elaborated, and a program's entry.
///
/// Each report's scope, type, and solution render through the tolerant [`zonk_solved_term_metas`], so committed substitutions appear while goal-origin and unsolved metavariables stay visible as neutral terms; each is then written as a program writes it where the goal stands ([`written`](fn@crate::written)), standing on the frames the goal was born under, and its universe instances are erased ([`project_erased_universes`]), so every reported term is spelled the way the source could write it. An unsolved goal additionally carries sandboxed candidate suggestions ([`suggest_candidates`]), displayed through the same pipeline.
pub fn report_goals(
    context: &mut Context,
    sites: &[GoalSite],
    items: &[Item],
    entry: Option<&Entrypoint>,
) -> Vec<GoalReport> {
    // Suggestions run first, on the mutable context — each attempt sandboxed and rolled back — before the display phase borrows it immutably. Solved goals get none: a suggestion beside a `? =` answer is noise. `restore_budget` puts the attempts on the same footing as the finalization passes.
    context.restore_budget();
    let mut all_candidates: Vec<Suggestions> = Vec::with_capacity(sites.len());
    for (id, _, owner) in sites {
        if context.metavar_solution(*id).is_some() {
            all_candidates.push(Suggestions {
                candidates: Vec::new(),
                refusal: None,
            });
            continue;
        }
        let (telescope, refinements, result) = {
            let entry = context
                .metavar_entry(*id)
                .expect("a collected goal has a birth entry");
            (
                entry.telescope().clone(),
                entry.frames().clone(),
                entry.result().clone(),
            )
        };
        all_candidates.push(suggest_candidates(
            context,
            &telescope.iter().cloned().collect::<Vec<_>>(),
            &refinements,
            &result,
            items,
            entry,
            owner.as_ref(),
        ));
    }

    // Materialize committed substitutions tolerantly, then write each term as a program writes it where the goal stands, standing on the frames it was born under — a `use` argument resolution puts back there left out, a method called as a program calls it — and erase universe instances (the surface language cannot even spell `.{…}`, so a report never shows one — solved or unsolved). A lambda applied where it stands is contracted, since a solution is a reduct and may hold one no reader could write back (`contract_redexes`).
    let solved = |context: &Context, term: &Term| {
        settle_qualities(context, &zonk_solved_term_metas(context, term))
    };
    let shown = |context: &Context, term: &Term| {
        contract_redexes(&refold_recs(context, &project_erased_universes(term)))
    };
    let mut reports = Vec::with_capacity(sites.len());
    for ((id, extent, owner), suggestions) in sites.iter().zip(all_candidates) {
        // Every goal occurrence in the elaborated module was rebuilt by `elaborate_metavar`, which births on first sight in either mode — so the entry exists.
        let entry = context
            .metavar_entry(*id)
            .expect("a collected goal has a birth entry");
        let frames = entry.frames().clone();
        let witnesses = entry.frames().witnesses().map(|(name, _)| *name).collect();
        let mut scope = entry
            .telescope()
            .iter()
            .map(|(name, type_)| (Term::free_var(name), solved(context, type_)))
            .collect::<Vec<_>>();
        let mut goal = solved(context, entry.result());
        let mut solution = context
            .metavar_solution(*id)
            .map(|solution| solved(context, solution));
        // The drain's surrendered conversions this goal holds up, already in display form; the batch's own pipeline re-runs over them harmlessly.
        let mut obligations = context
            .goal_obligations()
            .iter()
            .filter(|obligation| obligation.goals.contains(id))
            .map(|obligation| {
                (
                    solved(context, &obligation.this),
                    solved(context, &obligation.that),
                )
            })
            .collect::<Vec<_>>();
        let mut candidates = suggestions
            .candidates
            .iter()
            .map(|candidate| solved(context, candidate))
            .collect::<Vec<_>>();
        let mut refusal = suggestions.refusal;

        context.reporting(|context| {
            context.standing_on(&frames, |context| {
                let terms = scope
                    .iter_mut()
                    .map(|(_, type_)| type_)
                    .chain([&mut goal])
                    .chain(solution.as_mut())
                    .chain(obligations.iter_mut().flat_map(|(this, that)| [this, that]))
                    .chain(candidates.iter_mut())
                    .chain(refusal.iter_mut().flat_map(|refusal| refusal.terms_mut()));
                for term in terms {
                    *term = written(context, term);
                }
            });
        });

        reports.push(
            GoalReport::new(
                *extent,
                *owner,
                witnesses,
                scope
                    .iter()
                    .map(|(name, type_)| (name.clone(), shown(context, type_)))
                    .collect(),
                shown(context, &goal),
                solution.map(|solution| shown(context, &solution)),
            )
            .beside(
                obligations
                    .iter()
                    .map(|(this, that)| (shown(context, this), shown(context, that)))
                    .collect(),
                candidates
                    .iter()
                    .map(|candidate| shown(context, candidate))
                    .collect(),
                refusal,
            ),
        );
    }

    reports
}

/// Like [`Zonk::zonk_field_telescope`], for a declaration's nested arity: the parameter domains, then the telescope they terminate in — a family's indices, a structure's fields.
pub fn zonk_arity(
    context: &Context,
    arity: &Telescope<Telescope<()>>,
) -> Result<Telescope<Telescope<()>>, Error> {
    Zonk::new(context).zonk_arity_within(arity)
}
