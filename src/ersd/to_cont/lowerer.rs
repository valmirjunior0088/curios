use {
    super::{
        Backpatch, Cont, ContMany, Emit, Frame, FrameEntropy, RecBody, RegionBuilder, free_names,
        lower_pure_prim, lower_value_prim, rec_computed_order, unsupported_sync_rec_item,
    },
    crate::{Entropy, cont, ersd},
    std::collections::{BTreeMap, HashMap},
};

#[derive(Debug)]
pub struct Lowerer<'a> {
    module: &'a mut cont::Module,
    clsrs: Entropy<cont::ClsrName>,
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a mut cont::Module) -> Self {
        Self {
            module,
            clsrs: Entropy::<cont::ClsrName>::new(),
        }
    }

    pub fn lower_module(
        &mut self,
        module: &ersd::Module,
        frame: &Frame,
    ) -> (cont::BlockName, cont::Region) {
        let (mut entry, resume) = FrameEntropy::new();
        let mut emit = Emit::new(&mut entry);
        let tail = Work {
            lowerer: self,
            emit: &mut emit,
        }
        .lower_module_items(&module.items, &module.body, frame.clone(), &resume);

        (resume, emit.finish(tail))
    }
}

/// Whether a top-level `let` body can be lowered *synchronously* by
/// `lower_pure_name` — i.e. it produces a value with no resume block, so its
/// continuation runs in the same stack frame. Conservatively limited to the forms
/// that are pure at any depth (a `Func` always is: its body lowers into its own
/// region). Everything else (prims, tuples, projections, and computational
/// `Apply`/`Match`/`NatMatch`) takes the CPS path in `lower_module_items`.
///
/// This is what keeps the flat-module lowering off the native stack: the prelude
/// is overwhelmingly functions and (erased) types, all synchronous, so they fold
/// into a flat loop; only the rare non-synchronous top-level `let` recurses.
fn is_synchronous(term: &ersd::Term) -> bool {
    matches!(
        &**term,
        ersd::Subterm::Func(_)
            | ersd::Subterm::Erased
            | ersd::Subterm::Unreachable
            | ersd::Subterm::Atom(_)
            | ersd::Subterm::Name(_)
    )
}

/// A unit of lowering work: the shared [`Lowerer`] (module + closure names) paired with the
/// region currently being emitted into. Threading one `Work` instead of a `Lowerer` and an
/// `Emit` separately keeps the per-primitive helpers down to their operands plus a continuation.
pub struct Work<'a, 'm, 'e> {
    lowerer: &'a mut Lowerer<'m>,
    emit: &'a mut Emit<'e>,
}

impl Work<'_, '_, '_> {
    /// Build a nested region with its own `Work`, sharing this work's `Lowerer` and name supply.
    fn in_subregion(&mut self, build: impl FnOnce(&mut Work) -> cont::Tail) -> cont::Region {
        let mut emit = self.emit.subregion();
        let tail = build(&mut Work {
            lowerer: &mut *self.lowerer,
            emit: &mut emit,
        });

        emit.finish(tail)
    }

    /// Allocate a fresh value in the current region. Thin accessor so the per-primitive lowering
    /// in `lower_prim` can emit values without reaching into the private `emit` field.
    pub fn fresh(&mut self, value: cont::Value) -> cont::ValueName {
        self.emit.fresh(value)
    }

    /// Mint a fresh block name. The block isn't bound to the current region yet —
    /// the caller is expected to follow up with [`Self::add_resume_block`] (or
    /// equivalent) once the block's region is ready.
    pub fn fresh_block(&mut self) -> cont::BlockName {
        self.emit.fresh_block()
    }

    /// Mint a fresh value name without binding it to the current region. Used
    /// when the name is needed as a block parameter (bound by control flow) or
    /// passed to a continuation before the binding lands in some other region.
    pub fn fresh_value(&mut self) -> cont::ValueName {
        self.emit.fresh_value()
    }

    /// Add a block to the current region whose region is built by `build` inside a
    /// fresh subregion. Used when emitting a tail (`Tail::Host`, `Tail::Call`, …)
    /// that branches to a synthesized continuation: the caller mints the block
    /// name, supplies the block's params, and provides a closure that emits the
    /// continuation's region against an outer `Cont`.
    pub fn add_resume_block(
        &mut self,
        name: cont::BlockName,
        params: Vec<cont::ValueName>,
        build: impl FnOnce(&mut Work) -> cont::Tail,
    ) {
        let region = self.in_subregion(build);
        self.emit.add_block(name, cont::Block { params, region });
    }

    pub fn lower_closure(
        &mut self,
        func: &ersd::Func,
        frame: &Frame,
    ) -> (cont::ClsrName, Vec<cont::ValueName>) {
        let clsr_name = self.lowerer.clsrs.fresh();
        let (mut entry, resume) = FrameEntropy::new();
        let mut clsr_frame = Frame::new();

        // The candidate flag rides from the `ersd::Argument` straight into the
        // `cont::Argument`, glued to the freshly-bound name.
        let fields = func
            .captures
            .iter()
            .map(|capture| {
                let name = entry.fresh_value();
                clsr_frame.push(capture.name.clone(), name.clone());

                cont::Argument {
                    name,
                    candidate: capture.candidate,
                }
            })
            .collect::<Vec<_>>();

        let params = func
            .params
            .iter()
            .map(|param| {
                let name = entry.fresh_value();
                clsr_frame.push(param.name.clone(), name.clone());

                cont::Argument {
                    name,
                    candidate: param.candidate,
                }
            })
            .collect::<Vec<_>>();

        let mut emit = Emit::new(&mut entry);
        let tail = Work {
            lowerer: &mut *self.lowerer,
            emit: &mut emit,
        }
        .lower_tail(&func.body, &clsr_frame, &resume);
        let region = emit.finish(tail);

        self.lowerer.module.add_clsr(
            clsr_name.clone(),
            cont::Clsr {
                fields,
                params,
                resume,
                region,
            },
        );

        let captured_values = func
            .captures
            .iter()
            .map(|capture| frame.find(&capture.name))
            .collect();

        (clsr_name, captured_values)
    }

    /// Lower a `rec` group's bindings into `frame` synchronously (no resume blocks)
    /// and return the extended frame. Shared by local `Subterm::Rec` lowering and
    /// the flat top-level `ersd::Item::Rec`, so it takes the `names` and `items`
    /// directly rather than an `ersd::Rec` (whose `tail` it never used).
    pub fn lower_letrec_bindings<'x>(
        &mut self,
        names: &[String],
        items: impl IntoIterator<Item = &'x ersd::Term>,
        frame: &Frame,
    ) -> Frame {
        let mut frame = frame.clone();
        let reserved = names
            .iter()
            .map(|name| {
                let reserved = self.emit.fresh_value();
                frame.push(name.clone(), reserved.clone());

                reserved
            })
            .collect::<Vec<_>>();

        for (item, target) in items.into_iter().zip(reserved) {
            match self.plan_backpatch(item, &frame) {
                Some(backpatch) => {
                    self.emit.add_prealloc(target.clone(), backpatch.prealloc());
                    self.emit_backpatch(target, &backpatch, &frame);
                }
                None => match &**item {
                    ersd::Subterm::Apply(_)
                    | ersd::Subterm::Match(_)
                    | ersd::Subterm::NatMatch(_) => unsupported_sync_rec_item(item),
                    _ => self.lower_letrec_item(item, target, &frame),
                },
            }
        }

        frame
    }

    pub fn lower_pure_name(&mut self, term: &ersd::Term, frame: &Frame) -> cont::ValueName {
        match &**term {
            ersd::Subterm::Name(name) => frame.find(name.as_str()),
            ersd::Subterm::Erased => self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![]))),
            ersd::Subterm::Unreachable => {
                panic!("unreachable Ersd term cannot be lowered in pure-name position")
            }
            ersd::Subterm::Prim(ersd::Prim::Pure(pure)) => lower_pure_prim(self, pure, frame),
            ersd::Subterm::Prim(ersd::Prim::Host(_)) => unreachable!(
                "host primitive reached pure-name context — pure-name lowering cannot \
                 construct the resume block required by Tail::Host"
            ),
            ersd::Subterm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);

                self.emit.fresh(cont::Value::Pure(cont::Data::Clsr(
                    clsr_name,
                    captured_values,
                )))
            }
            ersd::Subterm::Tuple(s) => {
                let field_names = s
                    .fields
                    .iter()
                    .map(|f| self.lower_pure_name(f, frame))
                    .collect::<Vec<_>>();

                self.emit
                    .fresh(cont::Value::Pure(cont::Data::Tpl(field_names)))
            }
            ersd::Subterm::Atom(atom) => self
                .emit
                .fresh(cont::Value::Pure(cont::Data::Nat(atom.index as u32))),
            ersd::Subterm::Let(let_) => {
                let body = self.lower_pure_name(&let_.body, frame);
                let frame = frame.extended([(let_.name.clone(), body)]);

                self.lower_pure_name(&let_.tail, &frame)
            }
            ersd::Subterm::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(&letrec.names, letrec.items.iter(), frame);

                self.lower_pure_name(&letrec.tail, &frame)
            }
            ersd::Subterm::Proj(proj) => {
                let head = self.lower_pure_name(&proj.head, frame);

                self.emit
                    .fresh(cont::Value::Eval(cont::Code::TplGet(head, proj.index)))
            }
            ersd::Subterm::Apply(_) | ersd::Subterm::Match(_) | ersd::Subterm::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
        }
    }

    pub fn lower_letrec_item(&mut self, term: &ersd::Term, target: cont::ValueName, frame: &Frame) {
        match &**term {
            ersd::Subterm::Apply(_) | ersd::Subterm::Match(_) | ersd::Subterm::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
            _ => {
                let value = self.lower_pure_name(term, frame);
                self.emit.add_value(target, cont::Value::Alias(value));
            }
        }
    }

    pub fn lower_value_name(
        &mut self,
        term: &ersd::Term,
        frame: &Frame,
        cont: Cont<'_>,
    ) -> cont::Tail {
        match &**term {
            ersd::Subterm::Name(name) => cont.call(self, frame.find(name.as_str())),
            ersd::Subterm::Erased => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(vec![])));

                cont.call(self, value)
            }
            ersd::Subterm::Unreachable => cont::Tail::Unreachable,
            ersd::Subterm::Prim(prim) => lower_value_prim(self, prim, frame, cont),
            ersd::Subterm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Clsr(
                    clsr_name,
                    captured_values,
                )));

                cont.call(self, value)
            }
            ersd::Subterm::Tuple(s) => self.lower_struct(&s.fields, frame, vec![], cont),
            ersd::Subterm::Atom(atom) => {
                let value = self
                    .emit
                    .fresh(cont::Value::Pure(cont::Data::Nat(atom.index as u32)));

                cont.call(self, value)
            }
            ersd::Subterm::Let(let_) => {
                let name = let_.name.clone();

                self.lower_value_name(
                    &let_.body,
                    frame,
                    Cont::new(move |work, body| {
                        let frame = frame.extended([(name, body)]);

                        work.lower_value_name(&let_.tail, &frame, cont)
                    }),
                )
            }
            ersd::Subterm::Rec(letrec) => self.lower_rec(
                &letrec.names,
                letrec.items.iter(),
                frame,
                RecBody::new(move |work, frame| work.lower_value_name(&letrec.tail, frame, cont)),
            ),
            ersd::Subterm::Proj(proj) => {
                let index = proj.index;

                self.lower_value_name(
                    &proj.head,
                    frame,
                    Cont::new(move |work, head| {
                        let v = work
                            .emit
                            .fresh(cont::Value::Eval(cont::Code::TplGet(head, index)));

                        cont.call(work, v)
                    }),
                )
            }
            ersd::Subterm::Apply(_) | ersd::Subterm::Match(_) | ersd::Subterm::NatMatch(_) => {
                let block = self.emit.fresh_block();
                let param = self.emit.fresh_value();
                let region = self.in_subregion(|work| cont.call(work, param.clone()));

                self.emit.add_block(
                    block.clone(),
                    cont::Block {
                        params: vec![param],
                        region,
                    },
                );

                self.lower_tail(term, frame, &block)
            }
        }
    }

    pub fn lower_names<'b>(
        &mut self,
        params: &'b [ersd::Term],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: ContMany<'b>,
    ) -> cont::Tail {
        match params {
            [] => cont.call(self, names),
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Cont::new(move |work, name| {
                    names.push(name);
                    work.lower_names(tail, frame, names, cont)
                }),
            ),
        }
    }

    fn lower_struct<'b>(
        &mut self,
        fields: &'b [ersd::Term],
        frame: &'b Frame,
        mut names: Vec<cont::ValueName>,
        cont: Cont<'b>,
    ) -> cont::Tail {
        match fields {
            [] => {
                let value = self.emit.fresh(cont::Value::Pure(cont::Data::Tpl(names)));

                cont.call(self, value)
            }
            [head, tail @ ..] => self.lower_value_name(
                head,
                frame,
                Cont::new(move |work, name| {
                    names.push(name);
                    work.lower_struct(tail, frame, names, cont)
                }),
            ),
        }
    }

    /// Classify a `rec` item: backpatches (`Func`/`Tuple`/`Arr`) get a prealloc'd shell so
    /// their identity is available before their fields; a `Func` is lowered here so its
    /// `ClsrName` is fixed. Everything else is "computed" (lowered via `lower_value_name`).
    pub fn plan_backpatch<'b>(
        &mut self,
        item: &'b ersd::Term,
        frame: &Frame,
    ) -> Option<Backpatch<'b>> {
        match &**item {
            ersd::Subterm::Func(func) => {
                let (clsr_name, captures) = self.lower_closure(func, frame);

                Some(Backpatch::Clsr(clsr_name, captures))
            }
            ersd::Subterm::Tuple(tuple) => Some(Backpatch::Tpl(&tuple.fields)),
            ersd::Subterm::Prim(ersd::Prim::Pure(ersd::PurePrim::Arr(elems))) => {
                Some(Backpatch::Arr(elems))
            }
            _ => None,
        }
    }

    pub fn emit_backpatch(
        &mut self,
        target: cont::ValueName,
        backpatch: &Backpatch,
        frame: &Frame,
    ) {
        match backpatch {
            Backpatch::Clsr(clsr_name, captures) => self.emit.add_value(
                target,
                cont::Value::Pure(cont::Data::Clsr(clsr_name.clone(), captures.clone())),
            ),
            Backpatch::Tpl(fields) => {
                let names = fields
                    .iter()
                    .map(|field| self.lower_pure_name(field, frame))
                    .collect();

                self.emit
                    .add_value(target, cont::Value::Pure(cont::Data::Tpl(names)));
            }
            Backpatch::Arr(elems) => {
                let names = elems
                    .iter()
                    .map(|elem| self.lower_pure_name(elem, frame))
                    .collect();

                self.emit
                    .add_value(target, cont::Value::Pure(cont::Data::Arr(names)));
            }
        }
    }

    /// Lower a `rec` group, then `body`. Backpatches are prealloc'd at region entry; call/match
    /// -valued bindings are lowered in dependency order through resume blocks; patches
    /// (which may reference those results) run last, just before `body`.
    pub fn lower_rec<'b>(
        &mut self,
        names: &'b [String],
        items: impl IntoIterator<Item = &'b ersd::Term>,
        frame: &Frame,
        body: RecBody<'b>,
    ) -> cont::Tail {
        let mut frame = frame.clone();
        let targets = names
            .iter()
            .map(|name| {
                let target = self.emit.fresh_value();
                frame.push(name.clone(), target.clone());

                target
            })
            .collect::<Vec<_>>();

        let mut backpatches: Vec<(cont::ValueName, Backpatch<'b>)> = vec![];
        let mut computed: Vec<(usize, cont::ValueName, &'b ersd::Term)> = vec![];

        for (index, (item, target)) in items.into_iter().zip(&targets).enumerate() {
            match self.plan_backpatch(item, &frame) {
                Some(backpatch) => backpatches.push((target.clone(), backpatch)),
                None => computed.push((index, target.clone(), item)),
            }
        }

        let computed_names = computed
            .iter()
            .map(|(index, _, _)| names[*index].as_str())
            .collect::<Vec<_>>();

        let name_to_pos = computed_names
            .iter()
            .enumerate()
            .map(|(pos, name)| (*name, pos))
            .collect::<HashMap<_, _>>();

        let deps = computed
            .iter()
            .map(|(_, _, rhs)| {
                free_names(rhs)
                    .iter()
                    .filter_map(|name| name_to_pos.get(name.as_str()).copied())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let order = rec_computed_order(&computed_names, &deps);

        for (target, backpatch) in &backpatches {
            self.emit.add_prealloc(target.clone(), backpatch.prealloc());
        }

        let sorted = order
            .into_iter()
            .map(|pos| {
                let (_, target, rhs) = &computed[pos];
                (target.clone(), *rhs)
            })
            .collect::<Vec<_>>();

        let backpatch_body: RecBody<'b> = RecBody::new(move |work, frame| {
            for (target, backpatch) in &backpatches {
                work.emit_backpatch(target.clone(), backpatch, frame);
            }

            body.call(work, frame)
        });

        self.lower_rec_computed(&sorted, &frame, backpatch_body)
    }

    pub fn lower_rec_computed<'b>(
        &mut self,
        computed: &'b [(cont::ValueName, &'b ersd::Term)],
        frame: &'b Frame,
        body: RecBody<'b>,
    ) -> cont::Tail {
        match computed {
            [] => body.call(self, frame),
            [(target, rhs), rest @ ..] => {
                let target = target.clone();

                self.lower_value_name(
                    rhs,
                    frame,
                    Cont::new(move |work, result| {
                        work.emit.add_value(target, cont::Value::Alias(result));
                        work.lower_rec_computed(rest, frame, body)
                    }),
                )
            }
        }
    }

    pub fn lower_tail(
        &mut self,
        term: &ersd::Term,
        frame: &Frame,
        resume: &cont::BlockName,
    ) -> cont::Tail {
        match &**term {
            ersd::Subterm::Unreachable => cont::Tail::Unreachable,
            ersd::Subterm::Apply(apply) => self.lower_value_name(
                &apply.head,
                frame,
                Cont::new(move |work, head| {
                    work.lower_names(
                        &apply.params,
                        frame,
                        vec![],
                        ContMany::call_indirect(head, resume.clone()),
                    )
                }),
            ),
            ersd::Subterm::Match(m) => self.lower_value_name(
                &m.head,
                frame,
                Cont::new(move |work, head| {
                    let mut cases = BTreeMap::new();

                    for (i, branch) in m.cases.iter().enumerate() {
                        let block = work.emit.fresh_block();
                        let region = work.in_subregion(|w| w.lower_tail(branch, frame, resume));

                        work.emit.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region,
                            },
                        );

                        cases.insert(
                            i as u32,
                            cont::JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    cont::Tail::Match(cont::MatchTarget {
                        operand: head,
                        cases,
                        default: None,
                    })
                }),
            ),
            ersd::Subterm::NatMatch(ersd::NatMatch::Induction {
                head: nat_head,
                zero_case,
                pred,
                ih,
                succ_case,
            }) => self.lower_value_name(
                nat_head,
                frame,
                Cont::new(move |work, head| {
                    // Constants shared across the loop.
                    let zero_nat = work.emit.fresh(cont::Value::Pure(cont::Data::Nat(0)));
                    let one_nat = work.emit.fresh(cont::Value::Pure(cont::Data::Nat(1)));

                    // Allocate block names up front so they can be referenced cross-block.
                    let loop_block_name = work.emit.fresh_block();
                    let body_block_name = work.emit.fresh_block();
                    let exit_block_name = work.emit.fresh_block();
                    let zero_resume_name = work.emit.fresh_block();

                    // zero_resume(pz): jump loop_block(0, pz)
                    // This is the resume for the zero_case lowering; its result seeds the loop.
                    let pz = work.emit.fresh_value();
                    work.emit.add_block(
                        zero_resume_name.clone(),
                        cont::Block {
                            params: vec![pz.clone()],
                            region: RegionBuilder::new().finish(cont::Tail::Jump(
                                cont::JumpTarget {
                                    target: loop_block_name.clone(),
                                    params: vec![zero_nat, pz],
                                },
                            )),
                        },
                    );

                    // loop_block(i, acc):
                    //   cmp = NatEql(i, n)
                    //   cmp==0 (i≠n) → body_block(i, acc)
                    //   cmp≠0 (i=n) → exit_block(acc)
                    let i = work.emit.fresh_value();
                    let acc = work.emit.fresh_value();
                    let loop_block_region = {
                        let mut sub = work.emit.subregion();
                        let cmp = sub.fresh(cont::Value::Eval(cont::Code::NatEql(i.clone(), head)));
                        sub.finish(cont::Tail::Match(cont::MatchTarget {
                            operand: cmp,
                            cases: BTreeMap::from([(
                                0,
                                cont::JumpTarget {
                                    target: body_block_name.clone(),
                                    params: vec![i.clone(), acc.clone()],
                                },
                            )]),
                            default: Some(cont::JumpTarget {
                                target: exit_block_name.clone(),
                                params: vec![acc.clone()],
                            }),
                        }))
                    };
                    work.emit.add_block(
                        loop_block_name.clone(),
                        cont::Block {
                            params: vec![i.clone(), acc.clone()],
                            region: loop_block_region,
                        },
                    );

                    // body_block(i2, acc2):
                    //   succ_frame = {pred→i2, ih→acc2}
                    //   lower succ_case with body_resume as resume
                    //
                    // body_resume(acc'):
                    //   i' = NatAdd(i2, 1)   -- i2 accessible from enclosing body_block frame
                    //   jump loop_block(i', acc')
                    let i2 = work.emit.fresh_value();
                    let acc2 = work.emit.fresh_value();
                    let body_resume_name = work.emit.fresh_block();
                    let acc_prime = work.emit.fresh_value();

                    let body_resume_region = {
                        let mut sub = work.emit.subregion();
                        let i_prime =
                            sub.fresh(cont::Value::Eval(cont::Code::NatAdd(i2.clone(), one_nat)));
                        sub.finish(cont::Tail::Jump(cont::JumpTarget {
                            target: loop_block_name,
                            params: vec![i_prime, acc_prime.clone()],
                        }))
                    };

                    let succ_frame =
                        frame.extended([(pred.clone(), i2.clone()), (ih.clone(), acc2.clone())]);
                    let body_region = {
                        let mut body = work.emit.subregion();
                        body.add_block(
                            body_resume_name.clone(),
                            cont::Block {
                                params: vec![acc_prime],
                                region: body_resume_region,
                            },
                        );
                        let body_tail = Work {
                            lowerer: &mut *work.lowerer,
                            emit: &mut body,
                        }
                        .lower_tail(
                            succ_case,
                            &succ_frame,
                            &body_resume_name,
                        );

                        body.finish(body_tail)
                    };
                    work.emit.add_block(
                        body_block_name,
                        cont::Block {
                            params: vec![i2, acc2],
                            region: body_region,
                        },
                    );

                    // exit_block(acc_final): return the accumulated result.
                    let acc_final = work.emit.fresh_value();
                    work.emit.add_block(
                        exit_block_name,
                        cont::Block {
                            params: vec![acc_final.clone()],
                            region: RegionBuilder::new().finish(cont::Tail::Jump(
                                cont::JumpTarget {
                                    target: resume.clone(),
                                    params: vec![acc_final],
                                },
                            )),
                        },
                    );

                    // Outer tail: lower zero_case; its result flows into zero_resume → loop.
                    work.lower_tail(zero_case, frame, &zero_resume_name)
                }),
            ),
            ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
                head: nm_head,
                cases: nm_cases,
                default: nm_default,
            }) => self.lower_value_name(
                nm_head,
                frame,
                Cont::new(move |work, head| {
                    let mut cases = BTreeMap::new();

                    for (val, branch) in nm_cases.iter() {
                        let block = work.emit.fresh_block();
                        let region = work.in_subregion(|w| w.lower_tail(branch, frame, resume));
                        work.emit.add_block(
                            block.clone(),
                            cont::Block {
                                params: vec![],
                                region,
                            },
                        );
                        cases.insert(
                            *val,
                            cont::JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    let default_block = work.emit.fresh_block();
                    let region = work.in_subregion(|w| w.lower_tail(nm_default, frame, resume));
                    work.emit.add_block(
                        default_block.clone(),
                        cont::Block {
                            params: vec![],
                            region,
                        },
                    );

                    cont::Tail::Match(cont::MatchTarget {
                        operand: head,
                        cases,
                        default: Some(cont::JumpTarget {
                            target: default_block,
                            params: vec![],
                        }),
                    })
                }),
            ),
            ersd::Subterm::Let(let_) => {
                let name = let_.name.clone();
                self.lower_value_name(
                    &let_.body,
                    frame,
                    Cont::new(move |work, body| {
                        let frame = frame.extended([(name, body)]);
                        work.lower_tail(&let_.tail, &frame, resume)
                    }),
                )
            }
            ersd::Subterm::Rec(letrec) => self.lower_rec(
                &letrec.names,
                letrec.items.iter(),
                frame,
                RecBody::new(move |work, frame| work.lower_tail(&letrec.tail, frame, resume)),
            ),
            _ => self.lower_value_name(term, frame, Cont::jump_to(resume.clone())),
        }
    }

    /// Lower the flat top-level `items`, then the entrypoint `body`, threading the
    /// accumulating value frame. Synchronous items (`is_synchronous`) are lowered
    /// in a flat loop via `lower_pure_name`; a non-synchronous top-level `let`
    /// falls back to CPS, resuming this loop for the remaining items *inside* its
    /// continuation. Native recursion is therefore bounded by the count of
    /// non-synchronous items (≈0 for the prelude, which is functions and types) —
    /// never the total — which is the whole point of the flat module (BUG.md).
    fn lower_module_items<'b>(
        &mut self,
        items: &'b [ersd::Item],
        body: &'b ersd::Term,
        mut frame: Frame,
        resume: &cont::BlockName,
    ) -> cont::Tail {
        let mut index = 0;

        while index < items.len() {
            match &items[index] {
                // A `rec` group of only synchronous members (mutually-recursive
                // functions — the common case) lowers in place, no recursion.
                ersd::Item::Rec { names, items: defs } if defs.iter().all(is_synchronous) => {
                    frame = self.lower_letrec_bindings(names, defs.iter(), &frame);
                    index += 1;
                }
                // A `rec` group with a computational member needs the CPS `lower_rec`
                // (resume blocks per computed item, dependency-ordered); resume this
                // loop for the remaining items inside its `RecBody`.
                ersd::Item::Rec { names, items: defs } => {
                    let rest = &items[index + 1..];
                    let resume = resume.clone();

                    return self.lower_rec(
                        names,
                        defs.iter(),
                        &frame,
                        RecBody::new(move |work, frame| {
                            work.lower_module_items(rest, body, frame.clone(), &resume)
                        }),
                    );
                }
                ersd::Item::Let {
                    name,
                    body: let_body,
                } if is_synchronous(let_body) => {
                    let value = self.lower_pure_name(let_body, &frame);
                    frame.push(name.clone(), value);
                    index += 1;
                }
                ersd::Item::Let {
                    name,
                    body: let_body,
                } => {
                    let name = name.clone();
                    let rest = &items[index + 1..];
                    let resume = resume.clone();
                    let captured = frame.clone();

                    return self.lower_value_name(
                        let_body,
                        &frame,
                        Cont::new(move |work, value| {
                            let frame = captured.extended([(name, value)]);

                            work.lower_module_items(rest, body, frame, &resume)
                        }),
                    );
                }
            }
        }

        self.lower_tail(body, &frame, resume)
    }
}
