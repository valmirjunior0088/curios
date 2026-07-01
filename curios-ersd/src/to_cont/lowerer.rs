use {
    super::{
        Backpatch, Cont, ContMany, Emit, Frame, FrameEntropy, RecBody, RegionBuilder,
        lower_pure_prim, lower_value_prim, rec_computed_order, unsupported_sync_rec_item,
    },
    curios_base::Entropy,
    curios_cont::{
        Argument, Block, BlockName, Clsr, ClsrName, Code, Data, JumpTarget, MatchTarget, Module,
        Region, Tail, Value, ValueName,
    },
    std::collections::{BTreeMap, HashMap},
};

#[derive(Debug)]
pub struct Lowerer<'a> {
    module: &'a mut Module,
    clsrs: Entropy<ClsrName>,
}

impl<'a> Lowerer<'a> {
    pub fn new(module: &'a mut Module) -> Self {
        Self {
            module,
            clsrs: Entropy::<ClsrName>::new(),
        }
    }

    pub fn lower_module(&mut self, module: &crate::Module, frame: &Frame) -> (BlockName, Region) {
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

/// A unit of lowering work: the shared [`Lowerer`] (module + closure names) paired with the
/// region currently being emitted into. Threading one `Work` instead of a `Lowerer` and an
/// `Emit` separately keeps the per-primitive helpers down to their operands plus a continuation.
pub struct Work<'a, 'm, 'e> {
    lowerer: &'a mut Lowerer<'m>,
    emit: &'a mut Emit<'e>,
}

impl Work<'_, '_, '_> {
    /// Build a nested region with its own `Work`, sharing this work's `Lowerer` and name supply.
    fn in_subregion(&mut self, build: impl FnOnce(&mut Work) -> Tail) -> Region {
        let mut emit = self.emit.subregion();
        let tail = build(&mut Work {
            lowerer: &mut *self.lowerer,
            emit: &mut emit,
        });

        emit.finish(tail)
    }

    /// Allocate a fresh value in the current region. Thin accessor so the per-primitive lowering
    /// in `lower_prim` can emit values without reaching into the private `emit` field.
    pub fn fresh(&mut self, value: Value) -> ValueName {
        self.emit.fresh(value)
    }

    /// Mint a fresh block name. The block isn't bound to the current region yet —
    /// the caller is expected to follow up with [`Self::add_resume_block`] (or
    /// equivalent) once the block's region is ready.
    pub fn fresh_block(&mut self) -> BlockName {
        self.emit.fresh_block()
    }

    /// Mint a fresh value name without binding it to the current region. Used
    /// when the name is needed as a block parameter (bound by control flow) or
    /// passed to a continuation before the binding lands in some other region.
    pub fn fresh_value(&mut self) -> ValueName {
        self.emit.fresh_value()
    }

    /// Add a block to the current region whose region is built by `build` inside a
    /// fresh subregion. Used when emitting a tail (`Tail::Host`, `Tail::Call`, …)
    /// that branches to a synthesized continuation: the caller mints the block
    /// name, supplies the block's params, and provides a closure that emits the
    /// continuation's region against an outer `Cont`.
    pub fn add_resume_block(
        &mut self,
        name: BlockName,
        params: Vec<ValueName>,
        build: impl FnOnce(&mut Work) -> Tail,
    ) {
        let region = self.in_subregion(build);
        self.emit.add_block(name, Block { params, region });
    }

    pub fn lower_closure(
        &mut self,
        func: &crate::Func,
        frame: &Frame,
    ) -> (ClsrName, Vec<ValueName>) {
        let clsr_name = self.lowerer.clsrs.fresh();
        let (mut entry, resume) = FrameEntropy::new();
        let mut clsr_frame = Frame::new();

        // The candidate flag rides from the `crate::Argument` straight into the
        // `Argument`, glued to the freshly-bound name.
        let fields = func
            .captures
            .iter()
            .map(|capture| {
                let name = entry.fresh_value();
                clsr_frame.push(capture.name.clone(), name.clone());

                Argument {
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

                Argument {
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
            Clsr {
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
    /// the flat top-level `crate::Item::Rec`, so it takes the `names` and `items`
    /// directly rather than an `crate::Rec` (whose `tail` it never used).
    pub fn lower_letrec_bindings<'x>(
        &mut self,
        names: &[String],
        items: impl IntoIterator<Item = &'x crate::Term>,
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
                    self.emit
                        .add_prealloc(target.clone(), backpatch.clsr.clone());
                    self.emit_backpatch(target, &backpatch);
                }
                None => match &**item {
                    crate::Subterm::Apply(_)
                    | crate::Subterm::Match(_)
                    | crate::Subterm::NatMatch(_) => unsupported_sync_rec_item(item),
                    _ => self.lower_letrec_item(item, target, &frame),
                },
            }
        }

        frame
    }

    /// The runtime stand-in for an `Erased` term — a proof or type that survived
    /// into a slot the signature keeps for uniform arity, but which no code reads.
    /// It only has to *inhabit* the slot, and every slot is the uniform `anyref`
    /// (`top_type`), so the canonical i31 `0` serves: a zero-cost scalar with no
    /// heap object to allocate (or hoist) — unlike an empty tuple, the other
    /// "nothing" value, which is a real `array.new`. The value is never inspected,
    /// so sharing the carrier with `Nat 0` is immaterial.
    fn erased(&mut self) -> ValueName {
        self.emit.fresh(Value::Pure(Data::Nat(0)))
    }

    pub fn lower_pure_name(&mut self, term: &crate::Term, frame: &Frame) -> ValueName {
        match &**term {
            crate::Subterm::Name(name) => frame.find(name.as_str()),
            crate::Subterm::Erased => self.erased(),
            crate::Subterm::Unreachable => {
                panic!("unreachable Ersd term cannot be lowered in pure-name position")
            }
            crate::Subterm::Prim(crate::Prim::Pure(pure)) => lower_pure_prim(self, pure, frame),
            crate::Subterm::Prim(crate::Prim::Host(_)) => unreachable!(
                "host primitive reached pure-name context — pure-name lowering cannot \
                 construct the resume block required by Tail::Host"
            ),
            crate::Subterm::Prim(crate::Prim::Cell(_)) => unreachable!(
                "cell primitive reached pure-name context — pure-name lowering cannot \
                 construct the resume block required by Tail::Cell"
            ),
            crate::Subterm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);

                self.emit
                    .fresh(Value::Pure(Data::Clsr(clsr_name, captured_values)))
            }
            crate::Subterm::Tuple(s) => {
                let field_names = s
                    .fields
                    .iter()
                    .map(|f| self.lower_pure_name(f, frame))
                    .collect::<Vec<_>>();

                self.emit.fresh(Value::Pure(Data::Tpl(field_names)))
            }
            crate::Subterm::Atom(atom) => {
                self.emit.fresh(Value::Pure(Data::Nat(atom.index as u32)))
            }
            crate::Subterm::Let(let_) => {
                let body = self.lower_pure_name(&let_.body, frame);
                let frame = frame.extended([(let_.name.clone(), body)]);

                self.lower_pure_name(&let_.tail, &frame)
            }
            crate::Subterm::Rec(letrec) => {
                let frame = self.lower_letrec_bindings(&letrec.names, letrec.items.iter(), frame);

                self.lower_pure_name(&letrec.tail, &frame)
            }
            crate::Subterm::Proj(proj) => {
                let head = self.lower_pure_name(&proj.head, frame);

                self.emit.fresh(Value::Eval(Code::TplGet(head, proj.index)))
            }
            crate::Subterm::Apply(_) | crate::Subterm::Match(_) | crate::Subterm::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
        }
    }

    pub fn lower_letrec_item(&mut self, term: &crate::Term, target: ValueName, frame: &Frame) {
        match &**term {
            crate::Subterm::Apply(_) | crate::Subterm::Match(_) | crate::Subterm::NatMatch(_) => {
                unsupported_sync_rec_item(term)
            }
            _ => {
                let value = self.lower_pure_name(term, frame);
                self.emit.add_value(target, Value::Alias(value));
            }
        }
    }

    pub fn lower_value_name(&mut self, term: &crate::Term, frame: &Frame, cont: Cont<'_>) -> Tail {
        match &**term {
            crate::Subterm::Name(name) => cont.call(self, frame.find(name.as_str())),
            crate::Subterm::Erased => {
                let value = self.erased();

                cont.call(self, value)
            }
            crate::Subterm::Unreachable => Tail::Unreachable,
            crate::Subterm::Prim(prim) => lower_value_prim(self, prim, frame, cont),
            crate::Subterm::Func(func) => {
                let (clsr_name, captured_values) = self.lower_closure(func, frame);
                let value = self
                    .emit
                    .fresh(Value::Pure(Data::Clsr(clsr_name, captured_values)));

                cont.call(self, value)
            }
            crate::Subterm::Tuple(s) => self.lower_struct(&s.fields, frame, vec![], cont),
            crate::Subterm::Atom(atom) => {
                let value = self.emit.fresh(Value::Pure(Data::Nat(atom.index as u32)));

                cont.call(self, value)
            }
            crate::Subterm::Let(let_) => {
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
            crate::Subterm::Rec(letrec) => self.lower_rec(
                &letrec.names,
                letrec.items.iter(),
                frame,
                RecBody::new(move |work, frame| work.lower_value_name(&letrec.tail, frame, cont)),
            ),
            crate::Subterm::Proj(proj) => {
                let index = proj.index;

                self.lower_value_name(
                    &proj.head,
                    frame,
                    Cont::new(move |work, head| {
                        let v = work.emit.fresh(Value::Eval(Code::TplGet(head, index)));

                        cont.call(work, v)
                    }),
                )
            }
            crate::Subterm::Apply(_) | crate::Subterm::Match(_) | crate::Subterm::NatMatch(_) => {
                let block = self.emit.fresh_block();
                let param = self.emit.fresh_value();
                let region = self.in_subregion(|work| cont.call(work, param.clone()));

                self.emit.add_block(
                    block.clone(),
                    Block {
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
        params: &'b [crate::Term],
        frame: &'b Frame,
        mut names: Vec<ValueName>,
        cont: ContMany<'b>,
    ) -> Tail {
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
        fields: &'b [crate::Term],
        frame: &'b Frame,
        mut names: Vec<ValueName>,
        cont: Cont<'b>,
    ) -> Tail {
        match fields {
            [] => {
                let value = self.emit.fresh(Value::Pure(Data::Tpl(names)));

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

    /// Classify a `rec` item: only a `Func` gets a prealloc'd shell, so its closure identity
    /// is available before its captures (and the `Func` is lowered here to fix its `ClsrName`).
    /// Everything else — including tuples and arrays — is "computed": lowered via
    /// `lower_value_name` in dependency order. A non-cyclic aggregate then builds directly; a
    /// genuinely self-referential one surfaces as a cycle in `rec_computed_order` and is
    /// rejected. Confining cyclic recursion to closures is what lets `tpl`/`arr` fields stay
    /// immutable.
    pub fn plan_backpatch(&mut self, item: &crate::Term, frame: &Frame) -> Option<Backpatch> {
        match &**item {
            crate::Subterm::Func(func) => {
                let (clsr, captures) = self.lower_closure(func, frame);

                Some(Backpatch { clsr, captures })
            }
            _ => None,
        }
    }

    pub fn emit_backpatch(&mut self, target: ValueName, backpatch: &Backpatch) {
        self.emit.add_value(
            target,
            Value::Pure(Data::Clsr(
                backpatch.clsr.clone(),
                backpatch.captures.clone(),
            )),
        );
    }

    /// Lower a `rec` group, then `body`. Backpatches are prealloc'd at region entry; call/match
    /// -valued bindings are lowered in dependency order through resume blocks; patches
    /// (which may reference those results) run last, just before `body`.
    pub fn lower_rec<'b>(
        &mut self,
        names: &'b [String],
        items: impl IntoIterator<Item = &'b crate::Term>,
        frame: &Frame,
        body: RecBody<'b>,
    ) -> Tail {
        let mut frame = frame.clone();
        let targets = names
            .iter()
            .map(|name| {
                let target = self.emit.fresh_value();
                frame.push(name.clone(), target.clone());

                target
            })
            .collect::<Vec<_>>();

        let mut backpatches: Vec<(ValueName, Backpatch)> = vec![];
        let mut computed: Vec<(usize, ValueName, &'b crate::Term)> = vec![];

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
                rhs.free_names()
                    .iter()
                    .filter_map(|name| name_to_pos.get(name.as_str()).copied())
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let order = rec_computed_order(&computed_names, &deps);

        for (target, backpatch) in &backpatches {
            self.emit
                .add_prealloc(target.clone(), backpatch.clsr.clone());
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
                work.emit_backpatch(target.clone(), backpatch);
            }

            body.call(work, frame)
        });

        self.lower_rec_computed(&sorted, &frame, backpatch_body)
    }

    pub fn lower_rec_computed<'b>(
        &mut self,
        computed: &'b [(ValueName, &'b crate::Term)],
        frame: &'b Frame,
        body: RecBody<'b>,
    ) -> Tail {
        match computed {
            [] => body.call(self, frame),
            [(target, rhs), rest @ ..] => {
                let target = target.clone();

                self.lower_value_name(
                    rhs,
                    frame,
                    Cont::new(move |work, result| {
                        work.emit.add_value(target, Value::Alias(result));
                        work.lower_rec_computed(rest, frame, body)
                    }),
                )
            }
        }
    }

    pub fn lower_tail(&mut self, term: &crate::Term, frame: &Frame, resume: &BlockName) -> Tail {
        match &**term {
            crate::Subterm::Unreachable => Tail::Unreachable,
            crate::Subterm::Apply(apply) => self.lower_value_name(
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
            crate::Subterm::Match(m) => self.lower_value_name(
                &m.head,
                frame,
                Cont::new(move |work, head| {
                    let mut cases = BTreeMap::new();

                    for (i, branch) in m.cases.iter().enumerate() {
                        let block = work.emit.fresh_block();
                        let region = work.in_subregion(|w| w.lower_tail(branch, frame, resume));

                        work.emit.add_block(
                            block.clone(),
                            Block {
                                params: vec![],
                                region,
                            },
                        );

                        cases.insert(
                            i as u32,
                            JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    Tail::Match(MatchTarget {
                        operand: head,
                        cases,
                        default: None,
                    })
                }),
            ),
            crate::Subterm::NatMatch(crate::NatMatch::Induction {
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
                    let zero_nat = work.emit.fresh(Value::Pure(Data::Nat(0)));
                    let one_nat = work.emit.fresh(Value::Pure(Data::Nat(1)));

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
                        Block {
                            params: vec![pz.clone()],
                            region: RegionBuilder::new().finish(Tail::Jump(JumpTarget {
                                target: loop_block_name.clone(),
                                params: vec![zero_nat, pz],
                            })),
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
                        let cmp = sub.fresh(Value::Eval(Code::NatEql(i.clone(), head)));
                        sub.finish(Tail::Match(MatchTarget {
                            operand: cmp,
                            cases: BTreeMap::from([(
                                0,
                                JumpTarget {
                                    target: body_block_name.clone(),
                                    params: vec![i.clone(), acc.clone()],
                                },
                            )]),
                            default: Some(JumpTarget {
                                target: exit_block_name.clone(),
                                params: vec![acc.clone()],
                            }),
                        }))
                    };
                    work.emit.add_block(
                        loop_block_name.clone(),
                        Block {
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
                        let i_prime = sub.fresh(Value::Eval(Code::NatAdd(i2.clone(), one_nat)));
                        sub.finish(Tail::Jump(JumpTarget {
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
                            Block {
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
                        Block {
                            params: vec![i2, acc2],
                            region: body_region,
                        },
                    );

                    // exit_block(acc_final): return the accumulated result.
                    let acc_final = work.emit.fresh_value();
                    work.emit.add_block(
                        exit_block_name,
                        Block {
                            params: vec![acc_final.clone()],
                            region: RegionBuilder::new().finish(Tail::Jump(JumpTarget {
                                target: resume.clone(),
                                params: vec![acc_final],
                            })),
                        },
                    );

                    // Outer tail: lower zero_case; its result flows into zero_resume → loop.
                    work.lower_tail(zero_case, frame, &zero_resume_name)
                }),
            ),
            crate::Subterm::NatMatch(crate::NatMatch::Dispatch {
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
                            Block {
                                params: vec![],
                                region,
                            },
                        );
                        cases.insert(
                            *val,
                            JumpTarget {
                                target: block,
                                params: vec![],
                            },
                        );
                    }

                    let default_block = work.emit.fresh_block();
                    let region = work.in_subregion(|w| w.lower_tail(nm_default, frame, resume));
                    work.emit.add_block(
                        default_block.clone(),
                        Block {
                            params: vec![],
                            region,
                        },
                    );

                    Tail::Match(MatchTarget {
                        operand: head,
                        cases,
                        default: Some(JumpTarget {
                            target: default_block,
                            params: vec![],
                        }),
                    })
                }),
            ),
            crate::Subterm::Let(let_) => {
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
            crate::Subterm::Rec(letrec) => self.lower_rec(
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
        items: &'b [crate::Item],
        body: &'b crate::Term,
        mut frame: Frame,
        resume: &BlockName,
    ) -> Tail {
        let mut index = 0;

        while index < items.len() {
            match &items[index] {
                // A `rec` group of only synchronous members (mutually-recursive
                // functions — the common case) lowers in place, no recursion.
                crate::Item::Rec { names, items: defs }
                    if defs.iter().all(crate::Term::is_synchronous) =>
                {
                    frame = self.lower_letrec_bindings(names, defs.iter(), &frame);
                    index += 1;
                }
                // A `rec` group with a computational member needs the CPS `lower_rec`
                // (resume blocks per computed item, dependency-ordered); resume this
                // loop for the remaining items inside its `RecBody`.
                crate::Item::Rec { names, items: defs } => {
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
                crate::Item::Let {
                    name,
                    body: let_body,
                } if let_body.is_synchronous() => {
                    let value = self.lower_pure_name(let_body, &frame);
                    frame.push(name.clone(), value);
                    index += 1;
                }
                crate::Item::Let {
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
