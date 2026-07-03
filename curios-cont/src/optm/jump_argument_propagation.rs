use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Jump-argument propagation: a block parameter that every edge feeds the same
/// single value is that value — substitute it through.
///
/// Copy propagation sees through renames and jump threading dissolves
/// single-predecessor blocks, but a *multi*-predecessor block parameter hides
/// its argument even when every edge agrees. The motivating shape is the loop
/// [`tail_recursion`](super::tail_recursion) mints from an erased `Nat`/`Bin`
/// induction: the case closures are threaded around the loop as header
/// parameters — the entry edge passes a known closure and every back edge
/// passes the parameter through unchanged — so the indirect calls through them
/// sit behind a parameter [`closure_lifting`](super::closure_lifting) cannot
/// resolve, and the loop body never devirtualizes and never inlines.
///
/// In φ terms: `p = φ(v, p, p, …)` is `p = v`. The analysis is a Kleene
/// fixpoint from "unknown": for each block parameter slot, collect the slot's
/// argument on every jump and match edge, resolve each through the aliases
/// and the slots already decided, and ignore arguments that resolve to the
/// parameter itself (a self-loop preserves whatever the value was). If the
/// remaining arguments agree on one name, the slot is decided; iterating lets
/// chains of parameter-to-parameter feeding collapse. Deciding is monotone —
/// a decided slot's arguments never change — so the fixpoint terminates.
///
/// A slot is left alone when:
///
/// - its block is any call/host/cell **resume** target — those parameters
///   carry the callee's result, which no edge spells out;
/// - any edge's argument stays opaque (an undecided parameter feeding it), or
///   two edges disagree, or no edge remains once self-arguments are ignored;
/// - the agreed value is **not in scope** at the block: an edge may pass a
///   name bound in its own arm that the block's subtree could not legally
///   read. In-scope means bound on the block's own ancestor path (a region's
///   values run before control enters its blocks, so an ancestor binding is
///   always evaluated when the block runs), or not bound in the body at all —
///   a module const.
///
/// The rewrite substitutes the parameter's uses and leaves the parameter and
/// its edge arguments in place, now dead — the same leave-it-to-cleanup
/// protocol as CSE's aliases: dead-argument elimination drops the slot and
/// every coordinated argument.
pub fn propagate_jump_arguments(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        propagate_in_body(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        propagate_in_body(&mut clsr.region);
    }
}

fn propagate_in_body(root: &mut Region) {
    let mut harvest = Harvest::default();
    harvest.region(root, &mut Vec::new());

    let decided = decide(harvest);
    if decided.is_empty() {
        return;
    }

    walk_region_mut(root, &mut Substitute { decided: &decided });
}

/// One block parameter slot's standing in the body.
struct Slot {
    param: ValueName,
    /// The slot's argument on every jump/match edge targeting the block.
    args: Vec<ValueName>,
    /// The path of the block's region — the scope the agreed value must
    /// enclose.
    path: Vec<usize>,
    /// A resume target's parameters carry callee results; never decided.
    poisoned: bool,
}

#[derive(Default)]
struct Harvest {
    slots: Vec<Slot>,
    /// Block name → index range of its slots in `slots`.
    blocks: HashMap<BlockName, (usize, usize)>,
    /// Jump/match edges, buffered by target name: an edge can point at a
    /// block indexed later in the walk.
    edges: Vec<(BlockName, Vec<ValueName>)>,
    /// Resume targets of call/host/cell tails.
    resumes: HashSet<BlockName>,
    /// Alias bindings, for resolving arguments to their sources.
    aliases: HashMap<ValueName, ValueName>,
    /// Every bound name → the path of the region binding it (block params are
    /// bound at their block's region path).
    bound: HashMap<ValueName, Vec<usize>>,
}

impl Harvest {
    fn region(&mut self, region: &Region, path: &mut Vec<usize>) {
        for (name, _) in &region.preallocs {
            self.bound.insert(name.clone(), path.clone());
        }

        for (name, value) in &region.values {
            self.bound.insert(name.clone(), path.clone());
            if let Value::Alias(source) = value {
                self.aliases.insert(name.clone(), source.clone());
            }
        }

        self.tail(&region.tail);

        for (position, (name, block)) in region.blocks.iter().enumerate() {
            path.push(position);

            let start = self.slots.len();
            for param in &block.params {
                self.bound.insert(param.clone(), path.clone());
                self.slots.push(Slot {
                    param: param.clone(),
                    args: Vec::new(),
                    path: path.clone(),
                    poisoned: false,
                });
            }
            self.blocks.insert(name.clone(), (start, self.slots.len()));

            self.region(&block.region, path);
            path.pop();
        }
    }

    fn tail(&mut self, tail: &Tail) {
        match tail {
            Tail::Jump(jump) => self.edge(jump),
            Tail::Match(target) => {
                for jump in target.cases.values() {
                    self.edge(jump);
                }
                if let Some(jump) = &target.default {
                    self.edge(jump);
                }
            }
            Tail::Call(CallTarget::Direct { resume, .. })
            | Tail::Call(CallTarget::Indirect { resume, .. }) => {
                self.resumes.insert(resume.clone());
            }
            Tail::Host(host) => {
                self.resumes.insert(host.resume().clone());
            }
            Tail::Cell(cell) => {
                self.resumes.insert(cell.resume().clone());
            }
            Tail::Unreachable => {}
        }
    }

    fn edge(&mut self, jump: &JumpTarget) {
        self.edges.push((jump.target.clone(), jump.params.clone()));
    }
}

/// The fixpoint: join every edge into its target's slots, then repeatedly
/// decide slots whose resolved arguments agree, until nothing new decides.
fn decide(mut harvest: Harvest) -> HashMap<ValueName, ValueName> {
    // Join edges into slots.
    let edges = std::mem::take(&mut harvest.edges);
    for (target, args) in &edges {
        let Some(&(start, end)) = harvest.blocks.get(target) else {
            continue;
        };

        let slots = &mut harvest.slots[start..end];

        // An arity mismatch poisons the whole block rather than guessing at
        // slot correspondence.
        if args.len() != slots.len() {
            for slot in slots {
                slot.poisoned = true;
            }
            continue;
        }

        for (slot, arg) in slots.iter_mut().zip(args) {
            slot.args.push(arg.clone());
        }
    }

    for target in &harvest.resumes {
        if let Some(&(start, end)) = harvest.blocks.get(target) {
            for slot in &mut harvest.slots[start..end] {
                slot.poisoned = true;
            }
        }
    }

    let mut decided: HashMap<ValueName, ValueName> = HashMap::new();

    loop {
        let mut changed = false;

        for slot in &harvest.slots {
            if slot.poisoned || decided.contains_key(&slot.param) || slot.args.is_empty() {
                continue;
            }

            let mut agreed: Option<ValueName> = None;
            let mut opaque = false;

            for arg in &slot.args {
                let arg = resolve(arg, &harvest.aliases, &decided);

                if arg == slot.param {
                    continue;
                }

                // An argument that is some *other* undecided parameter is
                // opaque this round; a later round may resolve it.
                if harvest.slots.iter().any(|s| s.param == arg) && !decided.contains_key(&arg) {
                    opaque = true;
                    break;
                }

                match &agreed {
                    None => agreed = Some(arg),
                    Some(seen) if *seen == arg => {}
                    Some(_) => {
                        opaque = true;
                        break;
                    }
                }
            }

            let Some(value) = (!opaque).then_some(agreed).flatten() else {
                continue;
            };

            if !in_scope(&value, &slot.path, &harvest.bound) {
                continue;
            }

            decided.insert(slot.param.clone(), value);
            changed = true;
        }

        if !changed {
            return decided;
        }
    }
}

fn resolve(
    name: &ValueName,
    aliases: &HashMap<ValueName, ValueName>,
    decided: &HashMap<ValueName, ValueName>,
) -> ValueName {
    let mut current = name.clone();
    let mut seen = HashSet::new();

    while let Some(next) = decided.get(&current).or_else(|| aliases.get(&current)) {
        if !seen.insert(current.clone()) {
            break;
        }
        current = next.clone();
    }

    current
}

/// Is `value` readable inside the block whose region sits at `block_path`?
/// Bound on an ancestor path (proper prefix, or the block's own path for its
/// declaring region's values) — yes. Not bound in the body — a module const,
/// visible everywhere. Bound anywhere else (a sibling arm, deeper) — no.
fn in_scope(
    value: &ValueName,
    block_path: &[usize],
    bound: &HashMap<ValueName, Vec<usize>>,
) -> bool {
    match bound.get(value) {
        None => true,
        Some(binding_path) => {
            binding_path.len() < block_path.len() && block_path.starts_with(binding_path)
        }
    }
}

struct Substitute<'a> {
    decided: &'a HashMap<ValueName, ValueName>,
}

impl SinkMut for Substitute<'_> {
    fn value_use(&mut self, name: &mut ValueName) {
        if let Some(value) = self.decided.get(name) {
            let mut resolved = value.clone();
            let mut seen = HashSet::new();
            seen.insert(name.clone());

            while let Some(next) = self.decided.get(&resolved) {
                if !seen.insert(resolved.clone()) {
                    break;
                }
                resolved = next.clone();
            }

            *name = resolved;
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    fn b(name: &str) -> BlockName {
        BlockName::from(name)
    }

    fn jump(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(target),
            params: args,
        })
    }

    fn add_main(module: &mut Module, region: Region) {
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![v("n").into()],
                resume: b("r"),
                region,
            },
        );
    }

    fn main_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    /// The motivating loop: `f` is threaded as a loop param; the entry passes
    /// a known closure and the back edge passes the param through, while the
    /// counter genuinely varies.
    fn threaded_loop() -> Module {
        let header = Block {
            params: vec![v("i"), v("f")],
            region: Region {
                preallocs: vec![],
                values: vec![
                    (v("one"), Value::Pure(Data::Nat(1))),
                    (v("j"), Value::Eval(Code::NatSub(v("i"), v("one")))),
                ],
                blocks: vec![],
                tail: Tail::Match(MatchTarget {
                    operand: v("i"),
                    cases: [(
                        0,
                        JumpTarget {
                            target: b("r"),
                            params: vec![v("f")],
                        },
                    )]
                    .into(),
                    default: Some(JumpTarget {
                        target: b("b@loop"),
                        params: vec![v("j"), v("f")],
                    }),
                }),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![(
                    v("c"),
                    Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![])),
                )],
                blocks: vec![(b("b@loop"), header)],
                tail: jump("b@loop", vec![v("n"), v("c")]),
            },
        );
        module
    }

    #[test]
    fn propagates_the_threaded_closure() {
        let mut module = threaded_loop();

        propagate_jump_arguments(&mut module);

        // The exit edge's use of the param now names the closure directly.
        let header = &main_region(&module).blocks[0].1.region;
        match &header.tail {
            Tail::Match(target) => {
                assert_eq!(target.cases[&0].params, vec![v("c")]);
                // The self-feeding closure slot is substituted (f == c); the
                // varying counter slot is untouched.
                assert_eq!(
                    target.default.as_ref().unwrap().params,
                    vec![v("j"), v("c")]
                );
            }
            other => panic!("expected match, got {other:?}"),
        }
    }

    #[test]
    fn leaves_disagreeing_edges_alone() {
        // Two entries pass different closures — nothing to propagate.
        let target = Block {
            params: vec![v("f")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v("f")]),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![
                    (
                        v("c1"),
                        Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![])),
                    ),
                    (
                        v("c2"),
                        Value::Pure(Data::Clsr(ClsrName::from("c9"), vec![])),
                    ),
                ],
                blocks: vec![(b("t"), target)],
                tail: Tail::Match(MatchTarget {
                    operand: v("n"),
                    cases: [
                        (
                            0,
                            JumpTarget {
                                target: b("t"),
                                params: vec![v("c1")],
                            },
                        ),
                        (
                            1,
                            JumpTarget {
                                target: b("t"),
                                params: vec![v("c2")],
                            },
                        ),
                    ]
                    .into(),
                    default: None,
                }),
            },
        );

        propagate_jump_arguments(&mut module);

        let block = &main_region(&module).blocks[0].1.region;
        match &block.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![v("f")]),
            other => panic!("expected jump, got {other:?}"),
        }
    }

    #[test]
    fn skips_resume_targets() {
        // k's parameter carries g's result even though a jump also feeds it.
        let k = Block {
            params: vec![v("res")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v("res")]),
            },
        };
        let entry = Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: Tail::Call(CallTarget::Direct {
                    target: FuncName::from("g"),
                    params: vec![],
                    resume: b("k"),
                }),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![(v("c"), Value::Pure(Data::Nat(1)))],
                blocks: vec![(b("k"), k), (b("e"), entry)],
                tail: Tail::Match(MatchTarget {
                    operand: v("n"),
                    cases: [(
                        0,
                        JumpTarget {
                            target: b("k"),
                            params: vec![v("c")],
                        },
                    )]
                    .into(),
                    default: Some(JumpTarget {
                        target: b("e"),
                        params: vec![],
                    }),
                }),
            },
        );

        propagate_jump_arguments(&mut module);

        let k = &main_region(&module).blocks[0].1.region;
        match &k.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![v("res")]),
            other => panic!("expected jump, got {other:?}"),
        }
    }

    #[test]
    fn respects_scope() {
        // Both arms pass the same name, but it is bound inside one arm — the
        // target block could not legally read it.
        let arm = |tail: Tail| Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![(v("x"), Value::Pure(Data::Nat(1)))],
                blocks: vec![],
                tail,
            },
        };

        // Only one arm actually binds x; use one arm to keep names unique.
        let a1 = arm(jump("t", vec![v("x")]));
        let target = Block {
            params: vec![v("p")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v("p")]),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("a1"), a1), (b("t"), target)],
                tail: jump("a1", vec![]),
            },
        );

        propagate_jump_arguments(&mut module);

        // x is bound in a sibling arm of t: not in scope, not substituted.
        let t = &main_region(&module).blocks[1].1.region;
        match &t.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![v("p")]),
            other => panic!("expected jump, got {other:?}"),
        }
    }

    #[test]
    fn decides_chained_parameters() {
        // t1's param is fed the constant; t2's param is fed t1's param —
        // the second round resolves it.
        let t2 = Block {
            params: vec![v("p2")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v("p2")]),
            },
        };
        let t1 = Block {
            params: vec![v("p1")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("t2"), t2)],
                tail: jump("t2", vec![v("p1")]),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![(v("c"), Value::Pure(Data::Nat(7)))],
                blocks: vec![(b("t1"), t1)],
                tail: jump("t1", vec![v("c")]),
            },
        );

        propagate_jump_arguments(&mut module);

        let t2 = &main_region(&module).blocks[0].1.region.blocks[0].1.region;
        match &t2.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![v("c")]),
            other => panic!("expected jump, got {other:?}"),
        }
    }
}
