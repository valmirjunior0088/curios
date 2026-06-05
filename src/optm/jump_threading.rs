use {super::*, std::collections::HashMap};

/// Jump threading: merge a block with a single predecessor into that predecessor.
///
/// When a region's `tail` is `Jump(b, args)` and `b` is a *local* block (defined
/// in that same region) entered from **nowhere else**, the boundary between them
/// is pure overhead: control always flows straight through. This pass dissolves
/// it — splicing `b`'s body into the region, binding `b`'s parameters to the jump
/// arguments as aliases, and replacing the tail with `b`'s tail.
///
/// This is the intra-function dual of [`function_inlining`]: it brings values
/// across what was a continuation edge, so constant folding and aggregate
/// projection can see a literal or constructor that previously arrived opaquely as
/// a block parameter. It is also where inlining's leftovers get cleaned up — a
/// spliced call leaves a single-predecessor continuation block at every site.
///
/// ## Why no renaming is needed
///
/// Unlike function inlining, this is an intra-function move, and the lowerer makes
/// value and block names unique across a whole function body. So a merged block's
/// names cannot collide with its host's — the only binding introduced is one
/// `param = Alias(arg)` per parameter, which copy propagation then collapses.
///
/// ## What is left alone
///
/// - **Multi-predecessor blocks** — a join reached from several edges (the merge
///   would duplicate it, or worse). The single-predecessor count includes every
///   entry: `Jump` tails, `Match` arms, *and* `Call` resumes.
/// - **Resume continuations and the resume sentinel** — a block entered as a
///   call's `resume`, or the virtual return sentinel, is never a `Jump` tail's
///   local target, so it is never selected. (Loops are excluded for free too: a
///   block that jumps to itself has at least two predecessors.)
/// - **Cross-level targets** — a single predecessor that sits in a *descendant*
///   region rather than the block's own region. Sound to thread, but it is a
///   two-location move; left for later.
pub fn thread_jumps(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        thread_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        thread_tree(&mut clsr.region);
    }
}

fn thread_tree(region: &mut Region) {
    loop {
        let predecessors = predecessor_counts(region);

        if !thread_region(region, &predecessors) {
            break;
        }
    }
}

/// Thread every straight-line chain reachable through tails, then recurse. A
/// merge only relocates control-flow edges (it never creates or splits one), so
/// the predecessor counts stay valid across the whole traversal.
fn thread_region(region: &mut Region, predecessors: &HashMap<BlockName, usize>) -> bool {
    let mut changed = false;

    while let Some(target) = threadable_target(region, predecessors) {
        merge_block(region, &target);
        changed = true;
    }

    for (_, block) in &mut region.blocks {
        changed |= thread_region(&mut block.region, predecessors);
    }

    changed
}

/// The region's tail target, if it is a `Jump` to a local block entered exactly
/// once.
fn threadable_target(
    region: &Region,
    predecessors: &HashMap<BlockName, usize>,
) -> Option<BlockName> {
    if let Tail::Jump(jump) = &region.tail
        && predecessors.get(&jump.target) == Some(&1)
        && region.blocks.iter().any(|(name, _)| name == &jump.target)
    {
        Some(jump.target.clone())
    } else {
        None
    }
}

/// Splice the region's local block `target` (the tail's sole-predecessor jump
/// destination) into the region.
fn merge_block(region: &mut Region, target: &BlockName) {
    let args = match &region.tail {
        Tail::Jump(jump) => jump.params.clone(),
        _ => unreachable!("merge_block called without a jump tail"),
    };

    let position = region
        .blocks
        .iter()
        .position(|(name, _)| name == target)
        .expect("threadable target is a local block");
    let (_, block) = region.blocks.remove(position);

    debug_assert_eq!(
        block.params.len(),
        args.len(),
        "jump to `{target}` passes {} args for {} params",
        args.len(),
        block.params.len(),
    );

    let aliases: Vec<_> = block
        .params
        .iter()
        .zip(&args)
        .map(|(param, arg)| (param.clone(), Value::Alias(arg.clone())))
        .collect();

    region.preallocs.extend(block.region.preallocs);
    region.values.extend(aliases);
    region.values.extend(block.region.values);
    region.blocks.extend(block.region.blocks);
    region.tail = block.region.tail;
}

/// Count how many edges enter each block: `Jump` tails, `Match` arms, and `Call`
/// resumes. A block name is unique within a function body, so a single tree-wide
/// map is exact.
fn predecessor_counts(region: &Region) -> HashMap<BlockName, usize> {
    let mut counts = HashMap::new();
    count_predecessors(region, &mut counts);
    counts
}

fn count_predecessors(region: &Region, counts: &mut HashMap<BlockName, usize>) {
    match &region.tail {
        Tail::Jump(jump) => bump(counts, &jump.target),
        Tail::Match(target) => {
            for jump in target.cases.values() {
                bump(counts, &jump.target);
            }
            if let Some(jump) = &target.default {
                bump(counts, &jump.target);
            }
        }
        Tail::Call(CallTarget::Direct { resume, .. })
        | Tail::Call(CallTarget::Indirect { resume, .. }) => bump(counts, resume),
    }

    for (_, block) in &region.blocks {
        count_predecessors(&block.region, counts);
    }
}

fn bump(counts: &mut HashMap<BlockName, usize>, name: &BlockName) {
    *counts.entry(name.clone()).or_insert(0) += 1;
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

    fn region(
        values: Vec<(ValueName, Value)>,
        blocks: Vec<(BlockName, Block)>,
        tail: Tail,
    ) -> Region {
        Region {
            preallocs: vec![],
            values,
            blocks,
            tail,
        }
    }

    fn jump(target: &str, args: Vec<ValueName>) -> Tail {
        Tail::Jump(JumpTarget {
            target: b(target),
            params: args,
        })
    }

    fn block(params: Vec<ValueName>, region: Region) -> Block {
        Block { params, region }
    }

    fn run(top: Region) -> Region {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: b("rm"),
                region: top,
            },
        );
        thread_jumps(&mut module);
        module.funcs().iter().next().unwrap().1.region.clone()
    }

    #[test]
    fn threads_single_predecessor_local_jump() {
        // R: a = 1; jump k(a)   |   k(p): r = 0; return r
        let result = run(region(
            vec![(v("a"), Value::Pure(Data::Nat(1)))],
            vec![(
                b("k"),
                block(
                    vec![v("p")],
                    region(
                        vec![(v("r"), Value::Pure(Data::Nat(0)))],
                        vec![],
                        jump("rm", vec![v("r")]),
                    ),
                ),
            )],
            jump("k", vec![v("a")]),
        ));

        // The block is gone; its param is bound to the argument and its body and
        // tail are spliced in.
        assert!(result.blocks.is_empty());
        let names = result.values.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>();
        assert_eq!(names, vec![v("a"), v("p"), v("r")]);
        assert!(matches!(&result.values[0].1, Value::Pure(Data::Nat(1))));
        assert!(matches!(&result.values[1].1, Value::Alias(a) if a == &v("a")));
        assert!(matches!(&result.values[2].1, Value::Pure(Data::Nat(0))));
        match &result.tail {
            Tail::Jump(j) => {
                assert_eq!(j.target, b("rm"));
                assert_eq!(j.params, vec![v("r")]);
            }
            other => panic!("expected return jump, got {other:?}"),
        }
    }

    #[test]
    fn threads_a_straight_line_chain() {
        // R: jump b1   b1: jump b2   b2: r = 0; return r  — all collapse into R.
        let result = run(region(
            vec![],
            vec![
                (b("b1"), block(vec![], region(vec![], vec![], jump("b2", vec![])))),
                (
                    b("b2"),
                    block(
                        vec![],
                        region(
                            vec![(v("r"), Value::Pure(Data::Nat(0)))],
                            vec![],
                            jump("rm", vec![v("r")]),
                        ),
                    ),
                ),
            ],
            jump("b1", vec![]),
        ));

        assert!(result.blocks.is_empty());
        let names = result.values.iter().map(|(n, _)| n.clone()).collect::<Vec<_>>();
        assert_eq!(names, vec![v("r")]);
        assert!(matches!(&result.values[0].1, Value::Pure(Data::Nat(0))));
        assert!(matches!(&result.tail, Tail::Jump(j) if j.target == b("rm")));
    }

    #[test]
    fn does_not_thread_multi_predecessor_block() {
        // R: jump j   j: ...   d: jump j  — j has two predecessors, so it stays.
        let result = run(region(
            vec![],
            vec![
                (
                    b("j"),
                    block(vec![], region(vec![], vec![], jump("rm", vec![]))),
                ),
                (b("d"), block(vec![], region(vec![], vec![], jump("j", vec![])))),
            ],
            jump("j", vec![]),
        ));

        assert!(result.blocks.iter().any(|(name, _)| name == &b("j")));
        assert!(matches!(&result.tail, Tail::Jump(j) if j.target == b("j")));
    }

    #[test]
    fn does_not_thread_a_self_loop() {
        // R: jump loop   loop: jump loop  — two predecessors (entry + back-edge).
        let result = run(region(
            vec![],
            vec![(
                b("loop"),
                block(vec![], region(vec![], vec![], jump("loop", vec![]))),
            )],
            jump("loop", vec![]),
        ));

        assert!(result.blocks.iter().any(|(name, _)| name == &b("loop")));
    }

    #[test]
    fn does_not_thread_a_call_resume_continuation() {
        // The continuation `k` is entered by a call's resume, not a jump tail.
        let result = run(region(
            vec![],
            vec![(
                b("k"),
                block(
                    vec![v("res")],
                    region(vec![], vec![], jump("rm", vec![v("res")])),
                ),
            )],
            Tail::Call(CallTarget::Direct {
                target: FuncName::from("f"),
                params: vec![],
                resume: b("k"),
            }),
        ));

        assert!(result.blocks.iter().any(|(name, _)| name == &b("k")));
        assert!(matches!(&result.tail, Tail::Call(_)));
    }
}
