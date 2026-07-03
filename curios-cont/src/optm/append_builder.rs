use {
    super::*,
    std::collections::{HashMap, HashSet},
};

/// Append-accumulator → builder: turn a loop that grows a buffer by
/// re-concatenating it every step into one that collects the pieces and
/// flattens once on the way out.
///
/// The shape is what a string-building fold leaves after tail-recursion
/// conversion and inlining (`std/Fmt`'s `go_with`, or any
/// `acc = concat(acc, piece)` loop):
///
/// ```text
///   Jump(b@loop, [.., init, ..])                Jump(b@loop, [.., [init], ..])
///   block b@loop[.., acc, ..]:                  block b@loop[.., parts, ..]:
///     v = BinConcat([acc, k])          =>         v = ArrAppend(parts, k)
///     ... Jump(b@loop, [.., v, ..])               ... Jump(b@loop, [.., v, ..])
///     ... finish(acc) ...                         whole = BinFlatten(parts)
///                                                 ... finish(whole) ...
/// ```
///
/// Each original step copies the whole accumulator — `BinConcat` is an
/// `array.new` plus full copies — so building n pieces costs Θ(n²) bytes
/// moved. A parts array appends one *reference* per step and the exit
/// flatten copies each byte exactly once: Θ(n).
///
/// Recognition is all-or-nothing per `(header, slot)` and default-declines on
/// any use shape outside the envelope:
///
/// - every back-edge argument in the slot is the parameter itself (a
///   pass-through) or a `BinConcat([acc, piece])`/`ArrConcat([acc, piece])`
///   binding — accumulator strictly on the left, one piece, `piece != acc` —
///   whose only use is that jump argument, all steps agreeing on one carrier;
/// - every *other* use of the parameter sits in a region that cannot reach a
///   back edge (an exit — e.g. the accumulator flowing into the loop's final
///   indirect call), where the rewrite materialises it with one flatten per
///   region and substitutes the region's own uses;
/// - the header is not the resume of any call, host, or cell op (a resume
///   carries the callee's result, so its entry edges cannot be reseeded), and
///   at least one back edge actually appends (an all-pass-through slot has
///   nothing to gain).
///
/// Soundness: byte concatenation is associative, parts arrive in program
/// order, and `BinFlatten`/`ArrFlatten` concatenate in order, so the escaped
/// value is byte-identical to the left-fold. Everything inserted is pure. A
/// region that leaves the loop and re-enters it is handled by composition:
/// its uses flatten (it cannot reach a back edge) and the re-entering jump is
/// an entry edge, reseeded as `[flattened]`.
///
/// [`fuse_concat_chains`] is the straight-line companion: partial evaluation
/// unrolls a known-format build into a left-nested chain of two-operand
/// concats — each rung copying the prefix again — and fusing them into one
/// variadic concat is the same Θ(n²) → Θ(n) with no loop involved.
pub fn build_append_loops(module: &mut Module) {
    let mut counter = 0;

    for (_, func) in module.funcs_mut() {
        build_in_body(&mut func.region, &mut counter);
    }
    for (_, clsr) in module.clsrs_mut() {
        build_in_body(&mut clsr.region, &mut counter);
    }
}

/// The buffer flavor a builder loop accumulates; fixes which append/flatten
/// ops the rewrite mints.
#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    Bin,
    Arr,
}

struct Candidate {
    header: BlockName,
    slot: usize,
    param: ValueName,
    /// Step binding name → its piece operand.
    steps: HashMap<ValueName, ValueName>,
    kind: Kind,
}

fn build_in_body(root: &mut Region, counter: &mut usize) {
    let candidates = find_candidates(root);

    for candidate in &candidates {
        apply_outside(root, candidate, counter);
    }
}

// ---------------------------------------------------------------------------
// Recognition
// ---------------------------------------------------------------------------

fn find_candidates(root: &Region) -> Vec<Candidate> {
    let resumes = collect_resumes(root);
    let mut candidates = Vec::new();

    collect_candidates(root, &resumes, &mut candidates);

    candidates
}

/// Every block name some call/host/cell tail resumes at. A resume target's
/// parameters carry the callee's result — not reseedable entry edges.
fn collect_resumes(region: &Region) -> HashSet<BlockName> {
    fn collect(region: &Region, resumes: &mut HashSet<BlockName>) {
        match &region.tail {
            Tail::Call(CallTarget::Direct { resume, .. })
            | Tail::Call(CallTarget::Indirect { resume, .. }) => {
                resumes.insert(resume.clone());
            }
            Tail::Host(host) => {
                resumes.insert(host.resume().clone());
            }
            Tail::Cell(cell) => {
                resumes.insert(cell.resume().clone());
            }
            Tail::Jump(_) | Tail::Match(_) | Tail::Unreachable => {}
        }

        for (_, block) in &region.blocks {
            collect(&block.region, resumes);
        }
    }

    let mut resumes = HashSet::new();
    collect(region, &mut resumes);

    resumes
}

fn collect_candidates(
    region: &Region,
    resumes: &HashSet<BlockName>,
    candidates: &mut Vec<Candidate>,
) {
    for (name, block) in &region.blocks {
        if !resumes.contains(name) && is_loop_header(name, block) {
            for slot in 0..block.params.len() {
                candidates.extend(analyze(name, block, slot));
            }
        }

        collect_candidates(&block.region, resumes, candidates);
    }
}

fn analyze(header: &BlockName, block: &Block, slot: usize) -> Option<Candidate> {
    let param = &block.params[slot];
    let bindings = collect_bindings(&block.region);
    let uses = count_uses(&block.region);

    // Classify every back-edge argument in the slot: pass-through or step.
    let mut steps = HashMap::new();
    let mut kind = None;

    for jump in back_edges(&block.region, header) {
        let arg = jump.params.get(slot)?;

        if arg == param {
            continue;
        }

        let (operands, step_kind) = match bindings.get(arg)? {
            Value::Eval(Code::BinConcat(operands)) => (operands, Kind::Bin),
            Value::Eval(Code::ArrConcat(operands)) => (operands, Kind::Arr),
            _ => return None,
        };

        // Accumulator strictly on the left, exactly one piece, and the piece
        // is not the accumulator (a self-doubling concat is not an append).
        if operands.len() != 2 || &operands[0] != param || &operands[1] == param {
            return None;
        }

        // The step's only use is this jump argument.
        if uses.get(arg).copied().unwrap_or(0) != 1 {
            return None;
        }

        if *kind.get_or_insert(step_kind) != step_kind {
            return None;
        }

        steps.insert(arg.clone(), operands[1].clone());
    }

    // An all-pass-through slot appends nothing; leave it alone.
    let kind = kind?;

    // Account for every use of the accumulator, region by region: a region
    // that can reach a back edge may hold only the recognized uses (step left
    // operands and pass-through jump arguments); anything else — an
    // observation mid-loop the parts array could not satisfy — declines.
    // Uses in exit-only regions become the escapes the rewrite flattens.
    let reaches = compute_reach(&block.region, header);
    check_uses(
        &block.region,
        &mut Vec::new(),
        header,
        slot,
        param,
        &steps,
        &reaches,
    )?;

    Some(Candidate {
        header: header.clone(),
        slot,
        param: param.clone(),
        steps,
        kind,
    })
}

/// Every binding in the subtree, by name. Names are unique per body, so one
/// flat map covers any depth.
fn collect_bindings(region: &Region) -> HashMap<ValueName, Value> {
    fn collect(region: &Region, bindings: &mut HashMap<ValueName, Value>) {
        for (name, value) in &region.values {
            bindings.insert(name.clone(), value.clone());
        }

        for (_, block) in &region.blocks {
            collect(&block.region, bindings);
        }
    }

    let mut bindings = HashMap::new();
    collect(region, &mut bindings);

    bindings
}

fn count_uses(region: &Region) -> HashMap<ValueName, usize> {
    struct Count(HashMap<ValueName, usize>);

    impl Sink for Count {
        fn value_use(&mut self, name: &ValueName) {
            *self.0.entry(name.clone()).or_insert(0) += 1;
        }
    }

    let mut count = Count(HashMap::new());
    walk_region(region, &mut count);

    count.0
}

/// Every jump target in the subtree that re-enters `header` — the back edges.
fn back_edges<'a>(region: &'a Region, header: &BlockName) -> Vec<&'a JumpTarget> {
    fn collect<'a>(region: &'a Region, header: &BlockName, edges: &mut Vec<&'a JumpTarget>) {
        for jump in tail_jump_targets(&region.tail) {
            if &jump.target == header {
                edges.push(jump);
            }
        }

        for (_, block) in &region.blocks {
            collect(&block.region, header, edges);
        }
    }

    let mut edges = Vec::new();
    collect(region, header, &mut edges);

    edges
}

fn tail_jump_targets(tail: &Tail) -> Vec<&JumpTarget> {
    match tail {
        Tail::Jump(jump) => vec![jump],
        Tail::Match(target) => {
            let mut jumps: Vec<&JumpTarget> = target.cases.values().collect();
            if let Some(jump) = &target.default {
                jumps.push(jump);
            }
            jumps
        }
        _ => vec![],
    }
}

fn tail_jump_targets_mut(tail: &mut Tail) -> Vec<&mut JumpTarget> {
    match tail {
        Tail::Jump(jump) => vec![jump],
        Tail::Match(target) => {
            let mut jumps: Vec<&mut JumpTarget> = target.cases.values_mut().collect();
            if let Some(jump) = &mut target.default {
                jumps.push(jump);
            }
            jumps
        }
        _ => vec![],
    }
}

/// Which subtree regions (keyed by their block-index path from the header's
/// region) can reach a back edge. A region reaches when its tail targets the
/// header, or targets any subtree block whose region reaches — call resumes
/// included, since the call returns there and continues.
fn compute_reach(root: &Region, header: &BlockName) -> HashMap<Vec<usize>, bool> {
    struct Node {
        targets: Vec<BlockName>,
    }

    fn index(
        region: &Region,
        path: &mut Vec<usize>,
        nodes: &mut HashMap<Vec<usize>, Node>,
        blocks: &mut HashMap<BlockName, Vec<usize>>,
    ) {
        nodes.insert(
            path.clone(),
            Node {
                targets: tail_targets(&region.tail).into_iter().cloned().collect(),
            },
        );

        for (position, (name, block)) in region.blocks.iter().enumerate() {
            path.push(position);
            blocks.insert(name.clone(), path.clone());
            index(&block.region, path, nodes, blocks);
            path.pop();
        }
    }

    let mut nodes = HashMap::new();
    let mut blocks = HashMap::new();
    index(root, &mut Vec::new(), &mut nodes, &mut blocks);

    let mut reaches: HashMap<Vec<usize>, bool> = nodes.keys().map(|k| (k.clone(), false)).collect();

    loop {
        let mut changed = false;

        for (path, node) in &nodes {
            if reaches[path] {
                continue;
            }

            let hit = node.targets.iter().any(|target| {
                target == header
                    || blocks
                        .get(target)
                        .is_some_and(|target_path| reaches[target_path])
            });

            if hit {
                reaches.insert(path.clone(), true);
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    reaches
}

/// Validate the accumulator's uses region by region (own straight line and
/// tail only — nested blocks are their own regions). Returns `None` when a
/// back-edge-reaching region holds a use beyond the recognized ones.
fn check_uses(
    region: &Region,
    path: &mut Vec<usize>,
    header: &BlockName,
    slot: usize,
    param: &ValueName,
    steps: &HashMap<ValueName, ValueName>,
    reaches: &HashMap<Vec<usize>, bool>,
) -> Option<()> {
    let uses = region_own_uses(region, param);

    let mut recognized = 0;

    // A step binding holds exactly one use of the accumulator (its left
    // operand — enforced at recognition).
    for (name, _) in &region.values {
        if steps.contains_key(name) {
            recognized += 1;
        }
    }

    for jump in tail_jump_targets(&region.tail) {
        if &jump.target == header && jump.params.get(slot) == Some(param) {
            recognized += 1;
        }
    }

    if reaches[path.as_slice()] {
        if uses != recognized {
            return None;
        }
    } else if recognized > 0 {
        // A step or pass-through in a region that cannot reach a back edge
        // should be impossible; decline rather than reason about it.
        return None;
    }

    for (position, (_, block)) in region.blocks.iter().enumerate() {
        path.push(position);
        check_uses(&block.region, path, header, slot, param, steps, reaches)?;
        path.pop();
    }

    Some(())
}

fn value_uses_name(value: &Value, param: &ValueName) -> bool {
    struct Found<'a> {
        param: &'a ValueName,
        found: bool,
    }

    impl Sink for Found<'_> {
        fn value_use(&mut self, name: &ValueName) {
            if name == self.param {
                self.found = true;
            }
        }
    }

    let mut found = Found {
        param,
        found: false,
    };
    walk_value_uses(value, &mut found);

    found.found
}

/// How many times `param` occurs in this region's own values and tail —
/// nested blocks excluded.
fn region_own_uses(region: &Region, param: &ValueName) -> usize {
    struct Count<'a> {
        param: &'a ValueName,
        count: usize,
    }

    impl Sink for Count<'_> {
        fn value_use(&mut self, name: &ValueName) {
            if name == self.param {
                self.count += 1;
            }
        }
    }

    let mut count = Count { param, count: 0 };

    for (_, value) in &region.values {
        walk_value_uses(value, &mut count);
    }
    walk_tail(&region.tail, &mut count);

    count.count
}

// ---------------------------------------------------------------------------
// Rewrite
// ---------------------------------------------------------------------------

/// Walk the body outside the loop: reseed every entry edge, and hand the
/// header's block over to the subtree rewrite when it is reached.
fn apply_outside(region: &mut Region, candidate: &Candidate, counter: &mut usize) {
    for jump in tail_jump_targets_mut(&mut region.tail) {
        if jump.target == candidate.header {
            let seed = mangle::builder_seed(*counter);
            *counter += 1;

            let init = jump.params[candidate.slot].clone();
            region
                .values
                .push((seed.clone(), Value::Pure(Data::Arr(vec![init]))));
            jump.params[candidate.slot] = seed;
        }
    }

    for (name, block) in &mut region.blocks {
        if name == &candidate.header {
            let reaches = compute_reach(&block.region, &candidate.header);
            apply_inside(
                &mut block.region,
                &mut Vec::new(),
                candidate,
                &reaches,
                counter,
            );
        } else {
            apply_outside(&mut block.region, candidate, counter);
        }
    }
}

/// Walk the loop subtree: convert steps to appends, and flatten the
/// accumulator once per exit region that observes it.
fn apply_inside(
    region: &mut Region,
    path: &mut Vec<usize>,
    candidate: &Candidate,
    reaches: &HashMap<Vec<usize>, bool>,
    counter: &mut usize,
) {
    for (name, value) in &mut region.values {
        if let Some(piece) = candidate.steps.get(name) {
            // The parts array is an `Arr` regardless of carrier; the carrier
            // only decides which flatten the escapes mint.
            *value = Value::Eval(Code::ArrAppend(candidate.param.clone(), piece.clone()));
        }
    }

    if !reaches[path.as_slice()] && region_own_uses(region, &candidate.param) > 0 {
        flatten_escapes(region, candidate, counter);
    }

    for (position, (_, block)) in region.blocks.iter_mut().enumerate() {
        path.push(position);
        apply_inside(&mut block.region, path, candidate, reaches, counter);
        path.pop();
    }
}

/// Materialise the parts array once for this region and substitute its own
/// uses: insert `whole = flatten(parts)` before the first using binding (or
/// after all bindings when only the tail observes it).
fn flatten_escapes(region: &mut Region, candidate: &Candidate, counter: &mut usize) {
    let whole = mangle::builder_flat(*counter);
    *counter += 1;

    let first_use = region
        .values
        .iter()
        .position(|(_, value)| value_uses_name(value, &candidate.param))
        .unwrap_or(region.values.len());

    struct Substitute<'a> {
        param: &'a ValueName,
        whole: &'a ValueName,
    }

    impl SinkMut for Substitute<'_> {
        fn value_use(&mut self, name: &mut ValueName) {
            if name == self.param {
                *name = self.whole.clone();
            }
        }
    }

    let mut substitute = Substitute {
        param: &candidate.param,
        whole: &whole,
    };

    for (_, value) in &mut region.values[first_use..] {
        walk_value_uses_mut(value, &mut substitute);
    }
    walk_tail_uses_mut(&mut region.tail, &mut substitute);

    let flatten = match candidate.kind {
        Kind::Bin => Code::BinFlatten(candidate.param.clone()),
        Kind::Arr => Code::ArrFlatten(candidate.param.clone()),
    };
    region
        .values
        .insert(first_use, (whole, Value::Eval(flatten)));
}

// ---------------------------------------------------------------------------
// Straight-line companion
// ---------------------------------------------------------------------------

/// Fuse a left-nested concat chain into one variadic concat: when a
/// `BinConcat`/`ArrConcat` binding's only use is as an operand of a later
/// concat of the same carrier in the same region, splice its operands in
/// place — `concat([concat([a, b]), c])` becomes `concat([a, b, c])`, one
/// allocation and one copy per byte instead of one per rung.
pub fn fuse_concat_chains(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        fuse_in_tree(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        fuse_in_tree(&mut clsr.region);
    }
}

fn fuse_in_tree(region: &mut Region) {
    let uses = count_uses(region);
    fuse_in_region(region, &uses);
}

fn fuse_in_region(region: &mut Region, uses: &HashMap<ValueName, usize>) {
    // Concat bindings seen so far in this region's straight line, with their
    // *current* operands (updated as fusions land, so chains collapse fully).
    let mut concats: HashMap<ValueName, (usize, Kind, Vec<ValueName>)> = HashMap::new();
    let mut fused: HashSet<ValueName> = HashSet::new();

    for position in 0..region.values.len() {
        let name = region.values[position].0.clone();

        let (kind, operands) = match &region.values[position].1 {
            Value::Eval(Code::BinConcat(operands)) => (Kind::Bin, operands.clone()),
            Value::Eval(Code::ArrConcat(operands)) => (Kind::Arr, operands.clone()),
            _ => continue,
        };

        let fusable = |operand: &ValueName| {
            uses.get(operand).copied().unwrap_or(0) == 1
                && concats
                    .get(operand)
                    .is_some_and(|(other, other_kind, _)| *other < position && *other_kind == kind)
        };

        let operands = if operands.iter().any(&fusable) {
            let mut spliced = Vec::with_capacity(operands.len());

            for operand in &operands {
                if fusable(operand) {
                    spliced.extend(concats[operand].2.iter().cloned());
                    fused.insert(operand.clone());
                } else {
                    spliced.push(operand.clone());
                }
            }

            let code = match kind {
                Kind::Bin => Code::BinConcat(spliced.clone()),
                Kind::Arr => Code::ArrConcat(spliced.clone()),
            };
            region.values[position].1 = Value::Eval(code);

            spliced
        } else {
            operands
        };

        concats.insert(name, (position, kind, operands));
    }

    region.values.retain(|(name, _)| !fused.contains(name));

    for (_, block) in &mut region.blocks {
        fuse_in_region(&mut block.region, uses);
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

    /// The canonical builder loop: enter `b@loop[i, acc]` with `(n, e)`; the
    /// body appends `piece` and loops via a match, or exits to `done`, which
    /// jumps out carrying `acc`.
    fn builder_module(step: Value, exit_arg: &str) -> Module {
        let done = Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v(exit_arg)]),
            },
        };

        let header = Block {
            params: vec![v("i"), v("acc")],
            region: Region {
                preallocs: vec![],
                values: vec![
                    (v("piece"), Value::Pure(Data::Bin(vec![0x61]))),
                    (v("step"), step),
                ],
                blocks: vec![(b("done"), done)],
                tail: Tail::Match(MatchTarget {
                    operand: v("i"),
                    cases: [(
                        0,
                        JumpTarget {
                            target: b("done"),
                            params: vec![],
                        },
                    )]
                    .into(),
                    default: Some(JumpTarget {
                        target: b("b@loop"),
                        params: vec![v("i"), v("step")],
                    }),
                }),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![(v("e"), Value::Pure(Data::Bin(vec![])))],
                blocks: vec![(b("b@loop"), header)],
                tail: jump("b@loop", vec![v("n"), v("e")]),
            },
        );
        module
    }

    fn main_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    #[test]
    fn rewrites_the_canonical_builder_loop() {
        let mut module = builder_module(
            Value::Eval(Code::BinConcat(vec![v("acc"), v("piece")])),
            "acc",
        );

        build_append_loops(&mut module);

        let entry = main_region(&module);

        // Entry edge reseeded: parts = [e]; jump carries the seed.
        let (seed_name, seed) = entry.values.last().unwrap();
        assert_eq!(seed_name.as_str(), "parts@builder#0");
        assert!(matches!(seed, Value::Pure(Data::Arr(elems)) if elems == &vec![v("e")]));
        match &entry.tail {
            Tail::Jump(target) => assert_eq!(target.params[1], seed_name.clone()),
            other => panic!("expected entry jump, got {other:?}"),
        }

        // Step became a reference append.
        let header = &entry.blocks[0].1.region;
        assert!(matches!(
            &header.values[1].1,
            Value::Eval(Code::ArrAppend(p, k)) if p == &v("acc") && k == &v("piece")
        ));

        // The exit block flattens once and jumps out with the whole.
        let done = &header.blocks[0].1.region;
        let (whole, flatten) = &done.values[0];
        assert_eq!(whole.as_str(), "whole@builder#1");
        assert!(matches!(flatten, Value::Eval(Code::BinFlatten(p)) if p == &v("acc")));
        match &done.tail {
            Tail::Jump(target) => assert_eq!(target.params, vec![whole.clone()]),
            other => panic!("expected exit jump, got {other:?}"),
        }
    }

    #[test]
    fn declines_accumulator_on_the_right() {
        // concat(piece, acc) prepends — not an append; everything stays.
        let mut module = builder_module(
            Value::Eval(Code::BinConcat(vec![v("piece"), v("acc")])),
            "acc",
        );

        build_append_loops(&mut module);

        let header = &main_region(&module).blocks[0].1.region;
        assert!(matches!(
            &header.values[1].1,
            Value::Eval(Code::BinConcat(_))
        ));
    }

    #[test]
    fn declines_observed_accumulator_inside_the_loop() {
        // The loop reads len(acc) each iteration: a mid-loop observation the
        // parts array cannot satisfy, so the whole slot declines.
        let done = Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![],
                tail: jump("r", vec![v("acc")]),
            },
        };
        let header = Block {
            params: vec![v("i"), v("acc")],
            region: Region {
                preallocs: vec![],
                values: vec![
                    (v("piece"), Value::Pure(Data::Bin(vec![0x61]))),
                    (v("len"), Value::Eval(Code::BinLen(v("acc")))),
                    (
                        v("step"),
                        Value::Eval(Code::BinConcat(vec![v("acc"), v("piece")])),
                    ),
                ],
                blocks: vec![(b("done"), done)],
                tail: Tail::Match(MatchTarget {
                    operand: v("len"),
                    cases: [(
                        0,
                        JumpTarget {
                            target: b("done"),
                            params: vec![],
                        },
                    )]
                    .into(),
                    default: Some(JumpTarget {
                        target: b("b@loop"),
                        params: vec![v("i"), v("step")],
                    }),
                }),
            },
        };

        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![(v("e"), Value::Pure(Data::Bin(vec![])))],
                blocks: vec![(b("b@loop"), header)],
                tail: jump("b@loop", vec![v("n"), v("e")]),
            },
        );

        build_append_loops(&mut module);

        let header = &main_region(&module).blocks[0].1.region;
        assert!(matches!(
            &header.values[2].1,
            Value::Eval(Code::BinConcat(_))
        ));
    }

    #[test]
    fn declines_resume_target_headers() {
        // b@loop is also a call resume: its parameter carries the callee's
        // result, so entry edges cannot be reseeded.
        let mut module = builder_module(
            Value::Eval(Code::BinConcat(vec![v("acc"), v("piece")])),
            "acc",
        );

        // Turn the entry into a call resuming at the header.
        {
            let region = &mut module.funcs_mut()[0].1.region;
            region.tail = Tail::Call(CallTarget::Direct {
                target: FuncName::from("g"),
                params: vec![],
                resume: b("b@loop"),
            });
        }

        build_append_loops(&mut module);

        let header = &main_region(&module).blocks[0].1.region;
        assert!(matches!(
            &header.values[1].1,
            Value::Eval(Code::BinConcat(_))
        ));
    }

    #[test]
    fn leaves_pass_through_only_slots_alone() {
        // The slot is threaded but never appended to — nothing to gain.
        let mut module = builder_module(Value::Eval(Code::BinLen(v("piece"))), "acc");

        // Back edge passes acc through unchanged.
        {
            let region = &mut module.funcs_mut()[0].1.region;
            let header = &mut region.blocks[0].1;
            if let Tail::Match(target) = &mut header.region.tail {
                target.default.as_mut().unwrap().params = vec![v("i"), v("acc")];
            }
        }

        build_append_loops(&mut module);

        let entry = main_region(&module);
        assert_eq!(entry.values.len(), 1, "no seed inserted");
    }

    #[test]
    fn rewrites_arr_carrier_loops_with_arr_flatten() {
        let mut module = builder_module(
            Value::Eval(Code::ArrConcat(vec![v("acc"), v("piece")])),
            "acc",
        );

        build_append_loops(&mut module);

        let header = &main_region(&module).blocks[0].1.region;
        assert!(matches!(
            &header.values[1].1,
            Value::Eval(Code::ArrAppend(..))
        ));
        let done = &header.blocks[0].1.region;
        assert!(matches!(
            &done.values[0].1,
            Value::Eval(Code::ArrFlatten(..))
        ));
    }

    #[test]
    fn fuses_left_nested_concat_chains() {
        // v1 = concat(a, b); v2 = concat(v1, c)  ==>  v2 = concat(a, b, c)
        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![
                    (v("a"), Value::Pure(Data::Bin(vec![1]))),
                    (v("b"), Value::Pure(Data::Bin(vec![2]))),
                    (v("c"), Value::Pure(Data::Bin(vec![3]))),
                    (v("v1"), Value::Eval(Code::BinConcat(vec![v("a"), v("b")]))),
                    (v("v2"), Value::Eval(Code::BinConcat(vec![v("v1"), v("c")]))),
                ],
                blocks: vec![],
                tail: jump("r", vec![v("v2")]),
            },
        );

        fuse_concat_chains(&mut module);

        let region = main_region(&module);
        assert_eq!(region.values.len(), 4, "intermediate binding dropped");
        assert!(matches!(
            &region.values[3].1,
            Value::Eval(Code::BinConcat(ops)) if ops == &vec![v("a"), v("b"), v("c")]
        ));
    }

    #[test]
    fn keeps_multi_use_intermediates() {
        // v1 is observed on its own — fusing would duplicate its copy.
        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![
                    (v("a"), Value::Pure(Data::Bin(vec![1]))),
                    (v("b"), Value::Pure(Data::Bin(vec![2]))),
                    (v("v1"), Value::Eval(Code::BinConcat(vec![v("a"), v("b")]))),
                    (v("v2"), Value::Eval(Code::BinConcat(vec![v("v1"), v("a")]))),
                ],
                blocks: vec![],
                tail: jump("r", vec![v("v1"), v("v2")]),
            },
        );

        fuse_concat_chains(&mut module);

        let region = main_region(&module);
        assert_eq!(region.values.len(), 4);
        assert!(matches!(
            &region.values[3].1,
            Value::Eval(Code::BinConcat(ops)) if ops == &vec![v("v1"), v("a")]
        ));
    }

    #[test]
    fn does_not_fuse_across_carriers() {
        let mut module = Module::new();
        add_main(
            &mut module,
            Region {
                preallocs: vec![],
                values: vec![
                    (v("a"), Value::Pure(Data::Arr(vec![]))),
                    (v("v1"), Value::Eval(Code::ArrConcat(vec![v("a"), v("a")]))),
                    (v("v2"), Value::Eval(Code::BinConcat(vec![v("v1")]))),
                ],
                blocks: vec![],
                tail: jump("r", vec![v("v2")]),
            },
        );

        fuse_concat_chains(&mut module);

        assert_eq!(main_region(&module).values.len(), 3);
    }
}
