//! Locals allocated by liveness: each emitted function's locals folded onto the fewest slots their live ranges allow, one value type a slot.
//!
//! **A local lives from its first write to its last read.** The function's instructions are numbered in the order they run within one pass over its blocks, and a local's range is the span from the first `local.set` or `local.tee` of it to the last instruction naming it. A branch leaves a block forward, past positions the span already covers, and a loop's back edge returns to its start: a local first written before a loop and named inside it may be read there on any round, so its range reaches the loop's end. Two locals of one value type whose ranges do not meet share a slot, the earlier's value dead before the later's first write.
//!
//! **A read the walk cannot show follows a write keeps a slot of its own.** WebAssembly validates a non-nullable local by the same block-scoped rule the walk follows — a write initializes a local until the end of the block it stands in — so every read of a non-nullable local passes it, and folding two such locals adds writes and removes none. A nullable or numeric local may be read before any write, its default being what is read; such a local is given a slot nothing else uses, from the function's start, and keeps its default.
//!
//! **Every local alike.** A value the continuations name and a scratch local an emitter minted are both read by the walk as they stand in the function, as LLVM's `WebAssemblyRegColoring` colors every virtual register once the code is final. Parameters keep their places.

#[cfg(test)]
mod tests;

use std::{
    cmp::Reverse,
    collections::{BTreeSet, BinaryHeap, HashMap},
};

/// Fold the locals of each function of `module` by liveness. See the module documentation.
pub(crate) fn allocate_locals(module: &mut curios_wasm::Module) {
    curios_profile::profile!("allocate_locals");
    for func in module.funcs_mut() {
        allocate(func);
    }
}

fn allocate(func: &mut curios_wasm::Func) {
    let ranges = Ranges::of(func);
    let (locals, renamed) = ranges.slots(&func.locals);
    rename(&mut func.expr.instrs, &renamed);
    func.locals = locals;
}

/// Each local's live range over the function's positions.
struct Ranges {
    /// The index of each local among the function's locals; a parameter is none of them.
    index: HashMap<curios_wasm::LocalName, usize>,
    /// Each local's first write, last naming, and whether a read was found that no write shows to follow.
    first: Vec<Option<usize>>,
    last: Vec<Option<usize>>,
    unshown: Vec<bool>,
    /// The loop each local must be live through: of the loops it is named in that began after its first write, the one that ends last.
    through: Vec<Option<usize>>,
    /// Each loop's first and last positions, by the order the walk entered them.
    loops: Vec<(usize, usize)>,
}

/// One block the walk is inside: the loop it is, where it is one, the locals a write inside it initialized, and what is left of it to walk.
struct Open<'a> {
    rest: std::slice::Iter<'a, curios_wasm::Instr>,
    loop_: Option<usize>,
    initialized: Vec<usize>,
    /// An `if`'s other arm, walked after this one under the state the `if` began in.
    otherwise: Option<&'a [curios_wasm::Instr]>,
}

impl Ranges {
    fn of(func: &curios_wasm::Func) -> Self {
        let index = func
            .locals
            .iter()
            .enumerate()
            .map(|(position, (name, _))| (name.clone(), position))
            .collect::<HashMap<_, _>>();
        let count = func.locals.len();
        let mut ranges = Ranges {
            index,
            first: vec![None; count],
            last: vec![None; count],
            unshown: vec![false; count],
            through: vec![None; count],
            loops: Vec::new(),
        };
        let mut initialized = vec![false; count];
        // The open loops, outermost first, each by its index in `loops`: what a naming inside them is checked against.
        let mut open_loops: Vec<usize> = Vec::new();
        let mut position = 0;
        let mut stack = vec![Open {
            rest: func.expr.instrs.iter(),
            loop_: None,
            initialized: Vec::new(),
            otherwise: None,
        }];

        while let Some(open) = stack.last_mut() {
            let Some(instr) = open.rest.next() else {
                let closed = stack.pop().expect("a block is open");
                for local in &closed.initialized {
                    initialized[*local] = false;
                }
                if let Some(loop_) = closed.loop_ {
                    ranges.loops[loop_].1 = position;
                    open_loops.pop();
                }
                if let Some(otherwise) = closed.otherwise {
                    stack.push(Open {
                        rest: otherwise.iter(),
                        loop_: None,
                        initialized: Vec::new(),
                        otherwise: None,
                    });
                }
                continue;
            };
            position += 1;
            match instr {
                curios_wasm::Instr::Block { instructions, .. } => stack.push(Open {
                    rest: instructions.iter(),
                    loop_: None,
                    initialized: Vec::new(),
                    otherwise: None,
                }),
                curios_wasm::Instr::Loop { instructions, .. } => {
                    let loop_ = ranges.loops.len();
                    ranges.loops.push((position, position));
                    open_loops.push(loop_);
                    stack.push(Open {
                        rest: instructions.iter(),
                        loop_: Some(loop_),
                        initialized: Vec::new(),
                        otherwise: None,
                    });
                }
                curios_wasm::Instr::If {
                    then_instructions,
                    else_instructions,
                    ..
                } => stack.push(Open {
                    rest: then_instructions.iter(),
                    loop_: None,
                    initialized: Vec::new(),
                    otherwise: Some(else_instructions),
                }),
                curios_wasm::Instr::LocalGet { local_name } => {
                    if let Some(&local) = ranges.index.get(local_name) {
                        ranges.unshown[local] |= !initialized[local];
                        ranges.named(local, position, &open_loops);
                    }
                }
                curios_wasm::Instr::LocalSet { local_name }
                | curios_wasm::Instr::LocalTee { local_name } => {
                    if let Some(&local) = ranges.index.get(local_name) {
                        ranges.first[local].get_or_insert(position);
                        if !initialized[local] {
                            initialized[local] = true;
                            stack
                                .last_mut()
                                .expect("a block is open")
                                .initialized
                                .push(local);
                        }
                        ranges.named(local, position, &open_loops);
                    }
                }
                _ => {}
            }
        }

        ranges
    }

    /// Note that `local` is named at `position`, inside `open_loops`.
    fn named(&mut self, local: usize, position: usize, open_loops: &[usize]) {
        self.last[local] = Some(position);
        let Some(first) = self.first[local] else {
            return;
        };
        // The outermost open loop that began after the first write: the loops' starts rise inward, so it is the first past the write. One found at a later naming ends no earlier than one found before it, which it holds or follows, so the latest is the one to be live through.
        let outermost = open_loops.partition_point(|loop_| self.loops[*loop_].0 <= first);
        if let Some(&loop_) = open_loops.get(outermost) {
            self.through[local] = Some(loop_);
        }
    }

    /// The slots, each named after the first local it holds, and each local's slot.
    fn slots(
        &self,
        locals: &[(curios_wasm::LocalName, curios_wasm::ValType)],
    ) -> (
        Vec<(curios_wasm::LocalName, curios_wasm::ValType)>,
        HashMap<curios_wasm::LocalName, curios_wasm::LocalName>,
    ) {
        // Each named local's range: from the start, to the end, where a read was found that no write shows to follow.
        let mut ranges = (0..locals.len())
            .filter_map(|local| {
                let last = self.last[local]?;
                Some(match (self.unshown[local], self.first[local]) {
                    (false, Some(first)) => {
                        let through = self.through[local].map_or(0, |loop_| self.loops[loop_].1);
                        (first, last.max(through), local)
                    }
                    _ => (0, usize::MAX, local),
                })
            })
            .collect::<Vec<_>>();
        ranges.sort_unstable();

        let mut slots: Vec<(curios_wasm::LocalName, curios_wasm::ValType)> = Vec::new();
        // The free slots of each value type in use, lowest first.
        let mut free: Vec<(curios_wasm::ValType, BTreeSet<usize>)> = Vec::new();
        // The slots in use, by the last position of the range holding each.
        let mut held: BinaryHeap<Reverse<(usize, usize)>> = BinaryHeap::new();
        let mut renamed = HashMap::new();
        for (start, end, local) in ranges {
            while let Some(&Reverse((last, slot))) = held.peek()
                && last < start
            {
                held.pop();
                let val_type = &slots[slot].1;
                pool(&mut free, val_type).insert(slot);
            }
            let (name, val_type) = &locals[local];
            let slot = match pool(&mut free, val_type).pop_first() {
                Some(slot) => slot,
                None => {
                    slots.push((name.clone(), val_type.clone()));
                    slots.len() - 1
                }
            };
            held.push(Reverse((end, slot)));
            renamed.insert(name.clone(), slots[slot].0.clone());
        }

        (slots, renamed)
    }
}

/// The free slots of `val_type`, an empty set where none has been freed.
fn pool<'a>(
    free: &'a mut Vec<(curios_wasm::ValType, BTreeSet<usize>)>,
    val_type: &curios_wasm::ValType,
) -> &'a mut BTreeSet<usize> {
    let found = free.iter().position(|(known, _)| known == val_type);
    let index = found.unwrap_or_else(|| {
        free.push((val_type.clone(), BTreeSet::new()));
        free.len() - 1
    });
    &mut free[index].1
}

/// Name each local by its slot in `instrs` and every block they nest.
fn rename(
    instrs: &mut [curios_wasm::Instr],
    renamed: &HashMap<curios_wasm::LocalName, curios_wasm::LocalName>,
) {
    let mut pending = vec![instrs];
    while let Some(instrs) = pending.pop() {
        for instr in instrs {
            match instr {
                curios_wasm::Instr::Block { instructions, .. }
                | curios_wasm::Instr::Loop { instructions, .. } => pending.push(instructions),
                curios_wasm::Instr::If {
                    then_instructions,
                    else_instructions,
                    ..
                } => {
                    pending.push(then_instructions);
                    pending.push(else_instructions);
                }
                curios_wasm::Instr::LocalGet { local_name }
                | curios_wasm::Instr::LocalSet { local_name }
                | curios_wasm::Instr::LocalTee { local_name } => {
                    if let Some(slot) = renamed.get(local_name) {
                        *local_name = slot.clone();
                    }
                }
                _ => {}
            }
        }
    }
}
