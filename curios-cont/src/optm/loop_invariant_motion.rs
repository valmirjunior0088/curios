use {super::*, std::collections::HashSet};

/// Loop-invariant code motion: move a loop header's per-iteration work that
/// never varies out to the region that enters the loop.
///
/// [`tail_recursion`](super::tail_recursion) converts a self-tail-recursive
/// function into `region R: Jump(b@loop, ...)` with the header block carrying
/// the body — and everything in the header's straight line re-executes every
/// iteration, including work whose operands never change: a `len` of a buffer
/// the loop only reads, or the headline case, a closure rebuilt each iteration
/// from captures the loop merely threads through. This pass moves those
/// bindings to the end of `R.values`, so they run once per loop *entry*.
///
/// The envelope is exactly the converted-loop shape, and deliberately so:
///
/// - **Entry**: `R.tail` is a `Jump` to a block `B` declared in `R.blocks`
///   whose subtree jumps back to `B` ([`is_loop_header`](super::is_loop_header)).
///   Hoisting appends to `R.values`, an *ancestor* binding — in scope inside
///   `B` with no new block or parameter threading. A call/host/cell resume
///   header never matches (`R.tail` is not a `Jump`), so hoisted work cannot
///   jump ahead of an effect.
/// - **Execution count**: `R` runs once per loop entry and the entry jump is
///   unconditional, so the loop body runs at least once — a hoisted binding
///   executes neither more often (per entry) nor speculatively.
/// - **Trap point**: between the end of `R.values` and the header's first
///   value there is only the entry jump — effects live exclusively at tails,
///   and `R`'s tail *is* that jump — so a hoisted trapping op (`NatDiv`,
///   `ArrGet`) traps at an observably identical point, on iteration one.
///
/// Only the header's straight line is scanned; its nested blocks are
/// conditional per iteration and moving them would be speculation. What
/// hoists: `Value::Eval` (except `ArrMap`, which invokes a possibly-effectful
/// closure) and every allocating `Value::Pure` — aggregates, bytestrings, and
/// boxed floats — whose operands are all loop-invariant. `Nat`/`Int` literals
/// are immediates, not allocations, and stay put; prealloc shells' fills stay
/// beside their shells. Loop-varying is decided without dataflow: a name is
/// varying iff it is a header parameter or bound anywhere in the header's
/// subtree — anything else is an enclosing-scope or const name, invariant by
/// the scoping law. Hoisted names are removed from the varying set as the scan
/// walks, so a chain of invariant work cascades out in one pass.
pub fn hoist_loop_invariants(module: &mut Module) {
    for (_, func) in module.funcs_mut() {
        hoist_in_region(&mut func.region);
    }
    for (_, clsr) in module.clsrs_mut() {
        hoist_in_region(&mut clsr.region);
    }
}

fn hoist_in_region(region: &mut Region) {
    // Inner loops first: their hoists land in enclosing regions, where an
    // outer loop's scan may pick them up in the same pass.
    for (_, block) in &mut region.blocks {
        hoist_in_region(&mut block.region);
    }

    let Tail::Jump(entry) = &region.tail else {
        return;
    };

    let Some(index) = region
        .blocks
        .iter()
        .position(|(name, block)| name == &entry.target && is_loop_header(name, block))
    else {
        return;
    };

    let header = &mut region.blocks[index].1;
    let mut varying: HashSet<ValueName> = header.params.iter().cloned().collect();
    varying.extend(bound_values(&header.region));

    let shells: HashSet<ValueName> = header
        .region
        .preallocs
        .iter()
        .map(|(name, _)| name.clone())
        .collect();

    let mut hoisted = Vec::new();
    let mut stays = Vec::new();

    for (name, value) in header.region.values.drain(..) {
        if !shells.contains(&name) && is_invariant(&value, &varying) {
            varying.remove(&name);
            hoisted.push((name, value));
        } else {
            stays.push((name, value));
        }
    }

    header.region.values = stays;
    region.values.extend(hoisted);
}

fn is_invariant(value: &Value, varying: &HashSet<ValueName>) -> bool {
    let movable = match value {
        // Copy propagation owns aliases; ArrMap invokes a closure whose region
        // may end in an effectful tail.
        Value::Alias(_) | Value::Eval(Code::ArrMap(..)) => false,
        Value::Eval(_) => true,
        Value::Pure(data) => match data {
            // Immediates — nothing saved by moving them.
            Data::Nat(_) | Data::Int(_) => false,
            // Everything else allocates per execution.
            Data::Flt(_) | Data::Bin(_) | Data::Arr(_) | Data::Tpl(_) | Data::Clsr(..) => true,
        },
    };

    movable && {
        let mut invariant = Invariant {
            varying,
            holds: true,
        };
        walk_value_uses(value, &mut invariant);

        invariant.holds
    }
}

struct Invariant<'a> {
    varying: &'a HashSet<ValueName>,
    holds: bool,
}

impl Sink for Invariant<'_> {
    fn value_use(&mut self, name: &ValueName) {
        if self.varying.contains(name) {
            self.holds = false;
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

    /// The converted-loop shape: `f(x@loop)` enters `b@loop[x]`, whose body
    /// holds `values` and jumps back to itself through a match arm.
    fn looped_func(values: Vec<(ValueName, Value)>, back_args: Vec<ValueName>) -> Module {
        let header = Block {
            params: vec![v("x")],
            region: Region {
                preallocs: vec![],
                values,
                blocks: vec![],
                tail: jump("b@loop", back_args),
            },
        };

        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![v("x@loop").into()],
                resume: b("r"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(v("k"), Value::Pure(Data::Nat(7)))],
                    blocks: vec![(b("b@loop"), header)],
                    tail: jump("b@loop", vec![v("x@loop")]),
                },
            },
        );
        module
    }

    fn entry_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region
    }

    fn header_region(module: &Module) -> &Region {
        &module.funcs()[0].1.region.blocks[0].1.region
    }

    #[test]
    fn hoists_invariant_eval_out_of_the_header() {
        // add(k, k) reads only the enclosing binding — once per entry, not per
        // iteration.
        let mut module = looped_func(
            vec![
                (v("v0"), Value::Eval(Code::NatAdd(v("k"), v("k")))),
                (v("v1"), Value::Eval(Code::NatAdd(v("x"), v("k")))),
            ],
            vec![v("v1")],
        );

        hoist_loop_invariants(&mut module);

        let entry = entry_region(&module);
        assert_eq!(entry.values.last().unwrap().0, v("v0"));
        // The varying add stays.
        assert_eq!(header_region(&module).values.len(), 1);
        assert_eq!(header_region(&module).values[0].0, v("v1"));
    }

    #[test]
    fn cascades_through_hoisted_names() {
        // v1 depends on v0; once v0 hoists it is no longer varying, so v1
        // follows in the same scan.
        let mut module = looped_func(
            vec![
                (v("v0"), Value::Eval(Code::NatAdd(v("k"), v("k")))),
                (v("v1"), Value::Eval(Code::NatMul(v("v0"), v("k")))),
            ],
            vec![v("x")],
        );

        hoist_loop_invariants(&mut module);

        let entry = entry_region(&module);
        let names: Vec<_> = entry.values.iter().map(|(n, _)| n.clone()).collect();
        assert_eq!(names, vec![v("k"), v("v0"), v("v1")]);
        assert!(header_region(&module).values.is_empty());
    }

    #[test]
    fn hoists_invariant_closure_construction() {
        // The headline case: a closure rebuilt every iteration from an
        // invariant capture becomes one allocation per entry.
        let mut module = looped_func(
            vec![(
                v("v0"),
                Value::Pure(Data::Clsr(ClsrName::from("c0"), vec![v("k")])),
            )],
            vec![v("x")],
        );

        hoist_loop_invariants(&mut module);

        assert_eq!(entry_region(&module).values.last().unwrap().0, v("v0"));
    }

    #[test]
    fn leaves_varying_and_immediate_values_in_place() {
        let mut module = looped_func(
            vec![
                // Varying: reads the header param.
                (v("v0"), Value::Eval(Code::NatAdd(v("x"), v("k")))),
                // Immediate: hoisting a Nat saves nothing.
                (v("v1"), Value::Pure(Data::Nat(1))),
            ],
            vec![v("v0")],
        );

        hoist_loop_invariants(&mut module);

        assert_eq!(header_region(&module).values.len(), 2);
    }

    #[test]
    fn ignores_non_loop_entry_jumps() {
        // The target block never jumps back — not a loop, nothing moves.
        let block = Block {
            params: vec![v("x")],
            region: Region {
                preallocs: vec![],
                values: vec![(v("v0"), Value::Eval(Code::NatAdd(v("k"), v("k"))))],
                blocks: vec![],
                tail: jump("r", vec![v("v0")]),
            },
        };

        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![v("n").into()],
                resume: b("r"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(v("k"), Value::Pure(Data::Nat(7)))],
                    blocks: vec![(b("b1"), block)],
                    tail: jump("b1", vec![v("n")]),
                },
            },
        );

        hoist_loop_invariants(&mut module);

        let header = &module.funcs()[0].1.region.blocks[0].1.region;
        assert_eq!(header.values.len(), 1);
    }

    #[test]
    fn scans_only_the_header_straight_line() {
        // An invariant value inside a nested (conditional) block stays: moving
        // it would speculate work the iteration may skip.
        let nested = Block {
            params: vec![],
            region: Region {
                preallocs: vec![],
                values: vec![(v("v9"), Value::Eval(Code::NatAdd(v("k"), v("k"))))],
                blocks: vec![],
                tail: jump("b@loop", vec![v("v9")]),
            },
        };
        let header = Block {
            params: vec![v("x")],
            region: Region {
                preallocs: vec![],
                values: vec![],
                blocks: vec![(b("b2"), nested)],
                tail: jump("b2", vec![]),
            },
        };

        let mut module = Module::new();
        module.add_func(
            FuncName::from("f"),
            Func {
                params: vec![v("x@loop").into()],
                resume: b("r"),
                region: Region {
                    preallocs: vec![],
                    values: vec![(v("k"), Value::Pure(Data::Nat(7)))],
                    blocks: vec![(b("b@loop"), header)],
                    tail: jump("b@loop", vec![v("x@loop")]),
                },
            },
        );

        hoist_loop_invariants(&mut module);

        let nested = &module.funcs()[0].1.region.blocks[0].1.region.blocks[0].1;
        assert_eq!(nested.region.values.len(), 1);
    }
}
