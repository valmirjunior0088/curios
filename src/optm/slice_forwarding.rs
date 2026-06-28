use {super::*, crate::Entropy, std::collections::HashMap};

/// Slice forwarding: re-base an aggregate access made through a slice onto the
/// slice's underlying buffer, so the slice never has to be materialised.
///
/// The free-monoid recursors for `Arr`/`Bin` are erased to a `Nat` loop over the
/// aggregate's length (`core::erase`), and the cons arm recovers its *tail* by
/// re-slicing the original buffer at every step: `tail := slice(xs, len - i, len)`.
/// A slice is an `array.new` + `array.copy` (`cont::to_wasm`), so a fold that
/// consumes its tail copies an `i`-long suffix on iteration `i` — `Θ(n²)` over the
/// whole walk. But a slice is only ever *read* through three primitives, and each
/// reads through to the base in `O(1)`:
///
/// ```text
/// len(  slice(b, s, e)    )   ⟶   e − s
/// get(  slice(b, s, e), j )   ⟶   get(b, s + j)
/// slice(slice(b, s, e), p, q) ⟶   slice(b, s + p, s + q)
/// ```
///
/// Rewriting each consumer this way drops the slice's last use; the now-dead
/// pure slice is reclaimed by the dead-code sweep that follows, turning the
/// quadratic fold linear. A consumer that is *not* one of these three — a closure
/// call, a host write, `concat`/`append`/`flatten`/`eql` — keeps the slice, so an
/// escaping tail still materialises exactly once (the irreducible cost). The
/// `slice(slice(..))` rule re-bases through to the deepest buffer, so a chain of
/// sub-slices collapses to a single re-based access in one pass.
///
/// Soundness rides on the same premise as [`simplify_maps`](super::simplify_maps):
/// aggregates are immutable, so `b[s + j] == slice(b, s, e)[j]` for every in-bounds
/// access, and a fully-forwarded slice's allocation is unobservable.
pub fn forward_slices(module: &mut Module) {
    // A private counter: the `v@slice#n` names it mints share a shape with no
    // other pass, so they never collide despite starting from zero each run.
    let offsets = Entropy::<usize>::new();

    for (_, func) in module.funcs_mut() {
        forward_in_region(&mut func.region, &mut HashMap::new(), &offsets);
    }
    for (_, clsr) in module.clsrs_mut() {
        forward_in_region(&mut clsr.region, &mut HashMap::new(), &offsets);
    }
}

/// Whether a tracked binding is an `Arr` slice or a `Bin` slice — a consumer only
/// forwards through a slice of its own carrier, though well-typed code can never
/// mix them.
#[derive(Clone, Copy, PartialEq)]
enum Carrier {
    Arr,
    Bin,
}

/// A binding known to be `slice(base, start, end)` of the given carrier. Every
/// field is a `ValueName` in scope wherever the slice itself is (a use is
/// dominated by the slice's definition, which is dominated by its operands).
#[derive(Clone)]
struct Slice {
    carrier: Carrier,
    base: ValueName,
    start: ValueName,
    end: ValueName,
}

/// Rewrite a region and its nested blocks in pre-order, threading the slice map
/// down. Bindings are SSA and globally unique within a function tree, and a use
/// is always dominated by its definition, so a flat map populated in pre-order
/// has every slice registered before the consumers that read it — and a slice
/// lingering from an already-walked sibling block is harmless, since no in-scope
/// use can name it.
fn forward_in_region(
    region: &mut Region,
    slices: &mut HashMap<ValueName, Slice>,
    offsets: &Entropy,
) {
    let mut rewritten = Vec::with_capacity(region.values.len());

    for (name, value) in std::mem::take(&mut region.values) {
        let value = match value {
            Value::Eval(code) => forward_eval(&name, code, slices, offsets, &mut rewritten),
            // An alias to a slice is a slice — register it so the alias's own
            // consumers forward too. (Copy propagation runs first and usually
            // threads these away, but tracking them costs nothing.)
            Value::Alias(source) => {
                if let Some(slice) = slices.get(&source).cloned() {
                    slices.insert(name.clone(), slice);
                }
                Value::Alias(source)
            }
            other => other,
        };

        rewritten.push((name, value));
    }

    region.values = rewritten;

    for (_, block) in &mut region.blocks {
        forward_in_region(&mut block.region, slices, offsets);
    }
}

/// Forward a single `Eval` binding. A slice-carried `len`/`get`/`slice` is
/// re-based onto the underlying buffer (minting any `start + index` adjustment
/// into `prelude`, which is spliced in just before this binding); a fresh slice
/// is registered under `name`; everything else passes through untouched.
fn forward_eval(
    name: &ValueName,
    code: Code,
    slices: &mut HashMap<ValueName, Slice>,
    offsets: &Entropy,
    prelude: &mut Vec<(ValueName, Value)>,
) -> Value {
    match code {
        Code::ArrLen(operand) => match lookup(slices, &operand, Carrier::Arr) {
            Some(slice) => Value::Eval(Code::NatSub(slice.end, slice.start)),
            None => Value::Eval(Code::ArrLen(operand)),
        },
        Code::BinLen(operand) => match lookup(slices, &operand, Carrier::Bin) {
            Some(slice) => Value::Eval(Code::NatSub(slice.end, slice.start)),
            None => Value::Eval(Code::BinLen(operand)),
        },
        Code::ArrGet(operand, index) => match lookup(slices, &operand, Carrier::Arr) {
            Some(slice) => {
                let offset = rebased_index(&slice.start, &index, offsets, prelude);
                Value::Eval(Code::ArrGet(slice.base, offset))
            }
            None => Value::Eval(Code::ArrGet(operand, index)),
        },
        Code::BinGet(operand, index) => match lookup(slices, &operand, Carrier::Bin) {
            Some(slice) => {
                let offset = rebased_index(&slice.start, &index, offsets, prelude);
                Value::Eval(Code::BinGet(slice.base, offset))
            }
            None => Value::Eval(Code::BinGet(operand, index)),
        },
        Code::ArrSlice(operand, start, end) => forward_slice(
            name,
            Carrier::Arr,
            operand,
            start,
            end,
            slices,
            offsets,
            prelude,
        ),
        Code::BinSlice(operand, start, end) => forward_slice(
            name,
            Carrier::Bin,
            operand,
            start,
            end,
            slices,
            offsets,
            prelude,
        ),
        other => Value::Eval(other),
    }
}

/// Forward (and register) a slice binding. When its operand is itself a slice,
/// the window re-bases onto the deeper buffer (`[s + p, s + q)` of the base);
/// either way `name` is recorded so its own consumers forward through to the
/// resolved base.
#[allow(clippy::too_many_arguments)]
fn forward_slice(
    name: &ValueName,
    carrier: Carrier,
    operand: ValueName,
    start: ValueName,
    end: ValueName,
    slices: &mut HashMap<ValueName, Slice>,
    offsets: &Entropy,
    prelude: &mut Vec<(ValueName, Value)>,
) -> Value {
    let (base, start, end) = match lookup(slices, &operand, carrier) {
        Some(inner) => (
            inner.base,
            rebased_index(&inner.start, &start, offsets, prelude),
            rebased_index(&inner.start, &end, offsets, prelude),
        ),
        None => (operand, start, end),
    };

    slices.insert(
        name.clone(),
        Slice {
            carrier,
            base: base.clone(),
            start: start.clone(),
            end: end.clone(),
        },
    );

    Value::Eval(rebuild_slice(carrier, base, start, end))
}

/// `base + index`, materialised as a fresh `NatAdd` binding pushed onto `prelude`.
fn rebased_index(
    base: &ValueName,
    index: &ValueName,
    offsets: &Entropy,
    prelude: &mut Vec<(ValueName, Value)>,
) -> ValueName {
    let offset = mangle::slice_offset(offsets.fresh());
    prelude.push((
        offset.clone(),
        Value::Eval(Code::NatAdd(base.clone(), index.clone())),
    ));
    offset
}

fn rebuild_slice(carrier: Carrier, base: ValueName, start: ValueName, end: ValueName) -> Code {
    match carrier {
        Carrier::Arr => Code::ArrSlice(base, start, end),
        Carrier::Bin => Code::BinSlice(base, start, end),
    }
}

/// The slice a name resolves to, if it is one of the requested carrier.
fn lookup(slices: &HashMap<ValueName, Slice>, name: &ValueName, carrier: Carrier) -> Option<Slice> {
    slices
        .get(name)
        .filter(|slice| slice.carrier == carrier)
        .cloned()
}

#[cfg(test)]
mod tests {
    use super::*;

    fn v(name: &str) -> ValueName {
        ValueName::from(name)
    }

    /// A `main` whose body is `values`, returning nothing of interest (an
    /// `Unreachable` tail keeps the harness from needing a real result).
    fn module_with(values: Vec<(ValueName, Value)>) -> Module {
        let mut module = Module::new();
        module.add_func(
            FuncName::from("main"),
            Func {
                params: vec![],
                resume: BlockName::from("b0"),
                region: Region {
                    preallocs: vec![],
                    values,
                    blocks: vec![],
                    tail: Tail::Unreachable,
                },
            },
        );
        module
    }

    fn body(module: &Module) -> &[(ValueName, Value)] {
        &module.funcs()[0].1.region.values
    }

    #[test]
    fn len_of_a_slice_becomes_the_window_width() {
        // xs : Arr, s, e : Nat ; w = slice(xs, s, e) ; n = len(w)
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::ArrSlice(v("xs"), v("s"), v("e")))),
            (v("n"), Value::Eval(Code::ArrLen(v("w")))),
        ]);

        forward_slices(&mut module);

        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::NatSub(end, start)) => {
                assert_eq!((end, start), (&v("e"), &v("s")));
            }
            other => panic!("expected len → end − start, got {other:?}"),
        }
    }

    #[test]
    fn get_through_a_slice_rebases_onto_the_base() {
        // w = slice(xs, s, e) ; x = get(w, j)  ⟶  off = s + j ; x = get(xs, off)
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::ArrSlice(v("xs"), v("s"), v("e")))),
            (v("x"), Value::Eval(Code::ArrGet(v("w"), v("j")))),
        ]);

        forward_slices(&mut module);
        let body = body(&module);

        // The offset binding was spliced in just before the rewritten `get`.
        let (offset_name, offset) = &body[1];
        match offset {
            Value::Eval(Code::NatAdd(start, index)) => {
                assert_eq!((start, index), (&v("s"), &v("j")));
            }
            other => panic!("expected an `s + j` offset, got {other:?}"),
        }
        match &body[2].1 {
            Value::Eval(Code::ArrGet(base, offset)) => {
                assert_eq!((base, offset), (&v("xs"), offset_name));
            }
            other => panic!("expected get(xs, off), got {other:?}"),
        }
    }

    #[test]
    fn slice_of_a_slice_collapses_to_the_base() {
        // w = slice(xs, s, e) ; u = slice(w, p, q) ; y = get(u, k)
        // u re-bases onto xs, and y forwards straight through it to xs.
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::ArrSlice(v("xs"), v("s"), v("e")))),
            (v("u"), Value::Eval(Code::ArrSlice(v("w"), v("p"), v("q")))),
            (v("y"), Value::Eval(Code::ArrGet(v("u"), v("k")))),
        ]);

        forward_slices(&mut module);

        // `u` now slices `xs` directly...
        let inner = body(&module)
            .iter()
            .find_map(|(name, value)| match value {
                Value::Eval(Code::ArrSlice(base, ..)) if name == &v("u") => Some(base.clone()),
                _ => None,
            })
            .expect("u survives as a slice of the base");
        assert_eq!(inner, v("xs"));

        // ...and the final get reads `xs`, not any intermediate window.
        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::ArrGet(base, _)) => assert_eq!(base, &v("xs")),
            other => panic!("expected get(xs, _), got {other:?}"),
        }
    }

    #[test]
    fn a_bin_access_does_not_forward_through_an_arr_slice() {
        // Carriers never cross in well-typed code; guard against it anyway.
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::ArrSlice(v("xs"), v("s"), v("e")))),
            (v("x"), Value::Eval(Code::BinGet(v("w"), v("j")))),
        ]);

        forward_slices(&mut module);

        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::BinGet(operand, index)) => {
                assert_eq!((operand, index), (&v("w"), &v("j")));
            }
            other => panic!("expected the bin get to be left alone, got {other:?}"),
        }
    }

    #[test]
    fn a_non_slice_access_is_left_alone() {
        let mut module = module_with(vec![
            (v("xs"), Value::Pure(Data::Arr(vec![]))),
            (v("n"), Value::Eval(Code::ArrLen(v("xs")))),
        ]);

        forward_slices(&mut module);

        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::ArrLen(operand)) => assert_eq!(operand, &v("xs")),
            other => panic!("expected len(xs) untouched, got {other:?}"),
        }
    }
}
