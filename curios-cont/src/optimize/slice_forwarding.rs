use {
    super::*,
    curios_base::{Carrier, Entropy, SuffixRead},
    std::collections::HashMap,
};

/// Slice forwarding: re-base an aggregate access made through a slice onto the
/// slice's underlying buffer, so the slice never has to be materialised.
///
/// The free-monoid recursors for `Lst`/`Bin` are erased to a `Nat` loop over the
/// aggregate's length (`core::erase`), and the cons arm recovers its *tail* by
/// re-slicing the original buffer at every step: `tail := slice(xs, len - i, len)`.
/// A slice is an `array.new` + `array.copy` (`cont::into_wasm`), so a fold that
/// consumes its tail copies an `i`-long suffix on iteration `i` — `Θ(n²)` over the
/// whole walk. But a slice is only ever *read* through the three suffix-view
/// primitives, each reading through to the base in `O(1)` — see the canonical
/// re-base laws on [`Carrier`](curios_base::Carrier).
///
/// Rewriting each consumer this way drops the slice's last use; the now-dead
/// pure slice is reclaimed by the dead-code sweep that follows, turning the
/// quadratic fold linear. A consumer that is *not* one of the three suffix reads —
/// a closure call, a host write, `concat`/`append`/`eql` — keeps the
/// slice, so an escaping tail still materialises exactly once (the irreducible
/// cost). The `slice(slice(..))` rule re-bases through to the deepest buffer, so a
/// chain of sub-slices collapses to a single re-based access in one pass.
///
/// Soundness rides on the same premise as [`simplify_maps`](super::simplify_maps):
/// aggregates are immutable, so `b[s + j] == slice(b, s, e)[j]` for every in-bounds
/// access, and a fully-forwarded slice's allocation is unobservable.
pub(crate) fn forward_slices(module: &mut Module) {
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

/// The mutable state threaded through the forwarding rewrite: the registry of known
/// slices, the offset-name source, and the prelude bindings spliced in just before
/// the binding currently being rewritten.
struct ForwardCtx<'a> {
    slices: &'a mut HashMap<ValueName, Slice>,
    offsets: &'a Entropy,
    prelude: &'a mut Vec<(ValueName, Value)>,
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
            Value::Eval(code) => forward_eval(
                &name,
                code,
                &mut ForwardCtx {
                    slices: &mut *slices,
                    offsets,
                    prelude: &mut rewritten,
                },
            ),
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
fn forward_eval(name: &ValueName, code: Code, ctx: &mut ForwardCtx) -> Value {
    let Some((operand, carrier, read)) = classify(&code) else {
        return Value::Eval(code);
    };

    match read {
        // `len(slice(b, s, e))` ⟶ `e − s`.
        SuffixRead::Len => match lookup(ctx.slices, &operand, carrier) {
            Some(slice) => Value::Eval(Code::NatSub(slice.end, slice.start)),
            None => Value::Eval(code),
        },
        // `get(slice(b, s, e), j)` ⟶ `get(b, s + j)`.
        SuffixRead::Get(index) => match lookup(ctx.slices, &operand, carrier) {
            Some(slice) => {
                let offset = rebased_index(&slice.start, &index, ctx);
                Value::Eval(rebuild_get(carrier, slice.base, offset))
            }
            None => Value::Eval(code),
        },
        // `slice(b, p, q)` — re-base onto the deepest base and register `name`.
        SuffixRead::Slice(start, end) => forward_slice(name, carrier, operand, start, end, ctx),
    }
}

/// Classify an `Eval`'d `Code` as a [`SuffixRead`] of some operand buffer, if it is
/// one: the operand that might resolve to a slice, its carrier, and which of the
/// three reads it performs. Anything else is left for the caller to pass through.
fn classify(code: &Code) -> Option<(ValueName, Carrier, SuffixRead<ValueName>)> {
    Some(match code {
        Code::LstLen(operand) => (operand.clone(), Carrier::Lst, SuffixRead::Len),
        Code::BinLen(operand) => (operand.clone(), Carrier::Bin, SuffixRead::Len),
        Code::LstGet(operand, index) => (
            operand.clone(),
            Carrier::Lst,
            SuffixRead::Get(index.clone()),
        ),
        Code::BinGet(operand, index) => (
            operand.clone(),
            Carrier::Bin,
            SuffixRead::Get(index.clone()),
        ),
        Code::LstSlice(operand, start, end) => (
            operand.clone(),
            Carrier::Lst,
            SuffixRead::Slice(start.clone(), end.clone()),
        ),
        Code::BinSlice(operand, start, end) => (
            operand.clone(),
            Carrier::Bin,
            SuffixRead::Slice(start.clone(), end.clone()),
        ),
        _ => return None,
    })
}

/// Forward (and register) a slice binding. When its operand is itself a slice,
/// the window re-bases onto the deeper buffer (`[s + p, s + q)` of the base);
/// either way `name` is recorded so its own consumers forward through to the
/// resolved base.
fn forward_slice(
    name: &ValueName,
    carrier: Carrier,
    operand: ValueName,
    start: ValueName,
    end: ValueName,
    ctx: &mut ForwardCtx,
) -> Value {
    let (base, start, end) = match lookup(ctx.slices, &operand, carrier) {
        Some(inner) => (
            inner.base,
            rebased_index(&inner.start, &start, ctx),
            rebased_index(&inner.start, &end, ctx),
        ),
        None => (operand, start, end),
    };

    ctx.slices.insert(
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

/// `base + index`, materialised as a fresh `NatAdd` binding pushed onto the prelude.
fn rebased_index(base: &ValueName, index: &ValueName, ctx: &mut ForwardCtx) -> ValueName {
    let offset = mangle::slice_offset(ctx.offsets.fresh());
    ctx.prelude.push((
        offset.clone(),
        Value::Eval(Code::NatAdd(base.clone(), index.clone())),
    ));
    offset
}

fn rebuild_slice(carrier: Carrier, base: ValueName, start: ValueName, end: ValueName) -> Code {
    match carrier {
        Carrier::Lst => Code::LstSlice(base, start, end),
        Carrier::Bin => Code::BinSlice(base, start, end),
    }
}

fn rebuild_get(carrier: Carrier, base: ValueName, index: ValueName) -> Code {
    match carrier {
        Carrier::Lst => Code::LstGet(base, index),
        Carrier::Bin => Code::BinGet(base, index),
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
        // xs : Lst, s, e : Nat ; w = slice(xs, s, e) ; n = len(w)
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::LstSlice(v("xs"), v("s"), v("e")))),
            (v("n"), Value::Eval(Code::LstLen(v("w")))),
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
            (v("w"), Value::Eval(Code::LstSlice(v("xs"), v("s"), v("e")))),
            (v("x"), Value::Eval(Code::LstGet(v("w"), v("j")))),
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
            Value::Eval(Code::LstGet(base, offset)) => {
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
            (v("w"), Value::Eval(Code::LstSlice(v("xs"), v("s"), v("e")))),
            (v("u"), Value::Eval(Code::LstSlice(v("w"), v("p"), v("q")))),
            (v("y"), Value::Eval(Code::LstGet(v("u"), v("k")))),
        ]);

        forward_slices(&mut module);

        // `u` now slices `xs` directly...
        let inner = body(&module)
            .iter()
            .find_map(|(name, value)| match value {
                Value::Eval(Code::LstSlice(base, ..)) if name == &v("u") => Some(base.clone()),
                _ => None,
            })
            .expect("u survives as a slice of the base");
        assert_eq!(inner, v("xs"));

        // ...and the final get reads `xs`, not any intermediate window.
        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::LstGet(base, _)) => assert_eq!(base, &v("xs")),
            other => panic!("expected get(xs, _), got {other:?}"),
        }
    }

    #[test]
    fn a_bin_access_does_not_forward_through_an_lst_slice() {
        // Carriers never cross in well-typed code; guard against it anyway.
        let mut module = module_with(vec![
            (v("w"), Value::Eval(Code::LstSlice(v("xs"), v("s"), v("e")))),
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
            (v("xs"), Value::Pure(Data::Lst(vec![]))),
            (v("n"), Value::Eval(Code::LstLen(v("xs")))),
        ]);

        forward_slices(&mut module);

        match &body(&module).last().unwrap().1 {
            Value::Eval(Code::LstLen(operand)) => assert_eq!(operand, &v("xs")),
            other => panic!("expected len(xs) untouched, got {other:?}"),
        }
    }
}
