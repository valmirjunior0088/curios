use super::*;

/// A read-only traversal of a Cont region tree.
///
/// The walker owns the recursion and the (large) per-node enumeration — notably
/// the `Code` operand match, written once in [`walk_code_operands!`] and shared
/// by both walkers. The smaller `Tail`/`Data` walkers are genuinely asymmetric
/// (only the read-only side fires `func_ref`/`clsr_ref` events), so those pairs
/// are kept in step by hand when a variant is added. A [`Sink`] only reacts to
/// the leaf events it cares about; every method defaults to a no-op, so a
/// harvester overrides just the ones it needs.
///
/// Binders — block parameters and the names on the left of `values`/`preallocs`
/// — are deliberately *not* reported as uses; only operand (use) positions fire
/// [`Sink::value_use`].
pub trait Sink {
    /// A `ValueName` in a use position.
    fn value_use(&mut self, name: &ValueName) {
        let _ = name;
    }

    /// A closure referenced by a value or a prealloc shell.
    fn clsr_ref(&mut self, name: &ClsrName) {
        let _ = name;
    }

    /// A function referenced by a direct call.
    fn func_ref(&mut self, name: &FuncName) {
        let _ = name;
    }
}

/// Walk a region and every nested block, firing events into `sink`.
pub fn walk_region(region: &Region, sink: &mut impl Sink) {
    for (_, clsr) in &region.preallocs {
        sink.clsr_ref(clsr);
    }

    for (_, value) in &region.values {
        walk_value(value, sink);
    }

    walk_tail(&region.tail, sink);

    for (_, block) in &region.blocks {
        walk_region(&block.region, sink);
    }
}

fn walk_value(value: &Value, sink: &mut impl Sink) {
    match value {
        Value::Pure(data) => walk_data(data, sink),
        Value::Eval(code) => walk_code(code, sink),
        Value::Alias(source) => sink.value_use(source),
    }
}

/// Walk a single `Data`, firing reference events into `sink`. Exposed so a lone
/// const value can be harvested without an enclosing region (see
/// [`harvest::data_refs`](super::harvest)).
pub fn walk_data_refs(data: &Data, sink: &mut impl Sink) {
    walk_data(data, sink);
}

/// Walk a single `Value`'s operands, firing [`Sink::value_use`] for each.
/// Exposed so a pass that classifies *where* a name occurs (value operands vs.
/// call/jump arguments) can reuse the operand enumeration on values alone and
/// handle tails itself (see
/// [`dead_argument_elimination`](super::dead_argument_elimination)).
pub fn walk_value_uses(value: &Value, sink: &mut impl Sink) {
    walk_value(value, sink);
}

fn walk_data(data: &Data, sink: &mut impl Sink) {
    match data {
        Data::Nat(_) | Data::Int(_) | Data::Flt(_) | Data::Bin(_) => {}
        Data::Lst(elems) | Data::Tpl(elems) => walk_uses(elems, sink),
        Data::Clsr(clsr, captures) => {
            sink.clsr_ref(clsr);
            walk_uses(captures, sink);
        }
    }
}

/// The block names a tail transfers control to: a `Jump`'s target, every `Match`
/// arm (cases and default), a `Call`'s resume continuation, or a `Host` op's
/// resume. The one place this edge enumeration lives, shared by predecessor
/// counting and reachability.
pub fn tail_targets(tail: &Tail) -> Vec<&BlockName> {
    match tail {
        Tail::Jump(jump) => vec![&jump.target],
        Tail::Match(target) => {
            let mut targets: Vec<&BlockName> =
                target.cases.values().map(|jump| &jump.target).collect();
            if let Some(jump) = &target.default {
                targets.push(&jump.target);
            }
            targets
        }
        Tail::Call(CallTarget::Direct { resume, .. })
        | Tail::Call(CallTarget::Indirect { resume, .. }) => vec![resume],
        Tail::Host(host) => vec![host.resume()],
        Tail::Cell(cell) => vec![cell.resume()],
        Tail::Unreachable => vec![],
    }
}

/// Walk a single `Tail`'s operands and references, firing events into `sink`.
/// Exposed (like [`walk_value_uses`] for values) so a pass can harvest a tail's
/// operands on their own — e.g. seeding the liveness roots in
/// [`dead_code_elimination`](super::dead_code_elimination) — without a region to
/// wrap them.
pub fn walk_tail(tail: &Tail, sink: &mut impl Sink) {
    match tail {
        Tail::Jump(target) => walk_jump(target, sink),
        Tail::Match(target) => {
            sink.value_use(&target.operand);
            for jump in target.cases.values() {
                walk_jump(jump, sink);
            }
            if let Some(jump) = &target.default {
                walk_jump(jump, sink);
            }
        }
        Tail::Call(CallTarget::Direct { target, params, .. }) => {
            sink.func_ref(target);
            walk_uses(params, sink);
        }
        Tail::Call(CallTarget::Indirect { target, params, .. }) => {
            sink.value_use(target);
            walk_uses(params, sink);
        }
        Tail::Host(host) => {
            for name in host.operands() {
                sink.value_use(name);
            }
        }
        Tail::Cell(cell) => {
            for name in cell.operands() {
                sink.value_use(name);
            }
        }
        Tail::Unreachable => {}
    }
}

fn walk_jump(target: &JumpTarget, sink: &mut impl Sink) {
    walk_uses(&target.params, sink);
}

/// The `Code` operand enumeration, written once and instantiated for both
/// walkers: matching a `&Code` binds `&ValueName` operands and feeds
/// [`Sink::value_use`]; matching a `&mut Code` binds `&mut ValueName` and feeds
/// [`SinkMut::value_use`] — the same source text serves both, by match
/// ergonomics.
macro_rules! walk_code_operands {
    ($code:expr, $sink:expr) => {{
        use Code::*;

        match $code {
            // Binary operands.
            NatEql(a, b)
            | NatNeq(a, b)
            | NatAdd(a, b)
            | NatSub(a, b)
            | NatMul(a, b)
            | NatLt(a, b)
            | NatDiv(a, b)
            | NatRem(a, b)
            | NatGt(a, b)
            | NatLte(a, b)
            | NatGte(a, b)
            | NatAnd(a, b)
            | NatOr(a, b)
            | NatXor(a, b)
            | NatShl(a, b)
            | NatShr(a, b)
            | NatRotl(a, b)
            | NatRotr(a, b)
            | IntEql(a, b)
            | IntNeq(a, b)
            | IntAdd(a, b)
            | IntSub(a, b)
            | IntMul(a, b)
            | IntDiv(a, b)
            | IntRem(a, b)
            | IntLt(a, b)
            | IntGt(a, b)
            | IntLte(a, b)
            | IntGte(a, b)
            | IntAnd(a, b)
            | IntOr(a, b)
            | IntXor(a, b)
            | IntShl(a, b)
            | IntShr(a, b)
            | IntRotl(a, b)
            | IntRotr(a, b)
            | FltAdd(a, b)
            | FltSub(a, b)
            | FltMul(a, b)
            | FltDiv(a, b)
            | FltRem(a, b)
            | FltEql(a, b)
            | FltNeq(a, b)
            | FltLt(a, b)
            | FltGt(a, b)
            | FltLte(a, b)
            | FltGte(a, b)
            | FltMin(a, b)
            | FltMax(a, b)
            | FltCopysign(a, b)
            | BinEql(a, b)
            | BinGet(a, b)
            | BinAppend(a, b)
            | LstGet(a, b)
            | LstAppend(a, b)
            | LstMap(a, b) => {
                $sink.value_use(a);
                $sink.value_use(b);
            }

            // Ternary operands.
            BinSlice(a, b, c) | LstSlice(a, b, c) => {
                $sink.value_use(a);
                $sink.value_use(b);
                $sink.value_use(c);
            }

            // Unary operands.
            NatClz(a)
            | NatCtz(a)
            | NatPopcnt(a)
            | NatEqz(a)
            | NatToInt(a)
            | NatToFlt(a)
            | IntClz(a)
            | IntCtz(a)
            | IntPopcnt(a)
            | IntEqz(a)
            | IntToNat(a)
            | IntToFlt(a)
            | FltNeg(a)
            | FltAbs(a)
            | FltSqrt(a)
            | FltFloor(a)
            | FltCeil(a)
            | FltTrunc(a)
            | FltNearest(a)
            | FltToNat(a)
            | FltToLeBin(a)
            | FltToInt(a)
            | BinLen(a)
            | LstLen(a)
            | TplGet(a, _) => $sink.value_use(a),

            // Variadic operands.
            BinConcat(operands) | LstConcat(operands) => {
                for name in operands {
                    $sink.value_use(name);
                }
            }
        }
    }};
}

fn walk_code(code: &Code, sink: &mut impl Sink) {
    walk_code_operands!(code, sink)
}

fn walk_uses(names: &[ValueName], sink: &mut impl Sink) {
    for name in names {
        sink.value_use(name);
    }
}

/// A mutable traversal: the same operand positions as [`Sink`], but each is
/// handed out as `&mut ValueName` so a pass can substitute it in place.
///
/// Only value-use positions are exposed — binders and closure/function
/// references are never rewritten by any current pass, so they are left out
/// rather than offered as no-op hooks.
pub trait SinkMut {
    fn value_use(&mut self, name: &mut ValueName);
}

/// The shared use-renamer for subtree clones: suffixes every value use whose
/// name is *bound* in the cloned subtree, leaving free names (enclosing-scope
/// and module-const references) untouched. Binders and block names are not
/// use positions, so each cloning pass renames those by hand to its own rules
/// ([`function_inlining`](super::function_inlining) maps the resume sentinel,
/// [`tag_threading`](super::tag_threading) gates on the bound block set).
pub struct FreshenUses<'a> {
    pub bound: &'a std::collections::HashSet<ValueName>,
    pub suffix: &'a str,
}

impl SinkMut for FreshenUses<'_> {
    fn value_use(&mut self, name: &mut ValueName) {
        if self.bound.contains(name) {
            *name = mangle::suffixed_value(name, self.suffix);
        }
    }
}

/// Walk a region and every nested block, offering each operand for rewriting.
pub fn walk_region_mut(region: &mut Region, sink: &mut impl SinkMut) {
    // Prealloc shells reference a `ClsrName`, never a value use — nothing to rewrite.
    for (_, value) in &mut region.values {
        walk_value_mut(value, sink);
    }

    walk_tail_mut(&mut region.tail, sink);

    for (_, block) in &mut region.blocks {
        walk_region_mut(&mut block.region, sink);
    }
}

fn walk_value_mut(value: &mut Value, sink: &mut impl SinkMut) {
    match value {
        Value::Pure(data) => walk_data_mut(data, sink),
        Value::Eval(code) => walk_code_mut(code, sink),
        Value::Alias(source) => sink.value_use(source),
    }
}

fn walk_data_mut(data: &mut Data, sink: &mut impl SinkMut) {
    match data {
        Data::Nat(_) | Data::Int(_) | Data::Flt(_) | Data::Bin(_) => {}
        Data::Lst(elems) | Data::Tpl(elems) => walk_uses_mut(elems, sink),
        Data::Clsr(_, captures) => walk_uses_mut(captures, sink),
    }
}

fn walk_tail_mut(tail: &mut Tail, sink: &mut impl SinkMut) {
    match tail {
        Tail::Jump(target) => walk_jump_mut(target, sink),
        Tail::Match(target) => {
            sink.value_use(&mut target.operand);
            for jump in target.cases.values_mut() {
                walk_jump_mut(jump, sink);
            }
            if let Some(jump) = &mut target.default {
                walk_jump_mut(jump, sink);
            }
        }
        Tail::Call(CallTarget::Direct { params, .. }) => walk_uses_mut(params, sink),
        Tail::Call(CallTarget::Indirect { target, params, .. }) => {
            sink.value_use(target);
            walk_uses_mut(params, sink);
        }
        Tail::Host(host) => {
            for name in host.operands_mut() {
                sink.value_use(name);
            }
        }
        Tail::Cell(cell) => {
            for name in cell.operands_mut() {
                sink.value_use(name);
            }
        }
        Tail::Unreachable => {}
    }
}

fn walk_jump_mut(target: &mut JumpTarget, sink: &mut impl SinkMut) {
    walk_uses_mut(&mut target.params, sink);
}

fn walk_code_mut(code: &mut Code, sink: &mut impl SinkMut) {
    walk_code_operands!(code, sink)
}

fn walk_uses_mut(names: &mut [ValueName], sink: &mut impl SinkMut) {
    for name in names {
        sink.value_use(name);
    }
}
