//! Deterministic printing — the `Stage::Cont`/`Stage::ContOptm` observer and the diffing surface.
//!
//! The continuation IR prints as nested CPS: a function's body is a chain of bindings closed by a transfer, and every function and continuation prints *at its binding site*, indented under it. Why this rung stops resembling Curios, and why the nesting rather than a flat block list, is `documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md`. The short of it is that the nesting *is* the scope: a continuation reading a value bound in the continuation that encloses it reads it in view, where a flat list of labelled blocks would have shown the same value as free. The same argument places the erased rung's lifted functions at their binding sites, and it applies here to values across continuations.
//!
//! **The elision rule is the erased rung's**, unchanged: print every declaration something else refers to, state each fact once at its definition site, and spell every reference as short as is unambiguous. Applied to identities, `~v`, `~f`, `~k` and `~r` print and the structural `~n` does not — a node is referred to only as its parent's `next` or `body`, which is exactly what nesting expresses. Applied to the header, only the rows the program reaches are declared: `prune_unreachable` leaves a row whose last construction it deleted, so the arena outlives what builds from it.
//!
//! **The dump is faithful**, with the same boundary: faithful forbids *hiding*, not *not repeating*. Nothing inlines a single-use binding, folds a constant, reorders a transfer or drops dead code. In particular no continuation is folded into a let-binding, though most of them have exactly one predecessor and could be, because a continuation is what this representation has instead of a let, and because the fall in continuation count from `cont` to `cont-optm` is the optimizer's whole visible effect.
//!
//! **Two deliberate spellings.** Every function owns a bodyless return continuation, and that sentinel is not printed: an edge to it is `return x` and a call whose `return_to` is it is `return ~f7(x)`, which is what makes a tail call legible where `-> ~k11` against a header forty lines away is not. And hints repeat at every reference, not only at definition sites — the scheme in `documentation/design/tools/a-printer-states-each-fact-once-where-it-is-bound.md` requires them at definitions and does not forbid more, and a dump nobody scrolls back through is better off self-describing.
//!
//! Equal modules print byte-identically. The renderer follows a chain of bindings in a loop and recurses, under `recurse`, into what a node binds, so a deep module is printed on a stack that grows for it; indentation saturates at a fixed depth so output stays linear in module size rather than quadratic in nesting.

#[cfg(test)]
mod tests;

use {
    super::{
        Atom, Callee, CellOp, ChannelOp, ContinuationId, Edge, FieldGroup, FunctionId, Intrinsic,
        IntrinsicCall, Literal, Module, Node, NodeId, RowId, Slot, ValueExpr, ValueId,
    },
    curios_num::{Grain, Rounding},
    curios_utilities::{grown, recurse},
    std::{
        collections::{BTreeMap, BTreeSet},
        fmt,
    },
};

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let reach = Reach::of(self);
        let uses = self.value_use_counts();
        grown(|| {
            Printer {
                module: self,
                reach: &reach,
                uses: &uses,
                out: f,
            }
            .print()
        })
    }
}

/// The nesting depth beyond which indentation stops growing, keeping printed size linear in module size.
const INDENT_SATURATION: usize = 64;

// === What the header declares ================================================

/// The nominal rows the program reaches.
///
/// A fixpoint rather than one sweep: a reached row's slots may name further rows through [`Slot::Row`], and a slot's carrier cannot be read without the identity it names having been introduced. The sweep is needed at all because the row table has no liveness — `prune_unreachable` never touches it, so a row whose last construction was deleted stays as an unreferenced entry.
struct Reach {
    rows: BTreeSet<RowId>,
}

impl Reach {
    fn of(module: &Module) -> Self {
        let mut rows = BTreeSet::new();
        let mut work = Vec::new();

        // Exhaustive rather than a wildcard, so a node variant that learns to name a row cannot reach the printer while silently missing this walk.
        for (_, node) in module.live_nodes() {
            match node {
                Node::LetValue { value, .. } => match value {
                    ValueExpr::Row(row, _) => work.push(*row),
                    ValueExpr::Literal(_) | ValueExpr::List(_) | ValueExpr::Tuple(_) => {}
                },
                Node::LetIntrinsic { op, .. } => {
                    if let Intrinsic::RowGet(row, _) = op {
                        work.push(*row);
                    }
                }
                Node::LetFun { .. }
                | Node::LetCont { .. }
                | Node::ApplyFun { .. }
                | Node::ApplyCont(_)
                | Node::Switch { .. }
                | Node::Foreign { .. }
                | Node::Cell { .. }
                | Node::Channel { .. }
                | Node::Intrinsic { .. }
                | Node::Halt { .. }
                | Node::Panic(_)
                | Node::Unreachable => {}
            }
        }

        while let Some(row) = work.pop() {
            if !rows.insert(row) {
                continue;
            }
            let Some(definition) = module.defined_row(row) else {
                continue;
            };
            for slot in &definition.slots {
                if let Slot::Row(inner) = slot {
                    work.push(*inner);
                }
            }
        }

        Self { rows }
    }
}

// === The walk ================================================================

/// What the lexical walk has already printed. A well-formed module leaves nothing out, and what is left over is printed anyway rather than dropped, because this printer is read while debugging modules that do not verify.
#[derive(Default)]
struct Seen {
    nodes: BTreeSet<NodeId>,
    functions: BTreeSet<FunctionId>,
    continuations: BTreeSet<ContinuationId>,
}

struct Printer<'m, 'a, 'f, 'o> {
    module: &'m Module,
    reach: &'a Reach,
    uses: &'a BTreeMap<ValueId, usize>,
    out: &'f mut fmt::Formatter<'o>,
}

impl Printer<'_, '_, '_, '_> {
    fn print(&mut self) -> fmt::Result {
        let declared = self.header()?;
        let mut seen = Seen::default();

        if let Some(entry) = self.module.entry() {
            if declared {
                writeln!(self.out)?;
            }
            self.binding("entry", entry, ";", &mut seen, 0)?;
        }

        self.unreached(&mut seen)
    }

    /// The reached rows, in identity order. Answers whether anything was declared, so the entry knows whether a separating blank line is owed.
    fn header(&mut self) -> Result<bool, fmt::Error> {
        let mut declared = false;
        for id in &self.reach.rows {
            let Some(row) = self.module.defined_row(*id) else {
                continue;
            };
            let slots = row
                .slots
                .iter()
                .map(|slot| slot_name(*slot))
                .collect::<Vec<_>>()
                .join(", ");
            writeln!(self.out, "row {}{}({slots})", id, hint(&row.debug_name))?;
            declared = true;
        }
        Ok(declared)
    }

    /// Whatever the lexical walk did not reach, so a malformed module still dumps whole. Printing an orphan reaches further orphans, so the sweep repeats until it finds none.
    fn unreached(&mut self, seen: &mut Seen) -> fmt::Result {
        let module = self.module;
        let mut announced = false;

        loop {
            let function = module
                .live_functions()
                .map(|(id, _)| id)
                .find(|id| !seen.functions.contains(id));
            let continuation = module
                .live_continuations()
                .map(|(id, _)| id)
                .find(|id| !seen.continuations.contains(id));
            if function.is_none() && continuation.is_none() {
                break;
            }

            if !announced {
                writeln!(self.out)?;
                writeln!(self.out, "unreached")?;
                announced = true;
            }
            match (function, continuation) {
                (Some(id), _) => self.binding("let", id, ";", seen, 1)?,
                (None, Some(id)) => {
                    seen.continuations.insert(id);
                    let definition = module.continuation(id).expect("a live continuation");
                    let head = format!(
                        "cont {}{}{}",
                        id,
                        hint(&definition.debug_name),
                        self.params(Some(id), &definition.params)
                    );
                    // An orphan continuation belongs to no function the walk entered, so no sentinel is in hand and every transfer prints as the jump it is.
                    self.line(1, format!("{head} ="))?;
                    self.node(seen, definition.body, None, ";", 2)?;
                }
                (None, None) => break,
            }
        }

        Ok(())
    }

    /// One line of text at `indent`.
    fn line(&mut self, indent: usize, text: impl AsRef<str>) -> fmt::Result {
        for _ in 0..indent.min(INDENT_SATURATION) {
            self.out.write_str("    ")?;
        }
        self.out.write_str(text.as_ref())?;
        self.out.write_str("\n")
    }

    /// One function's definition at `indent`, headed by `keyword` — `entry` for the module's own, `let` everywhere else.
    fn binding(
        &mut self,
        keyword: &str,
        id: FunctionId,
        suffix: &'static str,
        seen: &mut Seen,
        indent: usize,
    ) -> fmt::Result {
        seen.functions.insert(id);
        let module = self.module;
        let Some(definition) = module.function(id) else {
            return self.line(indent, format!("{keyword} <missing {id}>{suffix}"));
        };
        let head = format!(
            "{keyword} {}{}{} =",
            id,
            hint(&definition.debug_name),
            self.params(None, &definition.params)
        );
        self.line(indent, head)?;

        self.node(
            seen,
            definition.body,
            Some(definition.return_cont),
            suffix,
            indent + 1,
        )
    }

    /// A node and the region under it, at `indent`. `ret` is the enclosing function's return sentinel, which is what tells a return from a jump and a tail call from a call; it is absent only where the walk entered a continuation no function owns.
    fn node(
        &mut self,
        seen: &mut Seen,
        id: NodeId,
        ret: Option<ContinuationId>,
        suffix: &'static str,
        indent: usize,
    ) -> fmt::Result {
        recurse(|| self.chain(seen, id, ret, suffix, indent))
    }

    /// The chain of bindings from `id` to the transfer that closes it: each binding is a line and the walk goes on to the node after it, and the functions and continuations a node binds are printed under it, each a region of its own.
    fn chain(
        &mut self,
        seen: &mut Seen,
        mut id: NodeId,
        ret: Option<ContinuationId>,
        suffix: &'static str,
        indent: usize,
    ) -> fmt::Result {
        let module = self.module;

        loop {
            // The node graph is a tree in every module the verifier admits — `verify_function_body` refuses a second parent — but this printer also reads modules that do not verify, so a second visit is marked rather than followed.
            if !seen.nodes.insert(id) {
                return self.line(indent, format!("<{id} again>{suffix}"));
            }
            let Some(node) = module.node(id) else {
                return self.line(indent, format!("<missing {id}>{suffix}"));
            };

            id = match node {
                Node::LetValue {
                    result,
                    value,
                    next,
                } => {
                    let binding =
                        format!("let {} = {};", self.binder(*result), self.value_expr(value));
                    self.line(indent, binding)?;
                    *next
                }
                Node::LetIntrinsic {
                    result,
                    op,
                    args,
                    next,
                } => {
                    let binding = format!(
                        "let {} = {};",
                        self.binder(*result),
                        self.intrinsic(op, args)
                    );
                    self.line(indent, binding)?;
                    *next
                }
                Node::LetFun { functions, body } => {
                    for (index, &function) in functions.iter().enumerate() {
                        let keyword = if index == 0 { "let" } else { "and" };
                        let last = index + 1 == functions.len();
                        self.binding(keyword, function, if last { ";" } else { "" }, seen, indent)?;
                    }
                    *body
                }
                Node::LetCont {
                    continuations,
                    body,
                } => {
                    for (index, &continuation) in continuations.iter().enumerate() {
                        let keyword = if index == 0 { "cont" } else { "and" };
                        let last = index + 1 == continuations.len();
                        let closing = if last { ";" } else { "" };
                        seen.continuations.insert(continuation);
                        match module.continuation(continuation) {
                            Some(definition) => {
                                let head = format!(
                                    "{keyword} {}{}{} =",
                                    continuation,
                                    hint(&definition.debug_name),
                                    self.params(Some(continuation), &definition.params)
                                );
                                self.line(indent, head)?;
                                self.node(seen, definition.body, ret, closing, indent + 1)?;
                            }
                            None => self.line(
                                indent,
                                format!("{keyword} <missing {continuation}>{closing}"),
                            )?,
                        }
                    }
                    *body
                }
                Node::ApplyFun {
                    callee,
                    args,
                    return_to,
                } => {
                    let call = format!("{}({})", self.callee(callee), self.operands(args));
                    let transfer = self.transfer(call, *return_to, ret, suffix);
                    return self.line(indent, transfer);
                }
                Node::Foreign {
                    function,
                    args,
                    return_to,
                } => {
                    let call = format!(
                        "{}/{}({})",
                        function.namespace(),
                        function.name(),
                        self.operands(args)
                    );
                    let transfer = self.transfer(call, *return_to, ret, suffix);
                    return self.line(indent, transfer);
                }
                Node::Cell {
                    op,
                    args,
                    return_to,
                } => {
                    let call = format!("{}({})", cell_name(*op), self.operands(args));
                    let transfer = self.transfer(call, *return_to, ret, suffix);
                    return self.line(indent, transfer);
                }
                Node::Channel {
                    op,
                    args,
                    return_to,
                } => {
                    let call = format!("{}({})", channel_name(*op), self.operands(args));
                    let transfer = self.transfer(call, *return_to, ret, suffix);
                    return self.line(indent, transfer);
                }
                Node::Intrinsic {
                    op,
                    args,
                    return_to,
                } => {
                    let call = format!("{}({})", intrinsic_call_name(*op), self.operands(args));
                    let transfer = self.transfer(call, *return_to, ret, suffix);
                    return self.line(indent, transfer);
                }

                Node::ApplyCont(edge) => {
                    let transfer = self
                        .returning(edge, ret)
                        .unwrap_or_else(|| format!("jump {}", self.edge(edge)));
                    return self.line(indent, format!("{transfer}{suffix}"));
                }
                Node::Switch {
                    scrutinee,
                    cases,
                    default,
                } => {
                    let scrutinee = self.atom(scrutinee);
                    self.line(indent, format!("switch {scrutinee}"))?;
                    for (value, edge) in cases {
                        let arm = self.arm(edge, ret);
                        self.line(indent, format!("| {value} => {arm}"))?;
                    }
                    if let Some(edge) = default {
                        let arm = self.arm(edge, ret);
                        self.line(indent, format!("| _ => {arm}"))?;
                    }
                    return self.line(indent, format!("end{suffix}"));
                }
                Node::Halt { function, args } => {
                    let halt = format!(
                        "halt {}/{}({}){suffix}",
                        function.namespace(),
                        function.name(),
                        self.operands(args)
                    );
                    return self.line(indent, halt);
                }
                Node::Panic(panic) => return self.line(indent, format!("panic {panic}{suffix}")),
                Node::Unreachable => return self.line(indent, format!("unreachable{suffix}")),
            };
        }
    }

    /// The return this edge is, if it is one. The single statement of what an edge to the sentinel prints as, so the transfer and the switch arm cannot drift apart about it.
    fn returning(&self, edge: &Edge, ret: Option<ContinuationId>) -> Option<String> {
        (ret == Some(edge.target)).then(|| match edge.args.as_slice() {
            [] => "return".into(),
            args => format!("return {}", self.operands(args)),
        })
    }

    /// A switch arm. An arm is always an edge, so it needs no word for the jump — but it still needs one for the return.
    fn arm(&self, edge: &Edge, ret: Option<ContinuationId>) -> String {
        self.returning(edge, ret).unwrap_or_else(|| self.edge(edge))
    }

    /// A call and where its result goes. A call returning to its own function's sentinel is a tail call, and says so.
    fn transfer(
        &self,
        call: String,
        return_to: ContinuationId,
        ret: Option<ContinuationId>,
        suffix: &str,
    ) -> String {
        if ret == Some(return_to) {
            format!("return {call}{suffix}")
        } else {
            format!("{call} -> {}{suffix}", self.continuation(return_to))
        }
    }

    // === Spellings ===========================================================

    /// A binder. A hintless value nothing reads is spelled `_`, which is what it is; a hinted one keeps its hint however dead it is.
    fn binder(&self, id: ValueId) -> String {
        match self.module.value(id) {
            Some(definition) => match &definition.debug_name {
                Some(name) => format!("{id}${name}"),
                None if self.uses.get(&id).copied().unwrap_or(0) == 0 => "_".into(),
                None => format!("{id}"),
            },
            None => format!("{id}"),
        }
    }

    fn value(&self, id: ValueId) -> String {
        match self.module.value(id) {
            Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
            None => format!("{id}"),
        }
    }

    fn function(&self, id: FunctionId) -> String {
        match self.module.function(id) {
            Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
            None => format!("{id}"),
        }
    }

    fn continuation(&self, id: ContinuationId) -> String {
        match self.module.continuation(id) {
            Some(definition) => format!("{id}{}", hint(&definition.debug_name)),
            None => format!("{id}"),
        }
    }

    /// A parameter list, bracketing each run of parameters the fields record says was one aggregate. The record is a fact of the program that [`Module::verify`] holds the list to, so it is stated where the list is. Only a continuation can carry one, so a function's list passes no owner rather than passing an identity the map could answer for by coincidence.
    fn params(&self, owner: Option<ContinuationId>, params: &[ValueId]) -> String {
        let groups: &[FieldGroup] = owner
            .and_then(|owner| self.module.field_groups().get(&owner))
            .map_or(&[], Vec::as_slice);
        let mut rendered = String::from("(");
        let mut index = 0;

        while index < params.len() {
            if index != 0 {
                rendered.push_str(", ");
            }
            match groups.iter().find(|group| group.start == index) {
                Some(group) => {
                    let end = (index + group.width).min(params.len());
                    let members = params[index..end]
                        .iter()
                        .map(|&param| self.binder(param))
                        .collect::<Vec<_>>()
                        .join(", ");
                    rendered.push_str(&format!("[{members}]"));
                    index = end;
                }
                None => {
                    rendered.push_str(&self.binder(params[index]));
                    index += 1;
                }
            }
        }

        rendered.push(')');
        rendered
    }

    fn callee(&self, callee: &Callee) -> String {
        // The sigil already says whether the call is direct, so neither side needs a word for it.
        match callee {
            Callee::Known(function) => self.function(*function),
            Callee::Closure(value) => self.value(*value),
        }
    }

    fn edge(&self, edge: &Edge) -> String {
        format!(
            "{}({})",
            self.continuation(edge.target),
            self.operands(&edge.args)
        )
    }

    fn atom(&self, atom: &Atom) -> String {
        match atom {
            Atom::Value(value) => self.value(*value),
            Atom::Fun(function) => self.function(*function),
            Atom::Literal(literal) => render_literal(literal),
            Atom::Filler => "pad".into(),
        }
    }

    fn operands(&self, atoms: &[Atom]) -> String {
        atoms
            .iter()
            .map(|atom| self.atom(atom))
            .collect::<Vec<_>>()
            .join(", ")
    }

    fn value_expr(&self, value: &ValueExpr) -> String {
        match value {
            ValueExpr::Literal(literal) => render_literal(literal),
            ValueExpr::List(atoms) => format!("[{}]", self.operands(atoms)),
            ValueExpr::Tuple(atoms) => format!("({})", self.operands(atoms)),
            ValueExpr::Row(row, atoms) => {
                format!("{}({})", self.row(*row), self.operands(atoms))
            }
        }
    }

    fn row(&self, id: RowId) -> String {
        match self.module.defined_row(id) {
            Some(row) => format!("{id}{}", hint(&row.debug_name)),
            None => format!("{id}"),
        }
    }

    /// An intrinsic applied to its operands. The two reads are projections rather than calls, because that is what they are and a projection is where a reader looks for one; the row qualifies its slot by bare identity, as the erased rung's qualified field does, since a path hint there would run into the index.
    fn intrinsic(&self, op: &Intrinsic, args: &[Atom]) -> String {
        match (op, args) {
            (Intrinsic::RowGet(row, index), [operand]) => {
                format!("{}.{row}/{index}", self.atom(operand))
            }
            (Intrinsic::TupleGet(index), [operand]) => {
                format!("{}.{index}", self.atom(operand))
            }
            _ => format!("{}({})", intrinsic_name(op), self.operands(args)),
        }
    }
}

// === Names ===================================================================

fn hint(name: &Option<String>) -> String {
    match name {
        Some(name) => format!("${name}"),
        None => String::new(),
    }
}

/// What one slot of a row holds. A row-typed slot names the row by bare identity, which is why the header's reachability has to close under it.
fn slot_name(slot: Slot) -> String {
    match slot {
        Slot::Tag => "tag".into(),
        Slot::Nat => "nat".into(),
        Slot::Flt => "flt".into(),
        Slot::List => "list".into(),
        Slot::Closure(arity) => format!("closure/{arity}"),
        Slot::Row(row) => format!("{row}"),
        Slot::Opaque => "opaque".into(),
    }
}

fn cell_name(op: CellOp) -> &'static str {
    match op {
        CellOp::Reserve => "Cell/reserve",
        CellOp::Fill => "Cell/fill",
        CellOp::Poll => "Cell/poll",
    }
}

fn intrinsic_call_name(op: IntrinsicCall) -> &'static str {
    match op {
        IntrinsicCall::ListMap => "List/map",
    }
}

fn grain_carrier(grain: Grain) -> &'static str {
    match grain {
        Grain::B => "Bits",
        Grain::X => "Bytes",
    }
}

/// The name of every intrinsic, carrier first and operation last.
///
/// The `/sys` path of a float operation rounded in `rounding`: `Flt/add` in the default direction, `Flt/toward_zero/add` in another.
fn flt_rounded(rounding: Rounding, operation: &str) -> String {
    match rounding {
        Rounding::TiesToEven => format!("Flt/{operation}"),
        rounding => format!("Flt/{}/{operation}", rounding.label()),
    }
}

/// Spelled out rather than derived from the Rust variant for the reason the erased rung spells its operations rather than printing them infix: below Core there are no types left to recover a carrier from, so the carrier lives in the name. The match is exhaustive with no wildcard arm, which is what makes a new intrinsic a compile error here rather than a leaked variant name in a dump. The variadic widths do not enter the name — the operand list already says how many there are.
fn intrinsic_name(op: &Intrinsic) -> String {
    let name = match op {
        Intrinsic::NatEql => "Nat/eql",
        Intrinsic::NatNeq => "Nat/neq",
        Intrinsic::NatAdd => "Nat/add",
        Intrinsic::NatSub => "Nat/sub",
        Intrinsic::NatMul => "Nat/mul",
        Intrinsic::NatLt => "Nat/lt",
        Intrinsic::NatDiv => "Nat/div",
        Intrinsic::NatRem => "Nat/rem",
        Intrinsic::NatLe => "Nat/le",
        Intrinsic::NatAnd => "Nat/and",
        Intrinsic::NatOr => "Nat/or",
        Intrinsic::NatXor => "Nat/xor",
        Intrinsic::NatMin => "Nat/min",
        Intrinsic::NatMax => "Nat/max",
        Intrinsic::NatShl => "Nat/shl",
        Intrinsic::NatPow => "Nat/pow",
        Intrinsic::NatShr => "Nat/shr",
        Intrinsic::NatEqz => "Nat/eqz",
        Intrinsic::NatToInt => "Nat/to_int",
        Intrinsic::IntAbs => "Int/abs",
        Intrinsic::IntSign => "Int/sign",
        Intrinsic::NatToFlt(Rounding::TiesToEven) => "Nat/to_flt",
        Intrinsic::NatToFlt(rounding) => return flt_rounded(*rounding, "of_nat"),
        Intrinsic::IntEql => "Int/eql",
        Intrinsic::IntNeq => "Int/neq",
        Intrinsic::IntAdd => "Int/add",
        Intrinsic::IntSub => "Int/sub",
        Intrinsic::IntMul => "Int/mul",
        Intrinsic::IntDiv => "Int/div",
        Intrinsic::IntRem => "Int/rem",
        Intrinsic::IntLt => "Int/lt",
        Intrinsic::IntLe => "Int/le",
        Intrinsic::IntAnd => "Int/and",
        Intrinsic::IntOr => "Int/or",
        Intrinsic::IntXor => "Int/xor",
        Intrinsic::IntMin => "Int/min",
        Intrinsic::IntMax => "Int/max",
        Intrinsic::IntShl => "Int/shl",
        Intrinsic::IntShr => "Int/shr",
        Intrinsic::IntEqz => "Int/eqz",
        Intrinsic::IntToNat => "Int/to_nat",
        Intrinsic::IntToFlt(Rounding::TiesToEven) => "Int/to_flt",
        Intrinsic::IntToFlt(rounding) => return flt_rounded(*rounding, "of_int"),
        Intrinsic::FltAdd(rounding) => return flt_rounded(*rounding, "add"),
        Intrinsic::FltSub(rounding) => return flt_rounded(*rounding, "sub"),
        Intrinsic::FltMul(rounding) => return flt_rounded(*rounding, "mul"),
        Intrinsic::FltDiv(rounding) => return flt_rounded(*rounding, "div"),
        Intrinsic::FltFma(rounding) => return flt_rounded(*rounding, "fma"),
        Intrinsic::FltRem => "Flt/rem",
        Intrinsic::FltEql => "Flt/eql",
        Intrinsic::FltNeq => "Flt/neq",
        Intrinsic::FltLt => "Flt/lt",
        Intrinsic::FltLe => "Flt/le",
        Intrinsic::FltMin => "Flt/min",
        Intrinsic::FltMax => "Flt/max",
        Intrinsic::FltNeg => "Flt/neg",
        Intrinsic::FltAbs => "Flt/abs",
        Intrinsic::FltSqrt(rounding) => return flt_rounded(*rounding, "sqrt"),
        Intrinsic::FltRoundIntegral(rounding) => {
            return format!("Flt/{}", rounding.integral_label());
        }
        Intrinsic::FltCopysign => "Flt/copysign",
        Intrinsic::FltToNat => "Flt/to_nat",
        Intrinsic::FltToLeBytes => "Flt/to_le_bytes",
        Intrinsic::FltOfLeBytes => "Flt/of_le_bytes",
        Intrinsic::FltToInt => "Flt/to_int",
        Intrinsic::FltMantissa => "Flt/mantissa",
        Intrinsic::FltExponent => "Flt/exponent",
        Intrinsic::ListLen => "List/len",
        Intrinsic::ListGet => "List/get",
        Intrinsic::ListSlice => "List/slice",
        Intrinsic::ListRest => "List/rest",
        Intrinsic::ListAppend => "List/append",
        Intrinsic::ListConcat(_) => "List/concat",
        Intrinsic::ListSettle => "List/settle",
        Intrinsic::ListFlat(_) => "List/flat",
        Intrinsic::WindowExtent => "Window/extent",
        Intrinsic::IsImmediate => "Immediate/is",
        Intrinsic::ImmediateGet => "Immediate/get",
        Intrinsic::BinLen(grain) => return format!("{}/len", grain_carrier(*grain)),
        Intrinsic::BinEql(grain) => return format!("{}/eql", grain_carrier(*grain)),
        Intrinsic::BinGet(grain) => return format!("{}/get", grain_carrier(*grain)),
        Intrinsic::BinSlice(grain) => return format!("{}/slice", grain_carrier(*grain)),
        Intrinsic::BinRest(grain) => return format!("{}/rest", grain_carrier(*grain)),
        Intrinsic::BinAppend(grain) => return format!("{}/append", grain_carrier(*grain)),
        Intrinsic::BinConcat(grain, _) => return format!("{}/concat", grain_carrier(*grain)),
        Intrinsic::BinChunk(grain, _) => return format!("{}/chunk", grain_carrier(*grain)),
        Intrinsic::BinReplicate(grain) => return format!("{}/replicate", grain_carrier(*grain)),
        Intrinsic::BinReinterp(grain) => {
            return match grain {
                Grain::X => "Bytes/to_bits".to_string(),
                Grain::B => "Bits/to_bytes".to_string(),
            };
        }
        Intrinsic::BinAnd(grain) => return format!("{}/and", grain_carrier(*grain)),
        Intrinsic::BinOr(grain) => return format!("{}/or", grain_carrier(*grain)),
        Intrinsic::BinXor(grain) => return format!("{}/xor", grain_carrier(*grain)),
        // Reached only through `Printer::intrinsic`'s projection arms with the wrong operand count, which a verified module does not produce.
        Intrinsic::RowGet(row, index) => return format!("Row/get({row}, {index})"),
        Intrinsic::TupleGet(index) => return format!("Tuple/get({index})"),
    };
    name.into()
}

/// A literal, carrier and all — the same spellings the erased rung prints, so a constant reads the same on both sides of the lowering.
fn render_literal(literal: &Literal) -> String {
    match literal {
        Literal::Nat(value) => format!("{value}"),
        Literal::Int(value) => format!("{value}:int"),
        Literal::Flt(value) => {
            let float = f64::from(*value);
            if float.is_finite() {
                format!("{float:?}:flt")
            } else {
                format!("flt:0x{:016x}", float.to_bits())
            }
        }
        Literal::Bin(grain, bits) => {
            let rendered = bits
                .to_packed_bytes()
                .iter()
                .map(|byte| format!("{byte:02x}"))
                .collect::<String>();
            match grain {
                Grain::B => format!("b\"{rendered}\"/{}", bits.bit_length()),
                Grain::X => format!("x\"{rendered}\""),
            }
        }
    }
}

fn channel_name(operation: ChannelOp) -> &'static str {
    match operation {
        ChannelOp::New => "Channel/new",
        ChannelOp::Push => "Channel/push",
        ChannelOp::Take => "Channel/take",
        ChannelOp::Close => "Channel/close",
        ChannelOp::Closed => "Channel/closed",
        ChannelOp::Count => "Channel/count",
        ChannelOp::Capacity => "Channel/capacity",
    }
}
