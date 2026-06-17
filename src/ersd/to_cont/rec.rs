use {
    crate::{cont, ersd},
    std::collections::BTreeSet,
};

pub fn unsupported_sync_rec_item(term: &ersd::Term) -> ! {
    panic!(
        "`to_cont` does not support a call-valued `rec` item in value position: \
         the following term reaches `Apply`/`Match`/`NatMatch` on its construction path \
         but is bound where a synchronous value is required: {term:?}",
    )
}

/// Free names of an `ersd::Term`, treating a nested `Func` as contributing its precomputed
/// `captures` (the closure's free variables) rather than descending into its body. Used to
/// build the dependency graph that feeds [`rec_computed_order`]; lives here because the `rec`
/// lowerer is its only consumer.
pub fn free_names(term: &ersd::Term) -> BTreeSet<String> {
    let mut names = BTreeSet::new();

    match &**term {
        ersd::Subterm::Erased | ersd::Subterm::Unreachable | ersd::Subterm::Atom(_) => {}
        ersd::Subterm::Name(name) => {
            names.insert(name.as_str().to_owned());
        }
        ersd::Subterm::Func(func) => names.extend(func.captures.iter().map(|c| c.name.clone())),
        ersd::Subterm::Apply(apply) => {
            names.extend(free_names(&apply.head));
            apply
                .params
                .iter()
                .for_each(|param| names.extend(free_names(param)));
        }
        ersd::Subterm::Tuple(tuple) => tuple
            .fields
            .iter()
            .for_each(|field| names.extend(free_names(field))),
        ersd::Subterm::Proj(proj) => names.extend(free_names(&proj.head)),
        ersd::Subterm::Match(m) => {
            names.extend(free_names(&m.head));
            m.cases
                .iter()
                .for_each(|case| names.extend(free_names(case)));
        }
        ersd::Subterm::NatMatch(ersd::NatMatch::Induction {
            head,
            zero_case,
            pred,
            ih,
            succ_case,
        }) => {
            names.extend(free_names(head));
            names.extend(free_names(zero_case));

            let mut succ = free_names(succ_case);
            succ.remove(pred);
            succ.remove(ih);
            names.extend(succ);
        }
        ersd::Subterm::NatMatch(ersd::NatMatch::Dispatch {
            head,
            cases,
            default,
        }) => {
            names.extend(free_names(head));
            cases
                .iter()
                .for_each(|(_, case)| names.extend(free_names(case)));
            names.extend(free_names(default));
        }
        ersd::Subterm::Let(let_) => {
            names.extend(free_names(&let_.body));

            let mut tail = free_names(&let_.tail);
            tail.remove(&let_.name);
            names.extend(tail);
        }
        ersd::Subterm::Rec(rec) => {
            let mut inner = BTreeSet::new();
            rec.items
                .iter()
                .for_each(|item| inner.extend(free_names(item)));
            inner.extend(free_names(&rec.tail));
            rec.names.iter().for_each(|name| {
                inner.remove(name);
            });
            names.extend(inner);
        }
        ersd::Subterm::Prim(prim) => names.extend(free_names_prim(prim)),
    }

    names
}

fn free_names_prim(prim: &ersd::Prim) -> BTreeSet<String> {
    match prim {
        ersd::Prim::Pure(p) => free_names_pure_prim(p),
        ersd::Prim::Host(h) => free_names_host_prim(h),
    }
}

fn free_names_host_prim(prim: &ersd::HostPrim) -> BTreeSet<String> {
    let operands: Vec<&ersd::Term> = match prim {
        ersd::HostPrim::IoClockWall | ersd::HostPrim::IoClockMono | ersd::HostPrim::IoArgs => {
            vec![]
        }
        ersd::HostPrim::IoAccept(a)
        | ersd::HostPrim::IoSocket(a)
        | ersd::HostPrim::IoClose(a)
        | ersd::HostPrim::IoRandom(a)
        | ersd::HostPrim::IoEnv(a)
        | ersd::HostPrim::IoExit(a) => vec![a],
        ersd::HostPrim::IoRead(a, b)
        | ersd::HostPrim::IoWrite(a, b)
        | ersd::HostPrim::IoOpen(a, b)
        | ersd::HostPrim::IoResolve(a, b)
        | ersd::HostPrim::IoBind(a, b)
        | ersd::HostPrim::IoConnect(a, b)
        | ersd::HostPrim::IoStartTls(a, b)
        | ersd::HostPrim::IoTlsServerConfig(a, b)
        | ersd::HostPrim::IoStartTlsServer(a, b)
        | ersd::HostPrim::IoListen(a, b)
        | ersd::HostPrim::IoSetNonblocking(a, b)
        | ersd::HostPrim::IoSetRecvTimeout(a, b)
        | ersd::HostPrim::IoSetSendTimeout(a, b)
        | ersd::HostPrim::IoSetReuseaddr(a, b) => vec![a, b],
        ersd::HostPrim::IoPoll(a, b, c) => vec![a, b, c],
    };

    operands.into_iter().flat_map(free_names).collect()
}

fn free_names_pure_prim(prim: &ersd::PurePrim) -> BTreeSet<String> {
    use ersd::PurePrim::*;

    let operands: Vec<&ersd::Term> = match prim {
        Nat(_) | Int(_) | Flt(_) | Bin(_) | Io(_) | Unit => vec![],
        NatToStr(a) | IntToStr(a) | FltToStr(a) | NatToInt(a) | NatToFlt(a) | IntToNat(a)
        | IntToFlt(a) | FltToNat(a) | FltToLeBin(a) | FltToInt(a) | FltNeg(a) | FltAbs(a)
        | FltSqrt(a) | FltFloor(a) | FltCeil(a) | FltTrunc(a) | FltNearest(a) | BinLen(a)
        | ArrLen(a) => {
            vec![a]
        }
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
        | FltAdd(a, b)
        | FltSub(a, b)
        | FltMul(a, b)
        | FltDiv(a, b)
        | FltEql(a, b)
        | FltNeq(a, b)
        | FltLt(a, b)
        | FltGt(a, b)
        | FltLte(a, b)
        | FltGte(a, b)
        | FltMin(a, b)
        | FltMax(a, b)
        | BinEql(a, b)
        | BinGet(a, b)
        | BinAppend(a, b)
        | ArrGet(a, b)
        | ArrAppend(a, b) => vec![a, b],
        BinSlice(a, b, c) | ArrSlice(a, b, c) => vec![a, b, c],
        BinConcat(operands) | ArrConcat(operands) | Arr(operands) => operands.iter().collect(),
    };

    operands.into_iter().flat_map(free_names).collect()
}

/// Post-order (dependencies first) of the call/match-valued `rec` bindings, panicking
/// with the offending cycle if two such bindings depend on each other's value — that
/// case needs runtime fixpoint cells, which are out of scope.
pub fn rec_computed_order(names: &[&str], deps: &[Vec<usize>]) -> Vec<usize> {
    fn visit(
        node: usize,
        names: &[&str],
        deps: &[Vec<usize>],
        marks: &mut [u8],
        stack: &mut Vec<usize>,
        order: &mut Vec<usize>,
    ) {
        marks[node] = 1;
        stack.push(node);

        for &next in &deps[node] {
            match marks[next] {
                1 => {
                    let start = stack.iter().position(|&n| n == next).unwrap();
                    let cycle = stack[start..]
                        .iter()
                        .chain([&next])
                        .map(|&n| names[n])
                        .collect::<Vec<_>>()
                        .join(" -> ");

                    panic!(
                        "`to_cont` does not support value-level mutual recursion: \
                         {cycle} would require runtime fixpoint cells; only closures may be \
                         mutually recursive (a cyclic tuple/array reaches this path too)",
                    );
                }
                0 => visit(next, names, deps, marks, stack, order),
                _ => {}
            }
        }

        stack.pop();
        marks[node] = 2;
        order.push(node);
    }

    let mut marks = vec![0u8; names.len()];
    let mut stack = vec![];
    let mut order = vec![];

    for node in 0..names.len() {
        if marks[node] == 0 {
            visit(node, names, deps, &mut marks, &mut stack, &mut order);
        }
    }

    order
}

/// A `rec`-bound closure shell and its captures. The `Func` is lowered eagerly so its
/// `clsr` name is shared by both the prealloc declaration (the shell) and the later fill.
/// Only closures need a shell: tuples and arrays are *not* prealloc'd — they lower as
/// computed items, so a cyclic one is rejected by [`rec_computed_order`] rather than
/// back-patched (see `plan_backpatch`). Confining cyclic recursion to closures is what lets
/// every `tpl`/`arr` wasm field stay immutable.
pub struct Backpatch {
    pub clsr: cont::ClsrName,
    pub captures: Vec<cont::ValueName>,
}
