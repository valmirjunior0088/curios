use {
    super::{Cont, Frame, Work},
    crate::{cont, ersd},
    num_bigint::BigUint,
};

fn lower_pure_unary_code(
    work: &mut Work,
    operand: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let operand = work.lower_pure_name(operand, frame);
    work.fresh(cont::Value::Eval(code(operand)))
}

fn lower_pure_binary_code(
    work: &mut Work,
    left: &ersd::Term,
    right: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let left = work.lower_pure_name(left, frame);
    let right = work.lower_pure_name(right, frame);
    work.fresh(cont::Value::Eval(code(left, right)))
}

fn lower_pure_ternary_code(
    work: &mut Work,
    first: &ersd::Term,
    second: &ersd::Term,
    third: &ersd::Term,
    frame: &Frame,
    code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code,
) -> cont::ValueName {
    let first = work.lower_pure_name(first, frame);
    let second = work.lower_pure_name(second, frame);
    let third = work.lower_pure_name(third, frame);
    work.fresh(cont::Value::Eval(code(first, second, third)))
}

fn lower_pure_names(work: &mut Work, terms: &[ersd::Term], frame: &Frame) -> Vec<cont::ValueName> {
    terms
        .iter()
        .map(|term| work.lower_pure_name(term, frame))
        .collect()
}

/// A resume block that packs a host op's `arity` results into an `arity`-field
/// record and feeds it to `cont`; returns the block name to plug into the
/// `HostTarget`. Arity 0 yields unit, 2/3 the status records.
fn record_resume<'b>(work: &mut Work, arity: usize, cont: Cont<'b>) -> cont::BlockName {
    let resume = work.fresh_block();
    let fields = (0..arity).map(|_| work.fresh_value()).collect::<Vec<_>>();

    work.add_resume_block(resume.clone(), fields.clone(), move |inner| {
        let record = inner.fresh(cont::Value::Pure(cont::Data::Tpl(fields)));

        cont.call(inner, record)
    });

    resume
}

/// A resume block that forwards a host op's single result straight to `cont`.
fn forward_resume<'b>(work: &mut Work, cont: Cont<'b>) -> cont::BlockName {
    let resume = work.fresh_block();
    let value = work.fresh_value();
    let value_clone = value.clone();

    work.add_resume_block(resume.clone(), vec![value], move |inner| {
        cont.call(inner, value_clone)
    });

    resume
}

fn lower_unary_code<'b>(
    work: &mut Work,
    operand: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
    work.lower_value_name(
        operand,
        frame,
        Cont::new(move |work, operand| {
            let value = work.fresh(cont::Value::Eval(code(operand)));

            cont.call(work, value)
        }),
    )
}

fn lower_binary_code<'b>(
    work: &mut Work,
    left: &'b ersd::Term,
    right: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName, cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
    work.lower_value_name(
        left,
        frame,
        Cont::new(move |work, left| {
            work.lower_value_name(
                right,
                frame,
                Cont::new(move |work, right| {
                    let value = work.fresh(cont::Value::Eval(code(left, right)));

                    cont.call(work, value)
                }),
            )
        }),
    )
}

fn lower_ternary_code<'b>(
    work: &mut Work,
    first: &'b ersd::Term,
    second: &'b ersd::Term,
    third: &'b ersd::Term,
    frame: &'b Frame,
    cont: Cont<'b>,
    code: impl FnOnce(cont::ValueName, cont::ValueName, cont::ValueName) -> cont::Code + 'b,
) -> cont::Tail {
    work.lower_value_name(
        first,
        frame,
        Cont::new(move |work, first| {
            work.lower_value_name(
                second,
                frame,
                Cont::new(move |work, second| {
                    work.lower_value_name(
                        third,
                        frame,
                        Cont::new(move |work, third| {
                            let value = work.fresh(cont::Value::Eval(code(first, second, third)));

                            cont.call(work, value)
                        }),
                    )
                }),
            )
        }),
    )
}

fn lower_lst<'b>(
    work: &mut Work,
    elements: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match elements {
        [] => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Arr(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_lst(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_bin_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_bin_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

fn lower_arr_concat<'b>(
    work: &mut Work,
    operands: &'b [ersd::Term],
    frame: &'b Frame,
    mut names: Vec<cont::ValueName>,
    cont: Cont<'b>,
) -> cont::Tail {
    match operands {
        [] => {
            let value = work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)));

            cont.call(work, value)
        }
        [head, tail @ ..] => work.lower_value_name(
            head,
            frame,
            Cont::new(move |work, name| {
                names.push(name);
                lower_arr_concat(work, tail, frame, names, cont)
            }),
        ),
    }
}

pub fn lower_pure_prim(work: &mut Work, prim: &ersd::PurePrim, frame: &Frame) -> cont::ValueName {
    match prim {
        ersd::PurePrim::Nat(value) => work.fresh(cont::Value::Pure(cont::Data::Nat(*value))),
        ersd::PurePrim::NatEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatEql)
        }
        ersd::PurePrim::NatNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatNeq)
        }
        ersd::PurePrim::NatAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatAdd)
        }
        ersd::PurePrim::NatSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatSub)
        }
        ersd::PurePrim::NatMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatMul)
        }
        ersd::PurePrim::NatLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLt)
        }
        ersd::PurePrim::NatDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatDiv)
        }
        ersd::PurePrim::NatRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatRem)
        }
        ersd::PurePrim::NatGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGt)
        }
        ersd::PurePrim::NatLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatLte)
        }
        ersd::PurePrim::NatGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatGte)
        }
        ersd::PurePrim::NatAnd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatAnd)
        }
        ersd::PurePrim::NatOr(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatOr)
        }
        ersd::PurePrim::NatXor(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatXor)
        }
        ersd::PurePrim::NatShl(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatShl)
        }
        ersd::PurePrim::NatShr(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::NatShr)
        }
        ersd::PurePrim::Int(value) => work.fresh(cont::Value::Pure(cont::Data::Int(*value))),
        ersd::PurePrim::Flt(value) => work.fresh(cont::Value::Pure(cont::Data::Flt(*value))),
        ersd::PurePrim::IntEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntEql)
        }
        ersd::PurePrim::IntNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntNeq)
        }
        ersd::PurePrim::IntAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntAdd)
        }
        ersd::PurePrim::IntSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntSub)
        }
        ersd::PurePrim::IntMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntMul)
        }
        ersd::PurePrim::IntDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntDiv)
        }
        ersd::PurePrim::IntRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntRem)
        }
        ersd::PurePrim::IntLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLt)
        }
        ersd::PurePrim::IntGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGt)
        }
        ersd::PurePrim::IntLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntLte)
        }
        ersd::PurePrim::IntGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntGte)
        }
        ersd::PurePrim::IntAnd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntAnd)
        }
        ersd::PurePrim::IntOr(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntOr)
        }
        ersd::PurePrim::IntXor(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntXor)
        }
        ersd::PurePrim::IntShl(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntShl)
        }
        ersd::PurePrim::IntShr(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::IntShr)
        }
        ersd::PurePrim::FltAdd(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltAdd)
        }
        ersd::PurePrim::FltSub(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltSub)
        }
        ersd::PurePrim::FltMul(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMul)
        }
        ersd::PurePrim::FltDiv(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltDiv)
        }
        ersd::PurePrim::FltRem(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltRem)
        }
        ersd::PurePrim::FltEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltEql)
        }
        ersd::PurePrim::FltNeq(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltNeq)
        }
        ersd::PurePrim::FltLt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLt)
        }
        ersd::PurePrim::FltGt(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGt)
        }
        ersd::PurePrim::FltLte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltLte)
        }
        ersd::PurePrim::FltGte(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltGte)
        }
        ersd::PurePrim::FltMin(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMin)
        }
        ersd::PurePrim::FltMax(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::FltMax)
        }
        ersd::PurePrim::FltNeg(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNeg)
        }
        ersd::PurePrim::FltAbs(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltAbs)
        }
        ersd::PurePrim::FltSqrt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltSqrt)
        }
        ersd::PurePrim::FltFloor(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltFloor)
        }
        ersd::PurePrim::FltCeil(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltCeil)
        }
        ersd::PurePrim::FltTrunc(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltTrunc)
        }
        ersd::PurePrim::FltNearest(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltNearest)
        }
        ersd::PurePrim::FltToLeBin(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToLeBin)
        }
        ersd::PurePrim::NatToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToInt)
        }
        ersd::PurePrim::NatToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::NatToFlt)
        }
        ersd::PurePrim::IntToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToNat)
        }
        ersd::PurePrim::IntToFlt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::IntToFlt)
        }
        ersd::PurePrim::FltToNat(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToNat)
        }
        ersd::PurePrim::FltToInt(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::FltToInt)
        }
        ersd::PurePrim::Bin(bytes) => work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone()))),
        ersd::PurePrim::BinLen(bin) => lower_pure_unary_code(work, bin, frame, cont::Code::BinLen),
        ersd::PurePrim::BinEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::BinEql)
        }
        // Handle identity is byte identity: the rep is bytes from here down.
        ersd::PurePrim::IoEql(left, right) => {
            lower_pure_binary_code(work, left, right, frame, cont::Code::BinEql)
        }
        ersd::PurePrim::BinGet(bin, idx) => {
            lower_pure_binary_code(work, bin, idx, frame, cont::Code::BinGet)
        }
        ersd::PurePrim::BinSlice(bin, start, end) => {
            lower_pure_ternary_code(work, bin, start, end, frame, cont::Code::BinSlice)
        }
        ersd::PurePrim::BinAppend(bin, byte) => {
            lower_pure_binary_code(work, bin, byte, frame, cont::Code::BinAppend)
        }
        ersd::PurePrim::BinConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::BinConcat(names)))
        }
        ersd::PurePrim::BinFlatten(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::BinFlatten)
        }
        ersd::PurePrim::Arr(elements) => {
            let names = lower_pure_names(work, elements, frame);

            work.fresh(cont::Value::Pure(cont::Data::Arr(names)))
        }
        ersd::PurePrim::ArrLen(lst) => lower_pure_unary_code(work, lst, frame, cont::Code::ArrLen),
        ersd::PurePrim::ArrGet(lst, idx) => {
            lower_pure_binary_code(work, lst, idx, frame, cont::Code::ArrGet)
        }
        ersd::PurePrim::ArrSlice(lst, start, end) => {
            lower_pure_ternary_code(work, lst, start, end, frame, cont::Code::ArrSlice)
        }
        ersd::PurePrim::ArrAppend(lst, elem) => {
            lower_pure_binary_code(work, lst, elem, frame, cont::Code::ArrAppend)
        }
        ersd::PurePrim::ArrConcat(operands) => {
            let names = lower_pure_names(work, operands, frame);

            work.fresh(cont::Value::Eval(cont::Code::ArrConcat(names)))
        }
        ersd::PurePrim::ArrFlatten(operand) => {
            lower_pure_unary_code(work, operand, frame, cont::Code::ArrFlatten)
        }
        ersd::PurePrim::ArrMap(src, f) => {
            lower_pure_binary_code(work, src, f, frame, cont::Code::ArrMap)
        }
        // A handle erases to its host token bytes: the LE encoding of the token
        // integer, the same `BigUint::to_bytes_le` the runtime mints and keys on.
        // This is the lone spot in the pipeline that knows a handle is bytes.
        ersd::PurePrim::Io(token) => work.fresh(cont::Value::Pure(cont::Data::Bin(
            BigUint::from(*token).to_bytes_le(),
        ))),
    }
}

pub fn lower_value_prim<'b>(
    work: &mut Work,
    prim: &'b ersd::Prim,
    frame: &'b Frame,
    cont: Cont<'b>,
) -> cont::Tail {
    match prim {
        ersd::Prim::Pure(pure_prim) => lower_value_pure_prim(work, pure_prim, frame, cont),
        ersd::Prim::Host(ersd::HostPrim::IoRead(handle, count)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    count,
                    frame,
                    Cont::new(move |work, count| {
                        // The host returns (status, bytes), packed into the
                        // `{ status, bytes }` record the prim's type promises.
                        let resume = record_resume(work, 2, cont);

                        cont::Tail::Host(cont::HostTarget::IoRead {
                            handle,
                            count,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoWrite(handle, bytes)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    bytes,
                    frame,
                    Cont::new(move |work, bytes| {
                        // `Io.write` returns `(status, written)`, packed into a
                        // `{ status, written }` record like `IoRead`/`IoOpen`.
                        let resume = record_resume(work, 2, cont);

                        cont::Tail::Host(cont::HostTarget::IoWrite {
                            handle,
                            bytes,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoOpen(path, mode)) => work.lower_value_name(
            path,
            frame,
            Cont::new(move |work, path| {
                work.lower_value_name(
                    mode,
                    frame,
                    Cont::new(move |work, mode| {
                        // (status, handle) packs into `{ status, handle }`.
                        let resume = record_resume(work, 2, cont);

                        cont::Tail::Host(cont::HostTarget::IoOpen { path, mode, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoLookup(host, port)) => work.lower_value_name(
            host,
            frame,
            Cont::new(move |work, host| {
                work.lower_value_name(
                    port,
                    frame,
                    Cont::new(move |work, port| {
                        // (status, handle) packs into `{ status, handle }`, like
                        // `IoSocket`: the lookup hands back a poll-readable handle.
                        let resume = record_resume(work, 2, cont);

                        cont::Tail::Host(cont::HostTarget::IoLookup { host, port, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoResolve(handle)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                // (status, addresses) packs into `{ status, addresses }`, the
                // second field an `Arr(Bin)` (still one anyref slot).
                let resume = record_resume(work, 2, cont);

                cont::Tail::Host(cont::HostTarget::IoResolve { handle, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoSocket(addr)) => work.lower_value_name(
            addr,
            frame,
            Cont::new(move |work, addr| {
                // (status, handle) packs into `{ status, handle }`, like `IoOpen`.
                let resume = record_resume(work, 2, cont);

                cont::Tail::Host(cont::HostTarget::IoSocket { addr, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoBind(handle, addr)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    addr,
                    frame,
                    Cont::new(move |work, addr| {
                        // `Io.bind` returns its status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoBind {
                            handle,
                            addr,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoConnect(handle, addr)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    addr,
                    frame,
                    Cont::new(move |work, addr| {
                        // `Io.connect` returns its status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoConnect {
                            handle,
                            addr,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoListen(handle, backlog)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    backlog,
                    frame,
                    Cont::new(move |work, backlog| {
                        // `Io.listen` returns its status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoListen {
                            handle,
                            backlog,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoAccept(handle)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                // (status, handle) packs into `{ status, handle }`: one operand
                // in, a two-field status record out.
                let resume = record_resume(work, 2, cont);

                cont::Tail::Host(cont::HostTarget::IoAccept { handle, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoStartTls(handle, sni)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    sni,
                    frame,
                    Cont::new(move |work, sni| {
                        // `Io.start_tls` returns its status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoStartTls {
                            handle,
                            sni,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoTlsServerConfig(cert, key)) => work.lower_value_name(
            cert,
            frame,
            Cont::new(move |work, cert| {
                work.lower_value_name(
                    key,
                    frame,
                    Cont::new(move |work, key| {
                        // (status, handle) packs into `{ status, handle }`, like `IoSocket`.
                        let resume = record_resume(work, 2, cont);

                        cont::Tail::Host(cont::HostTarget::IoTlsServerConfig { cert, key, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoStartTlsServer(handle, cfg)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    cfg,
                    frame,
                    Cont::new(move |work, cfg| {
                        // `Io.start_tls_server` returns its status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoStartTlsServer {
                            handle,
                            cfg,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoSetNonblocking(handle, on)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    on,
                    frame,
                    Cont::new(move |work, on| {
                        // The setters return their status scalar; forward it straight.
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoSetNonblocking { handle, on, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoSetRecvTimeout(handle, ms)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    ms,
                    frame,
                    Cont::new(move |work, ms| {
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoSetRecvTimeout { handle, ms, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoSetSendTimeout(handle, ms)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    ms,
                    frame,
                    Cont::new(move |work, ms| {
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoSetSendTimeout { handle, ms, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoSetReuseaddr(handle, on)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                work.lower_value_name(
                    on,
                    frame,
                    Cont::new(move |work, on| {
                        let resume = forward_resume(work, cont);

                        cont::Tail::Host(cont::HostTarget::IoSetReuseaddr { handle, on, resume })
                    }),
                )
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoPoll(handles, events, timeout)) => work
            .lower_value_name(
                handles,
                frame,
                Cont::new(move |work, handles| {
                    work.lower_value_name(
                        events,
                        frame,
                        Cont::new(move |work, events| {
                            work.lower_value_name(
                                timeout,
                                frame,
                                Cont::new(move |work, timeout| {
                                    // `Io.poll` returns the `Arr(Nat)` of revents
                                    // directly; forward it straight, like `IoArgs`.
                                    let resume = forward_resume(work, cont);

                                    cont::Tail::Host(cont::HostTarget::IoPoll {
                                        handles,
                                        events,
                                        timeout,
                                        resume,
                                    })
                                }),
                            )
                        }),
                    )
                }),
            ),
        ersd::Prim::Host(ersd::HostPrim::IoClose(handle)) => work.lower_value_name(
            handle,
            frame,
            Cont::new(move |work, handle| {
                // After `Io.close` the IR continues with `()`.
                let resume = record_resume(work, 0, cont);

                cont::Tail::Host(cont::HostTarget::IoClose { handle, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoClockWall) => {
            // Ambient: no operands. The host returns (secs_hi, secs_lo, nanos),
            // packed into the `{ secs_hi, secs_lo, nanos }` record.
            let resume = record_resume(work, 3, cont);

            cont::Tail::Host(cont::HostTarget::IoClockWall { resume })
        }
        ersd::Prim::Host(ersd::HostPrim::IoClockMono) => {
            // (secs, nanos) packs into `{ secs, nanos }`, like IoClockWall.
            let resume = record_resume(work, 2, cont);

            cont::Tail::Host(cont::HostTarget::IoClockMono { resume })
        }
        ersd::Prim::Host(ersd::HostPrim::IoRandom(count)) => work.lower_value_name(
            count,
            frame,
            Cont::new(move |work, count| {
                // `Io.random` returns the Bin directly; forward it straight.
                let resume = forward_resume(work, cont);

                cont::Tail::Host(cont::HostTarget::IoRandom { count, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoArgs) => {
            // Ambient: no operands. The host returns the `Arr(Bin)` directly;
            // forward it straight, like `IoRandom`.
            let resume = forward_resume(work, cont);

            cont::Tail::Host(cont::HostTarget::IoArgs { resume })
        }
        ersd::Prim::Host(ersd::HostPrim::IoEnv(name)) => work.lower_value_name(
            name,
            frame,
            Cont::new(move |work, name| {
                // (status, value) packs into `{ status, value }`, like `IoOpen`.
                let resume = record_resume(work, 2, cont);

                cont::Tail::Host(cont::HostTarget::IoEnv { name, resume })
            }),
        ),
        ersd::Prim::Host(ersd::HostPrim::IoExit(code)) => work.lower_value_name(
            code,
            frame,
            Cont::new(move |work, code| {
                // exit never returns — the host traps — so the resume is dead
                // code, kept only for the uniform shape (like `IoClose`).
                let resume = record_resume(work, 0, cont);

                cont::Tail::Host(cont::HostTarget::IoExit { code, resume })
            }),
        ),
        ersd::Prim::Cell(ersd::CellPrim::New(init)) => work.lower_value_name(
            init,
            frame,
            Cont::new(move |work, init| {
                let resume = forward_resume(work, cont);

                cont::Tail::Cell(cont::CellTarget::New { init, resume })
            }),
        ),
        ersd::Prim::Cell(ersd::CellPrim::Set(cell, value)) => work.lower_value_name(
            cell,
            frame,
            Cont::new(move |work, cell| {
                work.lower_value_name(
                    value,
                    frame,
                    Cont::new(move |work, value| {
                        let resume = record_resume(work, 0, cont);

                        cont::Tail::Cell(cont::CellTarget::Set {
                            cell,
                            value,
                            resume,
                        })
                    }),
                )
            }),
        ),
        ersd::Prim::Cell(ersd::CellPrim::Get(cell)) => work.lower_value_name(
            cell,
            frame,
            Cont::new(move |work, cell| {
                let resume = forward_resume(work, cont);

                cont::Tail::Cell(cont::CellTarget::Get { cell, resume })
            }),
        ),
    }
}

fn lower_value_pure_prim<'b>(
    work: &mut Work,
    prim: &'b ersd::PurePrim,
    frame: &'b Frame,
    cont: Cont<'b>,
) -> cont::Tail {
    match prim {
        ersd::PurePrim::Nat(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Nat(*value)));

            cont.call(work, value)
        }
        // A handle erases to its host token bytes (see the pure-position arm).
        ersd::PurePrim::Io(token) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Bin(
                BigUint::from(*token).to_bytes_le(),
            )));

            cont.call(work, value)
        }
        ersd::PurePrim::NatEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatEql)
        }
        ersd::PurePrim::NatAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatAdd)
        }
        ersd::PurePrim::NatSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatSub)
        }
        ersd::PurePrim::NatMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatMul)
        }
        ersd::PurePrim::NatLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLt)
        }
        ersd::PurePrim::NatNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatNeq)
        }
        ersd::PurePrim::NatDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatDiv)
        }
        ersd::PurePrim::NatRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatRem)
        }
        ersd::PurePrim::NatGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGt)
        }
        ersd::PurePrim::NatLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatLte)
        }
        ersd::PurePrim::NatGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatGte)
        }
        ersd::PurePrim::NatAnd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatAnd)
        }
        ersd::PurePrim::NatOr(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatOr)
        }
        ersd::PurePrim::NatXor(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatXor)
        }
        ersd::PurePrim::NatShl(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatShl)
        }
        ersd::PurePrim::NatShr(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::NatShr)
        }
        ersd::PurePrim::Int(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Int(*value)));

            cont.call(work, value)
        }
        ersd::PurePrim::Flt(value) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Flt(*value)));

            cont.call(work, value)
        }
        ersd::PurePrim::IntEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntEql)
        }
        ersd::PurePrim::IntAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntAdd)
        }
        ersd::PurePrim::IntSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntSub)
        }
        ersd::PurePrim::IntMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntMul)
        }
        ersd::PurePrim::IntNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntNeq)
        }
        ersd::PurePrim::IntDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntDiv)
        }
        ersd::PurePrim::IntRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntRem)
        }
        ersd::PurePrim::IntLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLt)
        }
        ersd::PurePrim::IntGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGt)
        }
        ersd::PurePrim::IntLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntLte)
        }
        ersd::PurePrim::IntGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntGte)
        }
        ersd::PurePrim::IntAnd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntAnd)
        }
        ersd::PurePrim::IntOr(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntOr)
        }
        ersd::PurePrim::IntXor(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntXor)
        }
        ersd::PurePrim::IntShl(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntShl)
        }
        ersd::PurePrim::IntShr(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::IntShr)
        }
        ersd::PurePrim::FltAdd(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltAdd)
        }
        ersd::PurePrim::FltSub(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltSub)
        }
        ersd::PurePrim::FltMul(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMul)
        }
        ersd::PurePrim::FltDiv(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltDiv)
        }
        ersd::PurePrim::FltRem(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltRem)
        }
        ersd::PurePrim::FltEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltEql)
        }
        ersd::PurePrim::FltNeq(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltNeq)
        }
        ersd::PurePrim::FltLt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLt)
        }
        ersd::PurePrim::FltGt(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGt)
        }
        ersd::PurePrim::FltLte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltLte)
        }
        ersd::PurePrim::FltGte(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltGte)
        }
        ersd::PurePrim::FltMin(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMin)
        }
        ersd::PurePrim::FltMax(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::FltMax)
        }
        ersd::PurePrim::FltNeg(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNeg)
        }
        ersd::PurePrim::FltAbs(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltAbs)
        }
        ersd::PurePrim::FltSqrt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltSqrt)
        }
        ersd::PurePrim::FltFloor(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltFloor)
        }
        ersd::PurePrim::FltCeil(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltCeil)
        }
        ersd::PurePrim::FltTrunc(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltTrunc)
        }
        ersd::PurePrim::FltNearest(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltNearest)
        }
        ersd::PurePrim::FltToLeBin(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToLeBin)
        }
        ersd::PurePrim::NatToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToInt)
        }
        ersd::PurePrim::IntToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToNat)
        }
        ersd::PurePrim::IntToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::IntToFlt)
        }
        ersd::PurePrim::NatToFlt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::NatToFlt)
        }
        ersd::PurePrim::FltToInt(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToInt)
        }
        ersd::PurePrim::FltToNat(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::FltToNat)
        }
        ersd::PurePrim::Bin(bytes) => {
            let value = work.fresh(cont::Value::Pure(cont::Data::Bin(bytes.clone())));

            cont.call(work, value)
        }
        ersd::PurePrim::BinLen(bin) => lower_unary_code(work, bin, frame, cont, cont::Code::BinLen),
        ersd::PurePrim::BinEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::BinEql)
        }
        // Handle identity is byte identity: the rep is bytes from here down.
        ersd::PurePrim::IoEql(left, right) => {
            lower_binary_code(work, left, right, frame, cont, cont::Code::BinEql)
        }
        ersd::PurePrim::BinGet(bin, idx) => {
            lower_binary_code(work, bin, idx, frame, cont, cont::Code::BinGet)
        }
        ersd::PurePrim::BinSlice(bin, start, end) => {
            lower_ternary_code(work, bin, start, end, frame, cont, cont::Code::BinSlice)
        }
        ersd::PurePrim::BinAppend(bin, byte) => {
            lower_binary_code(work, bin, byte, frame, cont, cont::Code::BinAppend)
        }
        ersd::PurePrim::BinConcat(operands) => {
            lower_bin_concat(work, operands, frame, vec![], cont)
        }
        ersd::PurePrim::BinFlatten(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::BinFlatten)
        }
        ersd::PurePrim::Arr(elements) => lower_lst(work, elements, frame, vec![], cont),
        ersd::PurePrim::ArrLen(lst) => lower_unary_code(work, lst, frame, cont, cont::Code::ArrLen),
        ersd::PurePrim::ArrGet(lst, idx) => {
            lower_binary_code(work, lst, idx, frame, cont, cont::Code::ArrGet)
        }
        ersd::PurePrim::ArrSlice(lst, start, end) => {
            lower_ternary_code(work, lst, start, end, frame, cont, cont::Code::ArrSlice)
        }
        ersd::PurePrim::ArrAppend(lst, elem) => {
            lower_binary_code(work, lst, elem, frame, cont, cont::Code::ArrAppend)
        }
        ersd::PurePrim::ArrConcat(operands) => {
            lower_arr_concat(work, operands, frame, vec![], cont)
        }
        ersd::PurePrim::ArrFlatten(operand) => {
            lower_unary_code(work, operand, frame, cont, cont::Code::ArrFlatten)
        }
        ersd::PurePrim::ArrMap(src, f) => {
            lower_binary_code(work, src, f, frame, cont, cont::Code::ArrMap)
        }
    }
}
