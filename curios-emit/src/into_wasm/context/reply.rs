//! The guest's half of a host operation's contract: a reply is held to its row before the program reads it, so a host that is not this toolchain's own native one — the browser's, an embedder's, one scripted in a test — cannot hand the program a value its row does not allow. The checks are the row's own, read off `curios-abi` as the native adapter reads them. Every row's results are held to what their wire types can be, a failure's padding included. A builtin's status is held to the statuses its contract names, and a success to its checks, read only when the status says the reply is one. A violation refuses as `host_reply`.
//!
//! Every value a check reads waits in a local: the results, which `host_instrs` holds there anyway before boxing them, and the operands a check measures, kept as they cross. A count and a scalar compare as the `i64` they cross as, and a buffer or a list by its `array.len`.

use {
    super::Context,
    crate::into_wasm::{call, either, get, i32_const, i64_const, when},
    curios_abi::{Check, ForeignFunction, HostOp, Outcome, WireLeaf, WireType, status},
};

/// A value waiting in a local, with the wire type it crossed as.
pub(super) type Waiting = (WireType, curios_wasm::LocalName);

impl<'a> Context<'a, '_> {
    /// The operands a builtin's checks measure, by name: what `host_call_instrs` keeps in locals as they cross.
    pub(super) fn measured_operands(function: &ForeignFunction) -> Vec<&'static str> {
        let ForeignFunction::Builtin(op) = function else {
            return Vec::new();
        };

        op.checks()
            .iter()
            .filter_map(|check| match *check {
                Check::Progress { request } | Check::Exact { request } => Some(request),
                Check::Accepted { buffer } => Some(buffer),
                Check::Parallel { list } => Some(list),
                _ => None,
            })
            .collect()
    }

    /// Whether a reply of `function`'s needs checking at all, and so its results waiting in locals: a builtin with a status or a check, or any row with a result a wire type does not fill with every value its lane holds.
    pub(super) fn reply_checked(function: &ForeignFunction) -> bool {
        let physical = function.signature().results.iter().any(|(_, wire_type)| {
            matches!(
                wire_type,
                WireType::Bool | WireType::Byte | WireType::List(WireLeaf::Bool)
            )
        });

        physical
            || match function {
                ForeignFunction::Builtin(op) => {
                    op.outcome() != Outcome::Returns || !op.checks().is_empty()
                }
                ForeignFunction::Declared(_) => false,
            }
    }

    /// Hold the reply waiting in `results` to `function`'s row, `operands` holding what its checks measure, each at its row position.
    pub(super) fn reply_check_instrs(
        &self,
        function: &ForeignFunction,
        results: &[Waiting],
        operands: &[Option<Waiting>],
    ) -> Vec<curios_wasm::Instr> {
        let mut output = results
            .iter()
            .flat_map(|result| self.physical_check_instrs(result))
            .collect::<Vec<_>>();

        let ForeignFunction::Builtin(op) = function else {
            return output;
        };

        let labels = function
            .signature()
            .results
            .iter()
            .map(|(label, _)| label)
            .collect::<Vec<_>>();
        let field = |name: &str| {
            let index = labels
                .iter()
                .position(|label| *label == name)
                .unwrap_or_else(|| panic!("`{}` checks a field it has no `{name}` of", op.name()));

            &results[index]
        };
        let params = &function.signature().params;
        let operand = |name: &str| {
            params
                .iter()
                .position(|(param, _)| param == name)
                .and_then(|index| operands[index].as_ref())
                .unwrap_or_else(|| panic!("`{}` reads `{name}`, which it did not keep", op.name()))
        };
        let payload = || results.last().expect("a row check reads a payload");

        let success = op
            .checks()
            .iter()
            .flat_map(|check| self.check_instrs(*check, &field, &operand, &payload))
            .collect::<Vec<_>>();

        match op.outcome() {
            Outcome::Returns | Outcome::Diverges => output.extend(success),
            _ => {
                let (_, status) = &results[0];

                output.extend(self.status_check_instrs(*op, status));

                if !success.is_empty() {
                    output.extend([get(status), curios_wasm::Instr::I64Eqz, when(success)]);
                }
            }
        }

        output
    }

    /// What a result's wire type is: a `Bool` is `0` or `1`, a `Byte` at most `255`, and every word of a `List(Bool)` either. These hold whichever way the reply went, since a failure's padding is a value the guest boxes all the same.
    fn physical_check_instrs(&self, (wire_type, local): &Waiting) -> Vec<curios_wasm::Instr> {
        let refuse = self.table().refuse_instrs(curios_cont::Panic::HostReply);

        match wire_type {
            WireType::Bool => vec![
                get(local),
                i32_const(1),
                curios_wasm::Instr::I32GtU,
                when(refuse),
            ],
            WireType::Byte => vec![
                get(local),
                i32_const(255),
                curios_wasm::Instr::I32GtU,
                when(refuse),
            ],
            WireType::List(WireLeaf::Bool) => vec![
                get(local),
                curios_wasm::Instr::RefAsNonNull,
                call(&self.table().reply_bools_func()),
                curios_wasm::Instr::I32Eqz,
                when(refuse),
            ],
            _ => vec![],
        }
    }

    /// Refuse a status `op` never answers. The named codes sit below `OTHER_BASE` and are read off one bitmask; past it is the errno lane, which a row answers whole or not at all.
    fn status_check_instrs(
        &self,
        op: HostOp,
        status: &curios_wasm::LocalName,
    ) -> Vec<curios_wasm::Instr> {
        let named = (0..status::OTHER_BASE)
            .filter(|&code| op.answers(code))
            .fold(0u64, |mask, code| mask | 1 << code);
        let lane = match op.answers(status::OTHER_BASE) {
            true => vec![
                get(status),
                i64_const(status::OTHER_BASE as i64),
                curios_wasm::Instr::I64Sub,
                i64_const(status::ERRNO_MAX as i64),
                curios_wasm::Instr::I64LeU,
            ],
            false => vec![i32_const(0)],
        };

        vec![
            get(status),
            i64_const(status::OTHER_BASE as i64),
            curios_wasm::Instr::I64LtU,
            either(
                curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                vec![
                    i64_const(named as i64),
                    get(status),
                    curios_wasm::Instr::I64ShrU,
                    curios_wasm::Instr::I32WrapI64,
                    i32_const(1),
                    curios_wasm::Instr::I32And,
                ],
                lane,
            ),
            curios_wasm::Instr::I32Eqz,
            when(self.table().refuse_instrs(curios_cont::Panic::HostReply)),
        ]
    }

    /// One check of a success, refusing where it does not hold.
    fn check_instrs<'r>(
        &self,
        check: Check,
        field: &impl Fn(&str) -> &'r Waiting,
        operand: &impl Fn(&str) -> &'r Waiting,
        payload: &impl Fn() -> &'r Waiting,
    ) -> Vec<curios_wasm::Instr> {
        let refuse = self.table().refuse_instrs(curios_cont::Panic::HostReply);
        let violated = |condition: Vec<curios_wasm::Instr>| {
            condition
                .into_iter()
                .chain([when(refuse.clone())])
                .collect::<Vec<_>>()
        };

        match check {
            // Between one and `n` when `n` is positive, and none when it is zero — `count` a read's length or a write's count, `bound` the request or the buffer's length.
            Check::Progress { request } => {
                violated(bounded(measure(payload()), measure(operand(request))))
            }
            Check::Accepted { buffer } => {
                violated(bounded(measure(payload()), measure(operand(buffer))))
            }
            Check::Exact { request } => violated(
                [measure(payload()), measure(operand(request))]
                    .concat()
                    .into_iter()
                    .chain([curios_wasm::Instr::I64Ne])
                    .collect(),
            ),
            Check::Parallel { list } => violated(
                [measure(payload()), measure(operand(list))]
                    .concat()
                    .into_iter()
                    .chain([curios_wasm::Instr::I64Ne])
                    .collect(),
            ),
            Check::NonEmpty => violated(
                measure(payload())
                    .into_iter()
                    .chain([curios_wasm::Instr::I64Eqz])
                    .collect(),
            ),
            Check::Present { field: name } => violated(
                measure(field(name))
                    .into_iter()
                    .chain([curios_wasm::Instr::I64Eqz])
                    .collect(),
            ),
            Check::Mask {
                field: name,
                allowed,
            } => {
                let (_, bytes) = field(name);

                violated(vec![
                    get(bytes),
                    curios_wasm::Instr::RefAsNonNull,
                    i32_const(i32::from(allowed)),
                    call(&self.table().reply_masks_func()),
                    curios_wasm::Instr::I32Eqz,
                ])
            }
            Check::Below { field: name, bound } => {
                let (_, value) = field(name);

                violated(vec![
                    get(value),
                    i64_const(bound as i64),
                    curios_wasm::Instr::I64GeU,
                ])
            }
            Check::Code { field: name, codes } => {
                let (_, value) = field(name);
                let named = codes.iter().enumerate().flat_map(|(index, &code)| {
                    [
                        get(value),
                        i64_const(code as i64),
                        curios_wasm::Instr::I64Eq,
                    ]
                    .into_iter()
                    .chain((index > 0).then_some(curios_wasm::Instr::I32Or))
                });

                violated(named.chain([curios_wasm::Instr::I32Eqz]).collect())
            }
            // Exactly one field active: an exit code at most 255 with no signal, or a signal with no code.
            Check::Exit { code, signal } => {
                let ((_, code), (_, signal)) = (field(code), field(signal));

                violated(vec![
                    get(signal),
                    curios_wasm::Instr::I64Eqz,
                    get(code),
                    i64_const(255),
                    curios_wasm::Instr::I64LeU,
                    curios_wasm::Instr::I32And,
                    get(signal),
                    i64_const(0),
                    curios_wasm::Instr::I64Ne,
                    get(code),
                    curios_wasm::Instr::I64Eqz,
                    curios_wasm::Instr::I32And,
                    curios_wasm::Instr::I32Or,
                    curios_wasm::Instr::I32Eqz,
                ])
            }
        }
    }
}

/// What a check reads of a waiting value, as an `i64`: a count or a scalar as the number it crossed as, a buffer or a list by its length.
fn measure((wire_type, local): &Waiting) -> Vec<curios_wasm::Instr> {
    match wire_type {
        WireType::Nat | WireType::Int => vec![get(local)],
        _ => vec![
            get(local),
            curios_wasm::Instr::ArrayLen,
            curios_wasm::Instr::I64ExtendI32U,
        ],
    }
}

/// Whether `count` breaks the bound `bound` sets: between one and `bound` when `bound` is positive, and none when it is zero.
fn bounded(
    count: Vec<curios_wasm::Instr>,
    bound: Vec<curios_wasm::Instr>,
) -> Vec<curios_wasm::Instr> {
    let above = [count.clone(), bound.clone()]
        .concat()
        .into_iter()
        .chain([curios_wasm::Instr::I64GtU]);
    let none = count.iter().cloned().chain([curios_wasm::Instr::I64Eqz]);

    bound
        .into_iter()
        .chain([
            curios_wasm::Instr::I64Eqz,
            either(
                curios_wasm::ValType::Num(curios_wasm::NumType::I32),
                count
                    .iter()
                    .cloned()
                    .chain([i64_const(0), curios_wasm::Instr::I64Ne])
                    .collect(),
                none.chain(above)
                    .chain([curios_wasm::Instr::I32Or])
                    .collect(),
            ),
        ])
        .collect()
}
