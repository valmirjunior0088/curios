use {
    super::{
        Argument, Block, BlockName, CallTarget, CellTarget, Clsr, ClsrName, Code, Data, Func,
        FuncName, HostTarget, JumpTarget, Module, Region, Tail, Value, ValueName,
    },
    curios_base::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_value_name<'a>(name: &'a ValueName) -> Printer<'a> {
    flat([pure("%"), pure(name.as_str())])
}

fn print_value_names<'a>(names: &'a [ValueName]) -> Printer<'a> {
    sep_flat(names.iter().map(print_value_name), || pure(", "))
}

/// A function/closure argument, prefixed with `*` when it is a specialization
/// candidate (its erased type was a function, a `Type`, or unit).
fn print_argument<'a>(arg: &'a Argument) -> Printer<'a> {
    if arg.candidate {
        flat([pure("*"), print_value_name(&arg.name)])
    } else {
        print_value_name(&arg.name)
    }
}

fn print_arguments<'a>(args: &'a [Argument]) -> Printer<'a> {
    sep_flat(args.iter().map(print_argument), || pure(", "))
}

fn print_block_name<'a>(name: &'a BlockName) -> Printer<'a> {
    pure(name.as_str())
}

fn print_func_name<'a>(name: &'a FuncName) -> Printer<'a> {
    pure(name.as_str())
}

fn print_clsr_name<'a>(name: &'a ClsrName) -> Printer<'a> {
    pure(name.as_str())
}

fn print_data<'a>(value: &'a Data) -> Printer<'a> {
    match value {
        Data::Nat(value) => pure(value.to_string()),
        Data::Int(value) => pure(value.to_string()),
        Data::Flt(value) => pure(value.to_string()),
        Data::Bin(bytes) => pure(
            bytes
                .iter()
                .map(|b| format!("\\{:02x}", b))
                .collect::<String>(),
        ),
        Data::Lst(elems) => flat([pure("["), print_value_names(elems), pure("]")]),
        Data::Tpl(elems) => flat([pure("("), print_value_names(elems), pure(")")]),
        Data::Clsr(target, fields) => flat([
            print_clsr_name(target),
            pure("{"),
            print_value_names(fields),
            pure("}"),
        ]),
    }
}

fn print_binary<'a>(name: &'static str, left: &'a ValueName, right: &'a ValueName) -> Printer<'a> {
    flat([
        pure(name),
        pure(" "),
        print_value_name(left),
        pure(", "),
        print_value_name(right),
    ])
}

fn print_unary<'a>(name: &'static str, operand: &'a ValueName) -> Printer<'a> {
    flat([pure(name), pure(" "), print_value_name(operand)])
}

fn print_code<'a>(op: &'a Code) -> Printer<'a> {
    match op {
        Code::NatEql(l, r) => print_binary("Nat.eql", l, r),
        Code::NatNeq(l, r) => print_binary("Nat.neq", l, r),
        Code::NatAdd(l, r) => print_binary("Nat.add", l, r),
        Code::NatSub(l, r) => print_binary("Nat.sub", l, r),
        Code::NatMul(l, r) => print_binary("Nat.mul", l, r),
        Code::NatLt(l, r) => print_binary("Nat.lt", l, r),
        Code::NatDiv(l, r) => print_binary("Nat.div", l, r),
        Code::NatRem(l, r) => print_binary("Nat.rem", l, r),
        Code::NatGt(l, r) => print_binary("Nat.gt", l, r),
        Code::NatLte(l, r) => print_binary("Nat.lte", l, r),
        Code::NatGte(l, r) => print_binary("Nat.gte", l, r),
        Code::IntEql(l, r) => print_binary("Int.eql", l, r),
        Code::IntNeq(l, r) => print_binary("Int.neq", l, r),
        Code::IntAdd(l, r) => print_binary("Int.add", l, r),
        Code::IntSub(l, r) => print_binary("Int.sub", l, r),
        Code::IntMul(l, r) => print_binary("Int.mul", l, r),
        Code::IntDiv(l, r) => print_binary("Int.div", l, r),
        Code::IntRem(l, r) => print_binary("Int.rem", l, r),
        Code::IntLt(l, r) => print_binary("Int.lt", l, r),
        Code::IntGt(l, r) => print_binary("Int.gt", l, r),
        Code::IntLte(l, r) => print_binary("Int.lte", l, r),
        Code::IntGte(l, r) => print_binary("Int.gte", l, r),
        Code::NatAnd(l, r) => print_binary("Nat.and", l, r),
        Code::NatOr(l, r) => print_binary("Nat.or", l, r),
        Code::NatXor(l, r) => print_binary("Nat.xor", l, r),
        Code::NatShl(l, r) => print_binary("Nat.shl", l, r),
        Code::NatShr(l, r) => print_binary("Nat.shr", l, r),
        Code::NatRotl(l, r) => print_binary("Nat.rotl", l, r),
        Code::NatRotr(l, r) => print_binary("Nat.rotr", l, r),
        Code::NatClz(o) => print_unary("Nat.clz", o),
        Code::NatCtz(o) => print_unary("Nat.ctz", o),
        Code::NatPopcnt(o) => print_unary("Nat.popcnt", o),
        Code::NatEqz(o) => print_unary("Nat.eqz", o),
        Code::IntAnd(l, r) => print_binary("Int.and", l, r),
        Code::IntOr(l, r) => print_binary("Int.or", l, r),
        Code::IntXor(l, r) => print_binary("Int.xor", l, r),
        Code::IntShl(l, r) => print_binary("Int.shl", l, r),
        Code::IntShr(l, r) => print_binary("Int.shr", l, r),
        Code::IntRotl(l, r) => print_binary("Int.rotl", l, r),
        Code::IntRotr(l, r) => print_binary("Int.rotr", l, r),
        Code::IntClz(o) => print_unary("Int.clz", o),
        Code::IntCtz(o) => print_unary("Int.ctz", o),
        Code::IntPopcnt(o) => print_unary("Int.popcnt", o),
        Code::IntEqz(o) => print_unary("Int.eqz", o),
        Code::FltAdd(l, r) => print_binary("Flt.add", l, r),
        Code::FltSub(l, r) => print_binary("Flt.sub", l, r),
        Code::FltMul(l, r) => print_binary("Flt.mul", l, r),
        Code::FltDiv(l, r) => print_binary("Flt.div", l, r),
        Code::FltRem(l, r) => print_binary("Flt.rem", l, r),
        Code::FltEql(l, r) => print_binary("Flt.eql", l, r),
        Code::FltNeq(l, r) => print_binary("Flt.neq", l, r),
        Code::FltLt(l, r) => print_binary("Flt.lt", l, r),
        Code::FltGt(l, r) => print_binary("Flt.gt", l, r),
        Code::FltLte(l, r) => print_binary("Flt.lte", l, r),
        Code::FltGte(l, r) => print_binary("Flt.gte", l, r),
        Code::FltMin(l, r) => print_binary("Flt.min", l, r),
        Code::FltMax(l, r) => print_binary("Flt.max", l, r),
        Code::FltNeg(o) => print_unary("Flt.neg", o),
        Code::FltAbs(o) => print_unary("Flt.abs", o),
        Code::FltSqrt(o) => print_unary("Flt.sqrt", o),
        Code::FltFloor(o) => print_unary("Flt.floor", o),
        Code::FltCeil(o) => print_unary("Flt.ceil", o),
        Code::FltTrunc(o) => print_unary("Flt.trunc", o),
        Code::FltNearest(o) => print_unary("Flt.nearest", o),
        Code::FltCopysign(l, r) => print_binary("Flt.copysign", l, r),
        Code::NatToInt(o) => print_unary("Nat.to_int", o),
        Code::NatToFlt(o) => print_unary("Nat.to_flt", o),
        Code::IntToNat(o) => print_unary("Int.to_nat", o),
        Code::IntToFlt(o) => print_unary("Int.to_flt", o),
        Code::FltToLeBin(o) => print_unary("Flt.to_le_bin", o),
        Code::FltToNat(o) => print_unary("Flt.to_nat", o),
        Code::FltToInt(o) => print_unary("Flt.to_int", o),
        Code::BinLen(bin) => print_unary("Bin.len", bin),
        Code::BinEql(l, r) => print_binary("Bin.eql", l, r),
        Code::BinGet(bin, idx) => print_binary("Bin.get", bin, idx),
        Code::BinSlice(bin, start, end) => flat([
            pure("Bin.slice"),
            pure(" "),
            print_value_name(bin),
            pure(", "),
            print_value_name(start),
            pure(", "),
            print_value_name(end),
        ]),
        Code::BinAppend(bin, byte) => print_binary("Bin.append", bin, byte),
        Code::BinConcat(operands) => {
            flat([pure("Bin.concat"), pure(" "), print_value_names(operands)])
        }
        Code::LstLen(lst) => print_unary("Lst.len", lst),
        Code::LstGet(lst, idx) => print_binary("Lst.get", lst, idx),
        Code::LstSlice(lst, start, end) => flat([
            pure("Lst.slice"),
            pure(" "),
            print_value_name(lst),
            pure(", "),
            print_value_name(start),
            pure(", "),
            print_value_name(end),
        ]),
        Code::LstAppend(lst, elem) => print_binary("Lst.append", lst, elem),
        Code::LstConcat(operands) => {
            flat([pure("Lst.concat"), pure(" "), print_value_names(operands)])
        }
        Code::LstMap(src, f) => print_binary("Lst.map", src, f),
        Code::TplGet(tuple, index) => flat([
            pure("Tpl.get "),
            print_value_name(tuple),
            pure(" "),
            pure(index.to_string()),
        ]),
    }
}

fn print_let_value<'a>(name: &'a ValueName, value: &'a Value) -> Printer<'a> {
    flat([
        pure("let "),
        print_value_name(name),
        pure(" = "),
        match value {
            Value::Pure(value) => print_data(value),
            Value::Eval(op) => print_code(op),
            Value::Alias(source) => print_value_name(source),
        },
        pure(";"),
    ])
}

fn print_prealloc<'a>(name: &'a ValueName, clsr: &'a ClsrName) -> Printer<'a> {
    flat([
        pure("prealloc "),
        print_value_name(name),
        pure(": "),
        print_clsr_name(clsr),
        pure(";"),
    ])
}

fn print_let_block<'a>(name: &'a BlockName, block: &'a Block) -> Printer<'a> {
    flat([
        pure("let "),
        print_block_name(name),
        pure("["),
        print_value_names(&block.params),
        pure("] =\n"),
        indent(print_region(&block.region)),
        pure(";"),
    ])
}

fn print_target<'a>(target: &'a JumpTarget) -> Printer<'a> {
    flat([
        print_block_name(&target.target),
        pure("["),
        print_value_names(&target.params),
        pure("]"),
    ])
}

fn print_tail<'a>(tail: &'a Tail) -> Printer<'a> {
    match tail {
        Tail::Jump(target) => print_target(target),
        Tail::Match(target) => flat([
            pure("match "),
            print_value_name(&target.operand),
            pure("\n"),
            flat(target.cases.iter().map(|(&value, target)| {
                flat([
                    pure("| "),
                    pure(value.to_string()),
                    pure(" => "),
                    print_target(target),
                    pure("\n"),
                ])
            })),
            match &target.default {
                Some(default) => flat([pure("| _ => "), print_target(default)]),
                None => pure("| _ => unreachable"),
            },
        ]),
        Tail::Call(target) => flat(match target {
            CallTarget::Direct {
                target: target_name,
                params,
                resume,
            } => [
                print_func_name(target_name),
                pure("("),
                print_value_names(params),
                pure(") "),
                print_block_name(resume),
            ],
            CallTarget::Indirect {
                target: target_name,
                params,
                resume,
            } => [
                print_value_name(target_name),
                pure("("),
                print_value_names(params),
                pure(") "),
                print_block_name(resume),
            ],
        }),
        Tail::Host(host) => match host {
            HostTarget::Foreign {
                function,
                operands,
                resume,
            } => flat(
                [pure(function.label.clone())]
                    .into_iter()
                    .chain(
                        operands
                            .iter()
                            .flat_map(|operand| [pure(" "), print_value_name(operand)]),
                    )
                    .chain([pure(" "), print_block_name(resume)])
                    .collect::<Vec<_>>(),
            ),
            HostTarget::IoExit { code, resume } => flat([
                pure("Io.exit "),
                print_value_name(code),
                pure(" "),
                print_block_name(resume),
            ]),
        },
        Tail::Cell(cell) => match cell {
            CellTarget::New { init, resume } => flat([
                pure("Cell.new "),
                print_value_name(init),
                pure(" "),
                print_block_name(resume),
            ]),
            CellTarget::Set {
                cell,
                value,
                resume,
            } => flat([
                pure("Cell.set "),
                print_value_name(cell),
                pure(" "),
                print_value_name(value),
                pure(" "),
                print_block_name(resume),
            ]),
            CellTarget::Get { cell, resume } => flat([
                pure("Cell.get "),
                print_value_name(cell),
                pure(" "),
                print_block_name(resume),
            ]),
        },
        Tail::Unreachable => pure("unreachable"),
    }
}

fn print_region<'a>(region: &'a Region) -> Printer<'a> {
    sep_flat(
        (region
            .preallocs
            .iter()
            .map(|(name, prealloc)| print_prealloc(name, prealloc)))
        .chain(
            region
                .values
                .iter()
                .map(|(name, value)| print_let_value(name, value)),
        )
        .chain(
            region
                .blocks
                .iter()
                .map(|(name, block)| print_let_block(name, block)),
        )
        .chain([print_tail(&region.tail)]),
        || pure("\n"),
    )
}

fn print_let_const<'a>(name: &'a ValueName, value: &'a Data) -> Printer<'a> {
    flat([
        pure("let "),
        print_value_name(name),
        pure(" = "),
        print_data(value),
        pure(";"),
    ])
}

fn print_let_clsr<'a>(name: &'a ClsrName, clsr: &'a Clsr) -> Printer<'a> {
    flat([
        pure("let "),
        print_clsr_name(name),
        pure("{"),
        print_arguments(&clsr.fields),
        pure("}("),
        print_arguments(&clsr.params),
        pure(") "),
        print_block_name(&clsr.resume),
        pure(" =\n"),
        indent(flat([print_region(&clsr.region), pure(";")])),
    ])
}

fn print_let_func<'a>(name: &'a FuncName, func: &'a Func) -> Printer<'a> {
    flat([
        pure("let "),
        print_func_name(name),
        pure("("),
        print_arguments(&func.params),
        pure(") "),
        print_block_name(&func.resume),
        pure(" =\n"),
        indent(flat([print_region(&func.region), pure(";")])),
    ])
}

fn print_module<'a>(module: &'a Module) -> Printer<'a> {
    sep_flat(
        (module
            .consts()
            .iter()
            .map(|(name, value)| print_let_const(name, value)))
        .chain(
            module
                .clsrs()
                .iter()
                .map(|(name, clsr)| print_let_clsr(name, clsr)),
        )
        .chain(
            module
                .funcs()
                .iter()
                .map(|(name, func)| print_let_func(name, func)),
        ),
        || pure("\n"),
    )
}

impl Display for Module {
    fn fmt(&self, formatter: &mut Formatter<'_>) -> Result {
        run_printer(print_module(self), formatter, 2)?;

        Ok(())
    }
}
