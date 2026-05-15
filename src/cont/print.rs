use {
    super::{
        Block, BlockName, CallTarget, Clsr, ClsrName, Code, Data, Func, FuncName, JumpTarget,
        Module, Region, Tail, Value, ValueName,
    },
    crate::printer::{Printer, flat, indent, pure, run_printer, sep_flat},
    std::fmt::{Display, Formatter, Result},
};

fn print_value_name<'a>(name: &'a ValueName) -> Printer<'a> {
    flat([pure("%"), pure(&name.string)])
}

fn print_value_names<'a>(names: &'a [ValueName]) -> Printer<'a> {
    sep_flat(names.iter().map(print_value_name), || pure(", "))
}

fn print_block_name<'a>(name: &'a BlockName) -> Printer<'a> {
    pure(&name.string)
}

fn print_func_name<'a>(name: &'a FuncName) -> Printer<'a> {
    pure(&name.string)
}

fn print_clsr_name<'a>(name: &'a ClsrName) -> Printer<'a> {
    pure(&name.string)
}

fn print_data<'a>(value: &'a Data) -> Printer<'a> {
    match value {
        Data::Unit => pure("()"),
        Data::Nat(value) => pure(value.to_string()),
        Data::Int(value) => pure(value.to_string()),
        Data::Flt(value) => pure(value.to_string()),
        Data::Bin(bytes) => pure(bytes.iter().map(|b| format!("\\{:02x}", b)).collect::<String>()),
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

fn print_code<'a>(op: &'a Code) -> Printer<'a> {
    match op {
        Code::NatEql(left, right) => flat([
            pure("Nat.eql"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatNeq(left, right) => flat([
            pure("Nat.neq"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatAdd(left, right) => flat([
            pure("Nat.add"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatSub(left, right) => flat([
            pure("Nat.sub"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatMul(left, right) => flat([
            pure("Nat.mul"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatLt(left, right) => flat([
            pure("Nat.lt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatDiv(left, right) => flat([
            pure("Nat.div"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatRem(left, right) => flat([
            pure("Nat.rem"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatGt(left, right) => flat([
            pure("Nat.gt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatLte(left, right) => flat([
            pure("Nat.lte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatGte(left, right) => flat([
            pure("Nat.gte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntEql(left, right) => flat([
            pure("Int.eql"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntNeq(left, right) => flat([
            pure("Int.neq"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntAdd(left, right) => flat([
            pure("Int.add"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntSub(left, right) => flat([
            pure("Int.sub"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntMul(left, right) => flat([
            pure("Int.mul"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntDiv(left, right) => flat([
            pure("Int.div"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntRem(left, right) => flat([
            pure("Int.rem"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntLt(left, right) => flat([
            pure("Int.lt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntGt(left, right) => flat([
            pure("Int.gt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntLte(left, right) => flat([
            pure("Int.lte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntGte(left, right) => flat([
            pure("Int.gte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatAnd(left, right) => flat([
            pure("Nat.and"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatOr(left, right) => flat([
            pure("Nat.or"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatXor(left, right) => flat([
            pure("Nat.xor"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatShl(left, right) => flat([
            pure("Nat.shl"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatShr(left, right) => flat([
            pure("Nat.shr"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatRotl(left, right) => flat([
            pure("Nat.rotl"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatRotr(left, right) => flat([
            pure("Nat.rotr"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatClz(operand) => flat([pure("Nat.clz"), pure(" "), print_value_name(operand)]),
        Code::NatCtz(operand) => flat([pure("Nat.ctz"), pure(" "), print_value_name(operand)]),
        Code::NatPopcnt(operand) => flat([pure("Nat.popcnt"), pure(" "), print_value_name(operand)]),
        Code::NatEqz(operand) => flat([pure("Nat.eqz"), pure(" "), print_value_name(operand)]),
        Code::IntAnd(left, right) => flat([
            pure("Int.and"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntOr(left, right) => flat([
            pure("Int.or"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntXor(left, right) => flat([
            pure("Int.xor"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntShl(left, right) => flat([
            pure("Int.shl"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntShr(left, right) => flat([
            pure("Int.shr"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntRotl(left, right) => flat([
            pure("Int.rotl"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntRotr(left, right) => flat([
            pure("Int.rotr"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::IntClz(operand) => flat([pure("Int.clz"), pure(" "), print_value_name(operand)]),
        Code::IntCtz(operand) => flat([pure("Int.ctz"), pure(" "), print_value_name(operand)]),
        Code::IntPopcnt(operand) => flat([pure("Int.popcnt"), pure(" "), print_value_name(operand)]),
        Code::IntEqz(operand) => flat([pure("Int.eqz"), pure(" "), print_value_name(operand)]),
        Code::FltAdd(left, right) => flat([
            pure("Flt.add"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltSub(left, right) => flat([
            pure("Flt.sub"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltMul(left, right) => flat([
            pure("Flt.mul"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltDiv(left, right) => flat([
            pure("Flt.div"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltEql(left, right) => flat([
            pure("Flt.eql"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltNeq(left, right) => flat([
            pure("Flt.neq"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltLt(left, right) => flat([
            pure("Flt.lt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltGt(left, right) => flat([
            pure("Flt.gt"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltLte(left, right) => flat([
            pure("Flt.lte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltGte(left, right) => flat([
            pure("Flt.gte"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltMin(left, right) => flat([
            pure("Flt.min"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltMax(left, right) => flat([
            pure("Flt.max"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::FltNeg(operand) => flat([pure("Flt.neg"), pure(" "), print_value_name(operand)]),
        Code::FltAbs(operand) => flat([pure("Flt.abs"), pure(" "), print_value_name(operand)]),
        Code::FltSqrt(operand) => flat([pure("Flt.sqrt"), pure(" "), print_value_name(operand)]),
        Code::FltFloor(operand) => flat([pure("Flt.floor"), pure(" "), print_value_name(operand)]),
        Code::FltCeil(operand) => flat([pure("Flt.ceil"), pure(" "), print_value_name(operand)]),
        Code::FltTrunc(operand) => flat([pure("Flt.trunc"), pure(" "), print_value_name(operand)]),
        Code::FltNearest(operand) => {
            flat([pure("Flt.nearest"), pure(" "), print_value_name(operand)])
        }
        Code::FltCopysign(left, right) => flat([
            pure("Flt.copysign"),
            pure(" "),
            print_value_name(left),
            pure(", "),
            print_value_name(right),
        ]),
        Code::NatToInt(operand) => flat([pure("Nat.to_int"), pure(" "), print_value_name(operand)]),
        Code::IntToNat(operand) => flat([pure("Int.to_nat"), pure(" "), print_value_name(operand)]),
        Code::IntToFlt(operand) => flat([pure("Int.to_flt"), pure(" "), print_value_name(operand)]),
        Code::NatToFlt(operand) => flat([pure("Nat.to_flt"), pure(" "), print_value_name(operand)]),
        Code::FltToInt(operand) => flat([pure("Flt.to_int"), pure(" "), print_value_name(operand)]),
        Code::FltToNat(operand) => flat([pure("Flt.to_nat"), pure(" "), print_value_name(operand)]),
        Code::NatToBin(operand) => flat([pure("Nat.to_bin"), pure(" "), print_value_name(operand)]),
        Code::BinLen(bin) => flat([pure("Bin.len"), pure(" "), print_value_name(bin)]),
        Code::BinGet(idx, bin) => flat([
            pure("Bin.get"),
            pure(" "),
            print_value_name(idx),
            pure(", "),
            print_value_name(bin),
        ]),
        Code::BinSlice(start, end, bin) => flat([
            pure("Bin.slice"),
            pure(" "),
            print_value_name(start),
            pure(", "),
            print_value_name(end),
            pure(", "),
            print_value_name(bin),
        ]),
        Code::BinConcat(operands) => flat([
            pure("Bin.concat"),
            pure(" "),
            print_value_names(operands),
        ]),
        Code::LstLen(lst) => flat([pure("Lst.len"), pure(" "), print_value_name(lst)]),
        Code::LstGet(idx, lst) => flat([
            pure("Lst.get"),
            pure(" "),
            print_value_name(idx),
            pure(", "),
            print_value_name(lst),
        ]),
        Code::LstSlice(start, end, lst) => flat([
            pure("Lst.slice"),
            pure(" "),
            print_value_name(start),
            pure(", "),
            print_value_name(end),
            pure(", "),
            print_value_name(lst),
        ]),
        Code::LstConcat(operands) => flat([
            pure("Lst.concat"),
            pure(" "),
            print_value_names(operands),
        ]),
        Code::TplGet(index, tuple) => flat([
            pure("Tpl.get "),
            pure(index.to_string()),
            pure(" "),
            print_value_name(tuple),
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
            flat(target.targets.iter().enumerate().map(|(index, target)| {
                flat([
                    pure("| "),
                    pure(index.to_string()),
                    pure(" -> "),
                    print_target(target),
                    pure("\n"),
                ])
            })),
            match &target.default {
                Some(default) => flat([pure("| _ -> "), print_target(default)]),
                None => pure("| _ -> unreachable"),
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
    }
}

fn print_region<'a>(region: &'a Region) -> Printer<'a> {
    sep_flat(
        (region
            .values
            .iter()
            .map(|(name, value)| print_let_value(name, value)))
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
        print_value_names(&clsr.fields),
        pure("}("),
        print_value_names(&clsr.params),
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
        print_value_names(&func.params),
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
