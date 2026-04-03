use {
    super::{
        Block, BlockName, CallTarget, Clsr, ClsrName, ConstOp, ConstValue, FltOp, Func, FuncName,
        IntOp, JumpTarget, Module, Region, Tail, Value, ValueName,
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

fn print_const_value<'a>(value: &'a ConstValue) -> Printer<'a> {
    match value {
        ConstValue::Unit => pure("()"),
        ConstValue::Int(value) => pure(value.to_string()),
        ConstValue::Flt(value) => pure(value.to_string()),
    }
}

fn print_const_op<'a>(op: &'a ConstOp) -> Printer<'a> {
    pure(match op {
        ConstOp::Int(IntOp::Eql) => "int.eql",
        ConstOp::Int(IntOp::Add) => "int.add",
        ConstOp::Int(IntOp::Sub) => "int.sub",
        ConstOp::Int(IntOp::Mul) => "int.mul",
        ConstOp::Flt(FltOp::Add) => "flt.add",
        ConstOp::Flt(FltOp::Sub) => "flt.sub",
        ConstOp::Flt(FltOp::Mul) => "flt.mul",
    })
}

fn print_let_value<'a>(name: &'a ValueName, value: &'a Value) -> Printer<'a> {
    flat([
        pure("let "),
        print_value_name(name),
        pure(" = "),
        match value {
            Value::Pure(value) => print_const_value(value),
            Value::Eval(op, params) => flat([
                print_const_op(op),
                if params.is_empty() {
                    pure("")
                } else {
                    flat([pure(" "), print_value_names(params)])
                },
            ]),
            Value::Clsr(target, fields) => flat([
                print_clsr_name(target),
                pure("{"),
                print_value_names(fields),
                pure("}"),
            ]),
            Value::Tpl2(first, second) => flat([
                pure("("),
                print_value_name(first),
                pure(", "),
                print_value_name(second),
                pure(")"),
            ]),
            Value::Proj(tuple, index) => flat([
                pure("proj "),
                pure(index.to_string()),
                pure(" "),
                print_value_name(tuple),
            ]),
            Value::Name(source) => print_value_name(source),
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
        Tail::Case(target) => flat([
            pure("| "),
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

fn print_let_const<'a>(name: &'a ValueName, value: &'a ConstValue) -> Printer<'a> {
    flat([
        pure("let "),
        print_value_name(name),
        pure(" = "),
        print_const_value(value),
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
