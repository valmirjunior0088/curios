use {
    crate::{cont, core},
    std::{collections::HashMap, marker::PhantomData},
};

pub fn to_cont(core_module: core::Module) -> cont::Module {
    let mut cont_module = cont::Module::new();
    Visit::new(&mut cont_module, core_module.items()).visit_module(core_module);

    cont_module
}

struct Entropy<T> {
    entropy: usize,
    prefix: &'static str,
    marker: PhantomData<T>,
}

impl Entropy<cont::ValueName> {
    fn value() -> Self {
        Self {
            entropy: 0,
            prefix: "v",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::BlockName> {
    fn block() -> Self {
        Self {
            entropy: 0,
            prefix: "b",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::FuncName> {
    fn func() -> Self {
        Self {
            entropy: 0,
            prefix: "f",
            marker: PhantomData,
        }
    }
}

impl Entropy<cont::ClsrName> {
    fn clsr() -> Self {
        Self {
            entropy: 0,
            prefix: "c",
            marker: PhantomData,
        }
    }
}

impl<T> Entropy<T>
where
    T: From<String>,
{
    fn fresh(&mut self) -> T {
        let entropy = self.entropy;
        self.entropy += 1;

        T::from(format!("{}{entropy}", self.prefix))
    }
}

struct Symbols {
    symbols: HashMap<String, cont::FuncName>,
}

impl Symbols {
    fn new(items: &[core::Item]) -> Self {
        let mut func_entropy = Entropy::func();

        Self {
            symbols: items
                .iter()
                .map(|item| (item.label.clone(), func_entropy.fresh()))
                .collect(),
        }
    }

    fn symbol(&self, symbol: &str) -> cont::FuncName {
        self.symbols.get(symbol).unwrap().clone()
    }
}

struct BuildOutput {
    clsrs: Vec<(cont::ClsrName, cont::Clsr)>,
    values: Vec<(cont::ValueName, cont::Value)>,
    blocks: Vec<(cont::BlockName, cont::Block)>,
    tail: cont::Tail,
}

struct Build<'a> {
    symbols: &'a Symbols,
    captures: HashMap<String, cont::ValueName>,
    param: Option<(String, cont::ValueName)>,
    clsr_entropy: &'a mut Entropy<cont::ClsrName>,
    clsrs: Vec<(cont::ClsrName, cont::Clsr)>,
    value_entropy: Entropy<cont::ValueName>,
    values: Vec<(cont::ValueName, cont::Value)>,
    locals: HashMap<String, cont::ValueName>,
    block_entropy: Entropy<cont::BlockName>,
    blocks: Vec<(cont::BlockName, cont::Block)>,
    resume: cont::BlockName,
}

impl<'a> Build<'a> {
    fn new<A>(
        symbols: &'a Symbols,
        captures: A,
        param: Option<(String, cont::ValueName)>,
        clsr_entropy: &'a mut Entropy<cont::ClsrName>,
        value_entropy: Entropy<cont::ValueName>,
        resume: cont::BlockName,
    ) -> Self
    where
        A: Into<Option<HashMap<String, cont::ValueName>>>,
    {
        Self {
            symbols,
            captures: captures.into().unwrap_or_default(),
            param,
            clsr_entropy,
            clsrs: vec![],
            value_entropy,
            values: vec![],
            locals: HashMap::new(),
            block_entropy: Entropy::block(),
            blocks: vec![],
            resume,
        }
    }

    fn build_unit(&mut self) -> cont::ValueName {
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Pure(cont::ConstValue::Unit),
        ));

        value_name
    }

    fn build_int(&mut self, value: i32) -> cont::ValueName {
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Pure(cont::ConstValue::Int(value)),
        ));

        value_name
    }

    fn build_flt(&mut self, value: f32) -> cont::ValueName {
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Pure(cont::ConstValue::Flt(value)),
        ));

        value_name
    }

    fn build_int_eql(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::IntEql, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_int_add(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::IntAdd, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_int_sub(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::IntSub, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_int_mul(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::IntMul, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_flt_add(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::FltAdd, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_flt_sub(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::FltSub, vec![first_name, second_name]),
        ));

        value_name
    }

    fn build_flt_mul(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let first_name = self.build_term(first);
        let second_name = self.build_term(second);
        let value_name = self.value_entropy.fresh();

        self.values.push((
            value_name.clone(),
            cont::Value::Eval(cont::ConstOp::FltMul, vec![first_name, second_name]),
        ));

        value_name
    }

    fn find_value(&mut self, label: &str) -> cont::ValueName {
        if let Some(value_name) = self.locals.get(label) {
            return value_name.clone();
        }

        if let Some((param_label, param_value)) = &self.param {
            if param_label == label {
                return param_value.clone();
            }
        }

        if let Some(value_name) = self.captures.get(label) {
            return value_name.clone();
        }

        if let Some(func_name) = self.symbols.symbols.get(label) {
            let _ = func_name;
            todo!()
        }

        todo!()
    }

    fn build_func(&mut self, body: &core::Scope<core::One>) -> cont::ValueName {
        let labels = body.collect().into_iter().collect::<Vec<_>>();

        let mut value_entropy = Entropy::value();

        let closure_captures = labels
            .iter()
            .map(|label| (label.clone(), value_entropy.fresh()))
            .collect::<HashMap<_, _>>();

        let clsr_param = value_entropy.fresh();

        let clsr_param_label = clsr_param.string.clone();

        let clsr_body = body.open(&[&core::Term::label(&clsr_param_label)]);

        let fields = labels
            .iter()
            .map(|label| closure_captures.get(label).unwrap().clone())
            .collect::<Vec<_>>();

        let resume = self.block_entropy.fresh();

        let BuildOutput {
            clsrs,
            values,
            blocks,
            tail,
        } = Build::new(
            self.symbols,
            Some(closure_captures),
            Some((clsr_param_label, clsr_param.clone())),
            self.clsr_entropy,
            value_entropy,
            resume.clone(),
        )
        .build(&clsr_body);

        self.clsrs.extend(clsrs);

        let clsr_name = self.clsr_entropy.fresh();

        self.clsrs.push((
            clsr_name.clone(),
            cont::Clsr {
                fields,
                params: vec![clsr_param],
                resume,
                region: cont::Region {
                    values,
                    blocks,
                    tail,
                },
            },
        ));

        let value_name = self.value_entropy.fresh();

        let clsr_fields = labels
            .iter()
            .map(|label| self.find_value(label))
            .collect::<Vec<_>>();

        self.values.push((
            value_name.clone(),
            cont::Value::Clsr(clsr_name, clsr_fields),
        ));

        value_name
    }

    fn build_apply(&mut self, head: &core::Term, param: &core::Term) -> cont::ValueName {
        let _ = (head, param);
        todo!()
    }

    fn build_pair(&mut self, first: &core::Term, second: &core::Term) -> cont::ValueName {
        let _ = (first, second);
        todo!()
    }

    fn build_split(
        &mut self,
        head: &core::Term,
        motive: &core::Scope<core::One>,
        tail: &core::Scope<core::Two>,
    ) -> cont::ValueName {
        let _ = (head, motive, tail);
        todo!()
    }

    fn build_atom(&mut self, atom: &core::Atom) -> cont::ValueName {
        let _ = atom;
        todo!()
    }

    fn build_match(
        &mut self,
        head: &core::Term,
        motive: &core::Scope<core::One>,
        cases: &std::collections::BTreeMap<core::Atom, Box<core::Term>>,
    ) -> cont::ValueName {
        let _ = (head, motive, cases);
        todo!()
    }

    fn build_let(
        &mut self,
        type_: &core::Term,
        body: &core::Term,
        tail: &core::Scope<core::One>,
    ) -> cont::ValueName {
        let _ = (type_, body, tail);
        todo!()
    }

    fn build_let_rec(
        &mut self,
        items: &[(core::Scope<core::Many>, core::Scope<core::Many>)],
        tail: &core::Scope<core::Many>,
    ) -> cont::ValueName {
        let _ = (items, tail);
        todo!()
    }

    fn build_name(&mut self, name: &core::Name) -> cont::ValueName {
        let _ = name;
        todo!()
    }

    fn build_term(&mut self, body: &core::Term) -> cont::ValueName {
        match body {
            core::Term::Type => self.build_unit(),
            core::Term::Intrinsic { intrinsic } => match intrinsic {
                core::Intrinsic::IntType => self.build_unit(),
                core::Intrinsic::Int(value) => self.build_int(*value),
                core::Intrinsic::IntEql(first, second) => self.build_int_eql(first, second),
                core::Intrinsic::IntAdd(first, second) => self.build_int_add(first, second),
                core::Intrinsic::IntSub(first, second) => self.build_int_sub(first, second),
                core::Intrinsic::IntMul(first, second) => self.build_int_mul(first, second),
                core::Intrinsic::FltType => self.build_unit(),
                core::Intrinsic::Flt(value) => self.build_flt(f32::from_bits(*value)),
                core::Intrinsic::FltAdd(first, second) => self.build_flt_add(first, second),
                core::Intrinsic::FltSub(first, second) => self.build_flt_sub(first, second),
                core::Intrinsic::FltMul(first, second) => self.build_flt_mul(first, second),
            },
            core::Term::FuncType { .. } => self.build_unit(),
            core::Term::Func { body } => self.build_func(body),
            core::Term::Apply { head, param } => self.build_apply(head, param),
            core::Term::PairType { .. } => self.build_unit(),
            core::Term::Pair { first, second } => self.build_pair(first, second),
            core::Term::Split { head, motive, tail } => self.build_split(head, motive, tail),
            core::Term::AtomType { .. } => self.build_unit(),
            core::Term::Atom { atom } => self.build_atom(atom),
            core::Term::Match {
                head,
                motive,
                cases,
            } => self.build_match(head, motive, cases),
            core::Term::Let { type_, body, tail } => self.build_let(type_, body, tail),
            core::Term::LetRec { items, tail } => self.build_let_rec(items, tail),
            core::Term::Name { name } => self.build_name(name),
        }
    }

    fn build(mut self, body: &core::Term) -> BuildOutput {
        let value_name = self.build_term(body);

        BuildOutput {
            clsrs: self.clsrs,
            values: self.values,
            blocks: self.blocks,
            tail: cont::Tail::Jump(cont::JumpTarget {
                target: self.resume,
                params: vec![value_name],
            }),
        }
    }
}

struct Visit<'a> {
    clsr_entropy: Entropy<cont::ClsrName>,
    symbols: Symbols,
    module: &'a mut cont::Module,
}

impl<'a> Visit<'a> {
    fn new(module: &'a mut cont::Module, items: &[core::Item]) -> Self {
        Self {
            clsr_entropy: Entropy::clsr(),
            symbols: Symbols::new(items),
            module,
        }
    }

    fn visit_definition(&mut self, label: &str, body: &core::Term) {
        let BuildOutput {
            clsrs,
            values,
            blocks,
            tail,
        } = Build::new(
            &self.symbols,
            None,
            None,
            &mut self.clsr_entropy,
            Entropy::value(),
            cont::BlockName::from(label),
        )
        .build(body);

        for (clsr_name, clsr) in clsrs {
            self.module.add_clsr(clsr_name, clsr);
        }

        self.module.add_func(
            self.symbols.symbol(label),
            cont::Func {
                params: vec![],
                resume: cont::BlockName::from(label),
                region: cont::Region {
                    values,
                    blocks,
                    tail,
                },
            },
        );
    }

    fn visit_module(&mut self, module: core::Module) {
        for item in module.items() {
            self.visit_definition(&item.label, &item.body);
        }
    }
}
