use super::{Accept, Inject, Scope, Variable, Visitor};

#[derive(Debug, Clone, Copy)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
}

#[derive(Debug, Clone, Copy)]
pub enum BoolOp {
    And,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub enum CompOp {
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
}

pub type Type = Term;

#[derive(Debug, Clone)]
pub enum Term {
    Global(String),
    Local(Variable),

    Type,

    FunctionType(Box<Type>, Scope<Type>),
    Function(Scope<Term>),
    Apply(Box<Term>, Box<Term>),

    TupleType(Box<Type>, Scope<Type>),
    Tuple(Box<Term>, Box<Term>),
    With(Box<Term>, Scope<Term>),

    RuneType(Vec<String>),
    Rune(String),
    When(Box<Term>, Vec<(String, Term)>),

    Int32Type,
    Int32(i32),
    Int32If(Box<Term>, Box<Term>, Box<Term>),
    Int32BinOp(BinOp, Box<Term>, Box<Term>),
    Int32BoolOp(BoolOp, Box<Term>, Box<Term>),
    Int32CompOp(CompOp, Box<Term>, Box<Term>),

    Flt32Type,
    Flt32(f32),
    Flt32BinOp(BinOp, Box<Term>, Box<Term>),
    Flt32CompOp(CompOp, Box<Term>, Box<Term>),
}

impl Term {
    pub fn global(name: impl Into<String>) -> Self {
        Self::Global(name.into())
    }

    pub fn local(name: impl Into<Variable>) -> Self {
        Self::Local(name.into())
    }

    pub fn function_type(input: impl Into<Box<Type>>, variable: Option<impl Into<String>>, output: Type) -> Self {
        Self::FunctionType(input.into(), Scope::construct(variable, output))
    }

    pub fn function(variable: impl Into<String>, output: Term) -> Self {
        Self::Function(Scope::construct(Some(variable), output))
    }

    pub fn apply(function: impl Into<Box<Term>>, argument: impl Into<Box<Term>>) -> Self {
        Self::Apply(function.into(), argument.into())
    }

    pub fn tuple_type(left: impl Into<Box<Type>>, variable: Option<impl Into<String>>, right: Type) -> Self {
        Self::TupleType(left.into(), Scope::construct(variable, right))
    }

    pub fn tuple(left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Tuple(left.into(), right.into())
    }

    pub fn with(
        scrutinee: impl Into<Box<Term>>,
        left: impl Into<String>,
        right: impl Into<String>,
        output: Term,
    ) -> Self {
        Self::With(scrutinee.into(), Scope::construct([left.into(), right.into()], output))
    }

    pub fn rune_type(runes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self::RuneType(runes.into_iter().map(Into::into).collect())
    }

    pub fn rune(rune: impl Into<String>) -> Self {
        Self::Rune(rune.into())
    }

    pub fn when(
        scrutinee: impl Into<Box<Term>>,
        branches: impl IntoIterator<Item = (impl Into<String>, Term)>,
    ) -> Self {
        Self::When(
            scrutinee.into(),
            branches.into_iter().map(|(key, value)| (key.into(), value)).collect(),
        )
    }

    pub fn int32_if(
        scrutinee: impl Into<Box<Term>>,
        truthy: impl Into<Box<Term>>,
        falsy: impl Into<Box<Term>>,
    ) -> Self {
        Self::Int32If(scrutinee.into(), truthy.into(), falsy.into())
    }

    pub fn int32_bin_op(op: BinOp, left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Int32BinOp(op, left.into(), right.into())
    }

    pub fn int32_bool_op(op: BoolOp, left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Int32BoolOp(op, left.into(), right.into())
    }

    pub fn int32_comp_op(op: CompOp, left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Int32CompOp(op, left.into(), right.into())
    }

    pub fn flt32_bin_op(op: BinOp, left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Flt32BinOp(op, left.into(), right.into())
    }

    pub fn flt32_comp_op(op: CompOp, left: impl Into<Box<Term>>, right: impl Into<Box<Term>>) -> Self {
        Self::Flt32CompOp(op, left.into(), right.into())
    }
}

impl Inject for Term {
    fn inject(variable: Variable) -> Self {
        Self::Local(variable)
    }
}

impl<K, V: Accept<V>> Accept<V> for Vec<(K, V)> {
    fn accept(self, visitor: &mut Visitor<V>) -> Self {
        self.into_iter().map(|(k, v)| (k, v.accept(visitor))).collect()
    }
}

impl Accept<Self> for Term {
    fn accept(self, v: &mut Visitor<Self>) -> Self {
        use Term::*;

        match self {
            Global(name) => Global(name),
            Local(variable) => v.visit(variable),

            Type => Type,

            FunctionType(input, output) => FunctionType(input.accept(v), output.accept(v)),
            Function(output) => Function(output.accept(v)),
            Apply(function, argument) => Apply(function.accept(v), argument.accept(v)),

            TupleType(left, right) => TupleType(left.accept(v), right.accept(v)),
            Tuple(left, right) => Tuple(left.accept(v), right.accept(v)),
            With(scrutinee, output) => With(scrutinee.accept(v), output.accept(v)),

            RuneType(runes) => RuneType(runes),
            Rune(rune) => Rune(rune),
            When(scrutinee, branches) => When(scrutinee.accept(v), branches.accept(v)),

            Int32Type => Int32Type,
            Int32(number) => Int32(number),
            Int32If(scrutinee, truthy, falsy) => Int32If(scrutinee.accept(v), truthy.accept(v), falsy.accept(v)),
            Int32BinOp(op, left, right) => Int32BinOp(op, left.accept(v), right.accept(v)),
            Int32BoolOp(op, left, right) => Int32BoolOp(op, left.accept(v), right.accept(v)),
            Int32CompOp(op, left, right) => Int32CompOp(op, left.accept(v), right.accept(v)),

            Flt32Type => Flt32Type,
            Flt32(number) => Flt32(number),
            Flt32BinOp(op, left, right) => Flt32BinOp(op, left.accept(v), right.accept(v)),
            Flt32CompOp(op, left, right) => Flt32CompOp(op, left.accept(v), right.accept(v)),
        }
    }
}

#[derive(Debug)]
pub enum Item {
    Declaration(String, Type),
    Definition(String, Term),
    Composite(String, Type, Term),
}

impl Item {
    pub fn declaration(name: impl Into<String>, declaration: Type) -> Self {
        Self::Declaration(name.into(), declaration)
    }

    pub fn definition(name: impl Into<String>, definition: Term) -> Self {
        Self::Definition(name.into(), definition)
    }

    pub fn composite(name: impl Into<String>, declaration: Type, definition: Term) -> Self {
        Self::Composite(name.into(), declaration, definition)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[ignore]
    #[rustfmt::skip]
    fn it_works() {
        let id_function_type = Term::function_type(Term::Type, Some("A"), {
            Term::function_type(Term::local("A"), None as Option<String>, {
                Term::local("A")
            })
        });

        println!("{:?}", id_function_type);

        let id_function = Term::function("A", {
            Term::function("a", {
                Term::local("a")
            })
        });

        println!("{:?}", id_function);

        let rune_type = Term::rune_type(["asd", "dsa"]);

        println!("{:?}", rune_type);
    }
}
