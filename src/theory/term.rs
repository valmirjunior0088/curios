use super::{Scope, Variable, Visit, Visitable, Visitor};

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

    FunctionType(Box<Type>, Scope<1, Type>),
    Function(Scope<1, Term>),
    Apply(Box<Term>, Box<Term>),

    TupleType(Box<Type>, Scope<1, Type>),
    Tuple(Box<Term>, Box<Term>),
    With(Box<Term>, Scope<2, Term>),

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
    pub fn boxed(self) -> Box<Self> {
        Box::new(self)
    }

    pub fn unbound<const N: usize>(self) -> Scope<N, Self> {
        Scope::unbound(self)
    }

    pub fn bound<const N: usize>(self, names: [impl Into<String>; N]) -> Scope<N, Self> {
        Scope::bound(self, names.map(Into::into))
    }

    pub fn global(name: impl Into<String>) -> Self {
        Self::Global(name.into())
    }

    pub fn local(name: impl Into<Variable>) -> Self {
        Self::Local(name.into())
    }

    pub fn function_type(input: Type, variable: impl Into<String>, output: Type) -> Self {
        Self::FunctionType(input.boxed(), output.bound([variable]))
    }

    pub fn function_type_unbound(input: Type, output: Type) -> Self {
        Self::FunctionType(input.boxed(), output.unbound())
    }

    pub fn function(variable: impl Into<String>, output: Term) -> Self {
        Self::Function(output.bound([variable]))
    }

    pub fn apply(function: Term, argument: Term) -> Self {
        Self::Apply(function.boxed(), argument.boxed())
    }

    pub fn tuple_type(left: Type, variable: impl Into<String>, right: Type) -> Self {
        Self::TupleType(left.boxed(), right.bound([variable]))
    }

    pub fn tuple_type_unbound(left: Type, right: Type) -> Self {
        Self::TupleType(left.boxed(), right.unbound())
    }

    pub fn tuple(left: Term, right: Term) -> Self {
        Self::Tuple(left.boxed(), right.boxed())
    }

    pub fn with(scrutinee: Term, left: impl Into<String>, right: impl Into<String>, output: Term) -> Self {
        Self::With(scrutinee.boxed(), output.bound([left.into(), right.into()]))
    }

    pub fn rune_type(runes: impl IntoIterator<Item = impl Into<String>>) -> Self {
        Self::RuneType(runes.into_iter().map(Into::into).collect())
    }

    pub fn rune(rune: impl Into<String>) -> Self {
        Self::Rune(rune.into())
    }

    pub fn when(scrutinee: Term, branches: impl IntoIterator<Item = (impl Into<String>, Term)>) -> Self {
        Self::When(
            scrutinee.boxed(),
            branches.into_iter().map(|(key, value)| (key.into(), value)).collect(),
        )
    }

    pub fn int32_if(scrutinee: Term, truthy: Term, falsy: Term) -> Self {
        Self::Int32If(scrutinee.boxed(), truthy.boxed(), falsy.boxed())
    }

    pub fn int32_bin_op(op: BinOp, left: Term, right: Term) -> Self {
        Self::Int32BinOp(op, left.boxed(), right.boxed())
    }

    pub fn int32_bool_op(op: BoolOp, left: Term, right: Term) -> Self {
        Self::Int32BoolOp(op, left.boxed(), right.boxed())
    }

    pub fn int32_comp_op(op: CompOp, left: Term, right: Term) -> Self {
        Self::Int32CompOp(op, left.boxed(), right.boxed())
    }

    pub fn flt32_bin_op(op: BinOp, left: Term, right: Term) -> Self {
        Self::Flt32BinOp(op, left.boxed(), right.boxed())
    }

    pub fn flt32_comp_op(op: CompOp, left: Term, right: Term) -> Self {
        Self::Flt32CompOp(op, left.boxed(), right.boxed())
    }
}

impl Visitable for Term {
    fn pure(variable: Variable) -> Self {
        Self::Local(variable)
    }

    fn bind(self, v: &Visitor<Self>) -> Self {
        use Term::*;

        match self {
            Global(name) => Global(name),
            Local(variable) => v.apply(variable),

            Type => Type,

            FunctionType(input, output) => FunctionType(v.visit(input), v.visit(output)),
            Function(output) => Function(v.visit(output)),
            Apply(function, argument) => Apply(v.visit(function), v.visit(argument)),

            TupleType(left, right) => TupleType(v.visit(left), v.visit(right)),
            Tuple(left, right) => Tuple(v.visit(left), v.visit(right)),
            With(scrutinee, output) => With(v.visit(scrutinee), v.visit(output)),

            RuneType(runes) => RuneType(runes),
            Rune(rune) => Rune(rune),
            When(scrutinee, branches) => When(v.visit(scrutinee), v.visit(branches)),

            Int32Type => Int32Type,
            Int32(number) => Int32(number),
            Int32If(scrutinee, truthy, falsy) => Int32If(v.visit(scrutinee), v.visit(truthy), v.visit(falsy)),
            Int32BinOp(op, left, right) => Int32BinOp(op, v.visit(left), v.visit(right)),
            Int32BoolOp(op, left, right) => Int32BoolOp(op, v.visit(left), v.visit(right)),
            Int32CompOp(op, left, right) => Int32CompOp(op, v.visit(left), v.visit(right)),

            Flt32Type => Flt32Type,
            Flt32(number) => Flt32(number),
            Flt32BinOp(op, left, right) => Flt32BinOp(op, v.visit(left), v.visit(right)),
            Flt32CompOp(op, left, right) => Flt32CompOp(op, v.visit(left), v.visit(right)),
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
        let id_function_type = Term::function_type(Term::Type, "A", {
            Term::function_type_unbound(Term::local("A"), {
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
