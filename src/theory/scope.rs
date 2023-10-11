#[derive(Debug, PartialEq, Eq, Clone)]
enum Variety {
    Free(String),
    Bound(usize, usize),
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Variable {
    variety: Variety,
}

impl Variable {
    pub fn free(name: String) -> Self {
        Self {
            variety: Variety::Free(name),
        }
    }

    fn bound(index: usize, qualifier: usize) -> Self {
        Self {
            variety: Variety::Bound(index, qualifier),
        }
    }

    pub fn name(&self) -> &str {
        match &self.variety {
            Variety::Free(name) => name,
            Variety::Bound(_, _) => panic!("bound variable -- should not happen"),
        }
    }
}

impl From<String> for Variable {
    fn from(value: String) -> Self {
        Self::free(value)
    }
}

impl From<&str> for Variable {
    fn from(value: &str) -> Self {
        Self::free(value.to_owned())
    }
}

pub trait Inject {
    fn inject(variable: Variable) -> Self;
}

pub struct Visitor<'a, A> {
    depth: usize,
    visit: &'a mut dyn FnMut(usize, Variable) -> A,
}

impl<'a, A: Accept<A>> Visitor<'a, A> {
    fn run(data: A, visit: &mut impl FnMut(usize, Variable) -> A) -> A {
        data.accept(&mut Visitor { depth: 0, visit })
    }

    fn descend(&mut self) -> Visitor<A> {
        Visitor {
            depth: self.depth + 1,
            visit: self.visit,
        }
    }

    pub fn visit(&mut self, variable: Variable) -> A {
        (self.visit)(self.depth, variable)
    }
}

pub trait Accept<A> {
    fn accept(self, visitor: &mut Visitor<A>) -> Self;
}

impl<A: Accept<A>> Accept<A> for Box<A> {
    fn accept(self, visitor: &mut Visitor<A>) -> Self {
        Self::new((*self).accept(visitor))
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Scope<A> {
    density: Vec<usize>,
    content: Box<A>,
}

impl<A: Inject + Accept<A> + Clone> Scope<A> {
    pub fn construct(targets: impl IntoIterator<Item = impl Into<String>>, content: A) -> Self {
        let targets = targets.into_iter().map(Into::into).collect::<Vec<_>>();

        if targets.is_empty() {
            return Self {
                density: vec![],
                content: Box::new(content),
            };
        }

        let mut density = vec![0; targets.len()];

        let content = Visitor::run(content, &mut |depth, variable| {
            let name = match &variable.variety {
                Variety::Free(name) => name,
                Variety::Bound(_, _) => return Inject::inject(variable),
            };

            let qualifier = match targets.iter().position(|target| target == name) {
                Some(qualifier) => qualifier,
                None => return Inject::inject(variable),
            };

            density[qualifier] += 1;

            Inject::inject(Variable::bound(depth, qualifier))
        });

        Self {
            density,
            content: Box::new(content),
        }
    }

    pub fn density(&self) -> &[usize] {
        &self.density
    }

    pub fn eliminate(self, sources: impl IntoIterator<Item = A>) -> A {
        if self.density.into_iter().sum::<usize>() == 0 {
            return *self.content;
        }

        let sources = sources.into_iter().collect::<Vec<_>>();

        Visitor::run(*self.content, &mut |depth, variable| {
            let (index, qualifier) = match variable.variety {
                Variety::Bound(index, qualifier) => (index, qualifier),
                Variety::Free(_) => return Inject::inject(variable),
            };

            let source = match depth == index {
                true => sources.get(qualifier).unwrap(),
                false => return Inject::inject(variable),
            };

            source.clone()
        })
    }
}

impl<A: Accept<A>> Accept<A> for Scope<A> {
    fn accept(self, visitor: &mut Visitor<A>) -> Self {
        Self {
            density: self.density,
            content: self.content.accept(&mut visitor.descend()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::time::Instant;

    #[derive(Debug, PartialEq, Eq, Clone)]
    enum Exp {
        Var(Variable),
        Abs(Scope<Self>),
        App(Box<Self>, Box<Self>),
    }

    impl Inject for Exp {
        fn inject(variable: Variable) -> Self {
            Self::Var(variable)
        }
    }

    impl Accept<Self> for Exp {
        fn accept(self, v: &mut Visitor<Self>) -> Self {
            match self {
                Self::Var(variable) => v.visit(variable),
                Self::Abs(output) => Self::Abs(output.accept(v)),
                Self::App(function, argument) => Self::App(function.accept(v), argument.accept(v)),
            }
        }
    }

    impl Exp {
        fn var(variable: impl Into<Variable>) -> Self {
            Self::Var(variable.into())
        }

        fn abs(target: impl Into<String>, output: Self) -> Self {
            Self::Abs(Scope::construct([target], output))
        }

        fn app(function: impl Into<Box<Self>>, argument: impl Into<Box<Self>>) -> Self {
            Self::App(function.into(), argument.into())
        }

        fn eval(self) -> Self {
            match self {
                Self::Var(variable) => Self::Var(variable),
                Self::Abs(output) => Self::Abs(output),
                Self::App(function, argument) => match function.eval() {
                    Self::Abs(output) => output.eliminate([*argument]).eval(),
                    function => Self::app(function, argument),
                },
            }
        }
    }

    #[test]
    fn it_works() {
        let identity = (0..3000).fold(Exp::abs("x", Exp::var("x")), |identity, index| {
            println!("{index} and counting");

            Exp::app(Exp::abs("x", Exp::var("x")), identity)
        });

        let now = Instant::now();
        println!("Evaluating...");
        let result = Exp::app(identity, Exp::var("test")).eval();
        println!("Done in {}ms!", now.elapsed().as_millis());

        assert_eq!(result, Exp::var("test"));
    }
}
