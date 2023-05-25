use std::{fmt, rc::Rc};

#[derive(Debug, Clone, PartialEq, Eq)]
enum Variety {
    Free(String),
    Bound(usize),
}

#[derive(Clone, PartialEq, Eq)]
pub struct Variable {
    variety: Variety,
}

impl Variable {
    pub fn free(name: String) -> Self {
        Self {
            variety: Variety::Free(name),
        }
    }

    pub fn unwrap(&self) -> &str {
        match &self.variety {
            Variety::Free(name) => name,
            Variety::Bound(_) => panic!("bound variable -- should not happen"),
        }
    }

    fn bound(index: usize) -> Self {
        Self {
            variety: Variety::Bound(index),
        }
    }
}

impl fmt::Debug for Variable {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        match &self.variety {
            Variety::Free(name) => write!(formatter, "\"@{name}\""),
            Variety::Bound(number) => write!(formatter, "\"#{number}\""),
        }
    }
}

impl From<String> for Variable {
    fn from(value: String) -> Self {
        Variable::free(value)
    }
}

impl From<&str> for Variable {
    fn from(value: &str) -> Self {
        Variable::free(value.to_owned())
    }
}

pub trait Visit<A> {
    fn visit(&self, a: A) -> A;
}

pub trait Visitable: Sized + Clone + 'static {
    fn pure(variable: Variable) -> Self;
    fn bind(self, visitor: &Visitor<Self>) -> Self;
}

pub struct Visitor<V> {
    action: Rc<dyn Fn(usize, Variable) -> V + 'static>,
    depth: usize,
}

impl<V: Visitable> Visitor<V> {
    pub fn apply(&self, variable: Variable) -> V {
        (self.action)(self.depth, variable)
    }

    fn run(v: V, action: impl Fn(usize, Variable) -> V + 'static) -> V {
        v.bind(&Self {
            action: Rc::new(action),
            depth: 0,
        })
    }
}

impl<V: Visitable> Visit<Box<V>> for Visitor<V> {
    fn visit(&self, v: Box<V>) -> Box<V> {
        v.bind(self).into()
    }
}

impl<const N: usize, V: Visitable> Visit<Scope<N, V>> for Visitor<V> {
    fn visit(&self, scope: Scope<N, V>) -> Scope<N, V> {
        scope.map(|v| {
            v.bind(&Self {
                action: Rc::clone(&self.action),
                depth: self.depth + N,
            })
        })
    }
}

impl<K, V: Visitable> Visit<Vec<(K, V)>> for Visitor<V> {
    fn visit(&self, vs: Vec<(K, V)>) -> Vec<(K, V)> {
        vs.into_iter().map(|(k, v)| (k, v.bind(self))).collect()
    }
}

#[derive(Clone)]
pub struct Scope<const N: usize, V> {
    v: Box<V>,
    unbound: bool,
}

impl<const N: usize, V: Visitable> Scope<N, V> {
    pub fn unbound(v: V) -> Self {
        Self {
            v: Box::new(v),
            unbound: true,
        }
    }

    pub fn bound(v: V, targets: [String; N]) -> Self {
        let v = Visitor::run(v, move |depth, variable| {
            let name = match &variable.variety {
                Variety::Free(name) => name,
                _ => return V::pure(variable),
            };

            match targets.iter().position(|target| name == target) {
                Some(index) => V::pure(Variable::bound(index + depth)),
                None => V::pure(variable),
            }
        });

        Self {
            v: Box::new(v),
            unbound: false,
        }
    }

    pub fn instantiate(self, vs: [V; N]) -> V {
        if self.unbound {
            return *self.v;
        }

        Visitor::run(*self.v, move |depth, variable| {
            let index = match variable.variety {
                Variety::Bound(index) => index,
                _ => return V::pure(variable),
            };

            match index >= depth {
                true => vs[index - depth].to_owned(),
                false => V::pure(variable),
            }
        })
    }

    fn map(self, f: impl Fn(V) -> V) -> Self {
        Self {
            v: Box::new(f(*self.v)),
            unbound: self.unbound,
        }
    }
}

impl<const N: usize, V: fmt::Debug> fmt::Debug for Scope<N, V> {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> Result<(), fmt::Error> {
        write!(formatter, "{{ {:?} }}", self.v)
    }
}
