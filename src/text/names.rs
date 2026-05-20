use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq)]
pub struct Name {
    pub path: Vec<String>,
}
