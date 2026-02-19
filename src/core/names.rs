#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Name {
    pub index: usize,
    pub label: String,
}

impl Name {
    pub fn new<A>(index: usize, label: A) -> Self
    where
        A: Into<String>,
    {
        Self {
            index,
            label: label.into(),
        }
    }
}

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub struct Atom {
    pub string: String,
}

impl<A> From<A> for Atom
where
    A: Into<String>,
{
    fn from(string: A) -> Self {
        Self {
            string: string.into(),
        }
    }
}
