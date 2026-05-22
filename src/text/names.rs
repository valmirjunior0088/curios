use crate::macros::name;

name!(Atom);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    path: Vec<String>,
}

impl Name {
    pub fn empty() -> Self {
        Self { path: vec![] }
    }

    pub fn with(&self, segment: &str) -> Self {
        Self {
            path: self
                .path
                .iter()
                .cloned()
                .chain([segment.to_string()])
                .collect(),
        }
    }

    pub fn extend(&self, tail: &[String]) -> Self {
        Self::from(self.path.iter().cloned().chain(tail.iter().cloned()))
    }

    pub fn join(&self) -> String {
        self.path.join("/")
    }

    pub fn is_single(&self) -> bool {
        self.path.len() == 1
    }

    pub fn head(&self) -> &str {
        &self.path[0]
    }

    pub fn last(&self) -> &str {
        self.path.last().unwrap()
    }

    pub fn tail(&self) -> &[String] {
        &self.path[1..]
    }

    pub fn interior(&self) -> &[String] {
        &self.path[1..self.path.len() - 1]
    }
}

impl<S, I> From<I> for Name
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    fn from(iter: I) -> Self {
        Self {
            path: iter.into_iter().map(Into::into).collect(),
        }
    }
}
