use {curios_base::Span, curios_core::Qualifier};

/// A surface reference, exactly as written in source: a [`Qualifier`](curios_core::Qualifier) plus an `is_abs` flag marking a leading `/` (an absolute, root-anchored path). It is *not* a canonical identity — resolution turns a `Name` into an always-absolute `Qualifier` — so equality and hashing compare the written form (ignoring the span, as everywhere in this crate).
#[derive(Debug, Clone)]
pub struct Name {
    span: Option<Span>,
    is_abs: bool,
    qualifier: Qualifier,
}

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.is_abs == other.is_abs && self.qualifier == other.qualifier
    }
}

impl Eq for Name {}

impl std::hash::Hash for Name {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.is_abs.hash(state);
        self.qualifier.hash(state);
    }
}

impl Name {
    pub(crate) fn new(is_abs: bool, qualifier: Qualifier) -> Self {
        Self {
            span: None,
            is_abs,
            qualifier,
        }
    }

    pub(crate) fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub(crate) fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub(crate) fn is_abs(&self) -> bool {
        self.is_abs
    }

    pub(crate) fn qualifier(&self) -> &Qualifier {
        &self.qualifier
    }

    pub(crate) fn with(&self, segment: &str) -> Self {
        Self {
            span: self.span.clone(),
            is_abs: self.is_abs,
            qualifier: self.qualifier.with(segment),
        }
    }

    pub(crate) fn join(&self) -> String {
        // A `Name` is a *surface* reference printed back as written: an absolute
        // reference keeps `Qualifier::join`'s leading `/`; a relative one strips it.
        // (Canonical core identities go through `Qualifier::join` directly and are
        // always absolute — this `is_abs`-respecting form is only for surface text.)
        match self.is_abs {
            true => self.qualifier.join(),
            false => self.qualifier.join().trim_start_matches('/').to_string(),
        }
    }

    pub(crate) fn is_single(&self) -> bool {
        self.qualifier.is_single()
    }

    pub(crate) fn head(&self) -> &str {
        self.qualifier.head()
    }

    pub(crate) fn last(&self) -> &str {
        self.qualifier.last()
    }
}

impl<S, I> From<I> for Name
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    fn from(iter: I) -> Self {
        Self {
            span: None,
            is_abs: false,
            qualifier: Qualifier::from(iter),
        }
    }
}
