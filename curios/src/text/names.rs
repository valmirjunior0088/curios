use crate::Span;

// `Qualifier` is a canonical, resolved identity: a sequence of module segments
// rooted at the module root. It is what the resolution tables key on.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Qualifier {
    segments: Vec<String>,
}

impl Qualifier {
    pub fn empty() -> Self {
        Self { segments: vec![] }
    }

    pub fn with(&self, segment: &str) -> Self {
        Self {
            segments: self
                .segments
                .iter()
                .cloned()
                .chain([segment.to_string()])
                .collect(),
        }
    }

    pub fn join(&self) -> String {
        // A canonical resolved identity is absolute: it carries a leading `/` so a
        // hand-built reference (e.g. the string-literal meta-emitter's `/syn/Str/…`)
        // matches a definition's key unambiguously. The empty (root) qualifier joins
        // to the empty string, not a bare `/`.
        match self.segments.is_empty() {
            true => String::new(),
            false => format!("/{}", self.segments.join("/")),
        }
    }

    pub fn is_single(&self) -> bool {
        self.segments.len() == 1
    }

    pub fn head(&self) -> &str {
        &self.segments[0]
    }

    pub fn last(&self) -> &str {
        self.segments.last().unwrap()
    }

    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.segments.iter().map(String::as_str)
    }

    pub fn segments(&self) -> &[String] {
        &self.segments
    }
}

impl<S, I> From<I> for Qualifier
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    fn from(iter: I) -> Self {
        Self {
            segments: iter.into_iter().map(Into::into).collect(),
        }
    }
}

// `Name` is a surface reference, exactly as written in source: a `Qualifier` plus an
// `is_abs` flag marking a leading `/` (an absolute, root-anchored reference).
// Resolution turns a `Name` into a canonical `Qualifier`.
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
    pub fn new(is_abs: bool, qualifier: Qualifier) -> Self {
        Self {
            span: None,
            is_abs,
            qualifier,
        }
    }

    pub fn with_span(mut self, span: Span) -> Self {
        self.span = Some(span);
        self
    }

    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn is_abs(&self) -> bool {
        self.is_abs
    }

    pub fn qualifier(&self) -> &Qualifier {
        &self.qualifier
    }

    pub fn with(&self, segment: &str) -> Self {
        Self {
            span: self.span.clone(),
            is_abs: self.is_abs,
            qualifier: self.qualifier.with(segment),
        }
    }

    pub fn join(&self) -> String {
        // A `Name` is a *surface* reference printed back as written: an absolute
        // reference keeps `Qualifier::join`'s leading `/`; a relative one strips it.
        // (Canonical core identities go through `Qualifier::join` directly and are
        // always absolute — this `is_abs`-respecting form is only for surface text.)
        match self.is_abs {
            true => self.qualifier.join(),
            false => self.qualifier.join().trim_start_matches('/').to_string(),
        }
    }

    pub fn is_single(&self) -> bool {
        self.qualifier.is_single()
    }

    pub fn head(&self) -> &str {
        self.qualifier.head()
    }

    pub fn last(&self) -> &str {
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
