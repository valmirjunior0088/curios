use {
    curios_utilities::{Qualifier, Span},
    std::{
        fmt,
        hash::{Hash, Hasher},
        ops::Deref,
    },
};

/// A surface reference, exactly as written in source: a [`Qualifier`] plus an `is_abs` flag marking a leading `/` (an absolute, root-anchored path). It is *not* a canonical identity — resolution turns a `Name` into an always-absolute `Qualifier` — so equality and hashing compare the written form (ignoring the spans, as everywhere in this crate).
#[derive(Debug, Clone)]
pub struct Name {
    span: Option<Span>,
    /// Where the *last* segment alone was written, when it was written apart from the path that reaches it — a `use m/{a, b}` group member, whose path is the group's and whose word is its own. `None` for a name written as one run of text, where [`Name::span`] already covers the last segment.
    ///
    /// Two spans, because a resolution can fail about either half and a reader needs the caret on the half that is wrong: `use /nope/{a}` is about `/nope` and `use /std/{nope}` is about `nope`. See `into_core::context::attach`, which picks between them by asking the refusal which half it names.
    leaf: Option<Span>,
    is_abs: bool,
    qualifier: Qualifier,
}

impl Name {
    pub(crate) fn new(is_abs: bool, qualifier: Qualifier) -> Self {
        Self {
            span: None,
            leaf: None,
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

    /// Where the last segment alone was written, falling back to the whole path's span when the two were written together.
    pub(crate) fn leaf_span(&self) -> Option<&Span> {
        self.leaf.as_ref().or(self.span.as_ref())
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
            leaf: None,
            is_abs: self.is_abs,
            qualifier: self.qualifier.with(segment),
        }
    }

    /// This name extended by a segment the reader wrote *apart* from it — a `use m/{a}` group member. The path keeps its own span and the segment contributes [`Name::leaf_span`], so a refusal about `a` underlines `a` while one about `m` still underlines `m`.
    pub(crate) fn with_label(&self, label: &Label) -> Self {
        Self {
            leaf: label.span().cloned(),
            ..self.with(label)
        }
    }

    pub(crate) fn join(&self) -> String {
        // A `Name` is a *surface* reference printed back as written: an absolute reference keeps `Qualifier::join`'s leading `/`; a relative one strips it. (Canonical core identities go through `Qualifier::join` directly and are always absolute — this `is_abs`-respecting form is only for surface text.)
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

impl PartialEq for Name {
    fn eq(&self, other: &Self) -> bool {
        self.is_abs == other.is_abs && self.qualifier == other.qualifier
    }
}

impl Eq for Name {}

impl Hash for Name {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.is_abs.hash(state);
        self.qualifier.hash(state);
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
            leaf: None,
            is_abs: false,
            qualifier: Qualifier::from(iter),
        }
    }
}

/// A written identifier at a declaring position — a binder, a declaration's name, a `use` selector — with the span of the word itself, where a [`Term`](super::Term)'s span covers the whole form it heads. Equality and hashing compare the text alone, as [`Name`]'s do: tests build spanless expected trees, and two labels spelled alike are one name however they were located.
#[derive(Debug, Clone)]
pub struct Label {
    text: String,
    span: Option<Span>,
}

impl Label {
    pub(crate) fn spanned(text: impl Into<String>, span: Span) -> Self {
        Self {
            text: text.into(),
            span: Some(span),
        }
    }

    /// Where the word was written, or `None` for a label the compiler spelled.
    pub fn span(&self) -> Option<&Span> {
        self.span.as_ref()
    }

    pub fn as_str(&self) -> &str {
        &self.text
    }
}

impl Deref for Label {
    type Target = str;

    fn deref(&self) -> &str {
        &self.text
    }
}

impl fmt::Display for Label {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(&self.text)
    }
}

impl PartialEq for Label {
    fn eq(&self, other: &Self) -> bool {
        self.text == other.text
    }
}

impl Eq for Label {}

impl PartialEq<str> for Label {
    fn eq(&self, other: &str) -> bool {
        self.text == other
    }
}

impl PartialEq<&str> for Label {
    fn eq(&self, other: &&str) -> bool {
        self.text == *other
    }
}

impl Hash for Label {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.text.hash(state);
    }
}

impl From<&str> for Label {
    fn from(text: &str) -> Self {
        Self {
            text: text.to_string(),
            span: None,
        }
    }
}

impl From<String> for Label {
    fn from(text: String) -> Self {
        Self { text, span: None }
    }
}

impl From<Label> for String {
    fn from(label: Label) -> Self {
        label.text
    }
}
