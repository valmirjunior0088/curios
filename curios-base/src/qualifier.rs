//! `Qualifier` is a canonical, resolved identity: a sequence of module segments
//! rooted at the module root. It is what the resolution tables key on, and
//! what `curios-core`'s `Structure`/`Context`/`Definition` use to track a
//! binding's declaring/use-site module without re-deriving structure from a
//! flattened string. Lives here (not `curios-core`, where it originated) as a
//! foundational value type with no dependency of its own on the rest of the
//! core calculus — `curios-base` is the shared leaf every pipeline crate
//! already depends on.

/// A resolved module path: the segment sequence from the module root (see the module docs above for why it lives in this crate). The empty qualifier *is* the root, not a degenerate case.
#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
pub struct Qualifier {
    segments: Vec<String>,
}

impl Qualifier {
    /// The root qualifier — no segments. The identity for `with`, and a legitimate value (e.g. `Context::island` for items of the entry module), not an error state.
    pub fn empty() -> Self {
        Self { segments: vec![] }
    }

    /// This qualifier extended by one child `segment` — descending one module level.
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

    /// The canonical flattened spelling — `/`-joined with a leading `/`, the empty string for the root — which is the exact string definition keys and hand-built references use, so it must match character-for-character.
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

    /// Whether this is exactly one segment — a root-level name, whose `head` and `last` coincide.
    pub fn is_single(&self) -> bool {
        self.segments.len() == 1
    }

    /// The leading (root) segment. Panics on the empty qualifier — use [`Qualifier::root_segment`] where the root qualifier is a legitimate value.
    pub fn head(&self) -> &str {
        &self.segments[0]
    }

    /// The final segment — a binding's own name, with [`Qualifier::without_last`] as its declaring module. Panics on the empty qualifier.
    pub fn last(&self) -> &str {
        self.segments.last().unwrap()
    }

    /// The segments in order, as `&str`.
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.segments.iter().map(String::as_str)
    }

    /// The raw segment list.
    pub fn segments(&self) -> &[String] {
        &self.segments
    }

    /// The qualifier prefix — everything but the last segment — the
    /// declaring/use-site module a binding belongs to. `[a, b, c]` → `[a, b]`;
    /// a single-segment or already-empty qualifier drops to empty.
    pub fn without_last(&self) -> Qualifier {
        Qualifier {
            segments: self.segments[..self.segments.len().saturating_sub(1)].to_vec(),
        }
    }

    /// The qualifier suffix — everything but the leading (root) segment —
    /// a root's own qualifier for content nested under it. `[a, b, c]` →
    /// `[b, c]`; a single-segment or already-empty qualifier drops to empty.
    pub fn without_first(&self) -> Qualifier {
        Qualifier {
            segments: self.segments.iter().skip(1).cloned().collect(),
        }
    }

    /// The first segment, or `""` if empty. Distinct from `head`, which
    /// indexes unchecked — this is for values (like `Context::island`) that
    /// can legitimately be the empty (root) qualifier.
    pub fn root_segment(&self) -> &str {
        self.segments.first().map(String::as_str).unwrap_or("")
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
