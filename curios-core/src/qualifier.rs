//! `Qualifier` is a canonical, resolved identity: a sequence of module segments
//! rooted at the module root. It is what the resolution tables key on, and
//! what `core`'s `Structure`/`Context`/`Definition` use to track a binding's
//! declaring/use-site module without re-deriving structure from a flattened
//! string. Lives in `curios-core` (not `curios-text`, where it originated)
//! because `curios-text` already depends on `curios-core` — not the other way
//! around — so this is the shared home, reused verbatim by both crates.

use curios_abi::RootId;

#[derive(Debug, Clone, Default, PartialEq, Eq, Hash)]
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

    /// The `RootId` this qualifier's leading segment names — the entry
    /// program for the empty (root) qualifier, same as for any other
    /// unrecognized segment (`RootId::of_segment`'s fallback).
    pub fn root_id(&self) -> RootId {
        RootId::of_segment(self.root_segment())
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
