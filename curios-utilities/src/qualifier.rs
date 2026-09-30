//! `Qualifier` is a canonical, resolved identity: a sequence of module segments rooted at the module root. It is what the resolution tables key on, and what the elaborator tracks a binding's declaring and use-site module by, without re-deriving structure from a flattened string. It lives in the shared leaf every pipeline crate depends on because it depends on nothing of the core calculus.
//!
//! **A qualifier is a copyable identity.** Its segments are interned once per process — one allocation per distinct path, never freed — and a qualifier is a `&'static` reference to that allocation, so copying one is copying a pointer, two equal qualifiers share it, and one can cross a thread. The archive reads a path back through the same table ([`Interned`]). Why the segments are shared at all, and why no structural hash is cached on top, is `README.md`'s; the restore half of the interning figure is retaken by `curios-prelude-archive`'s `stored_prelude_measurements`, which is where a figure for it belongs.
//!
//! **What a segment may spell lives here too**, as [`is_identifier`] and [`is_keyword`], rather than inside the lexer: `curios-text` refuses a keyword when it parses a path, and `curios-package` refuses one when it parses the name a package declares for itself, and one list below both keeps those two refusals the same refusal — `README.md`'s decision.

#[cfg(test)]
mod tests;

use {
    super::{Table, intern},
    std::{
        cmp::Ordering,
        collections::HashMap,
        fmt,
        hash::{Hash, Hasher},
        ptr,
        sync::{LazyLock, Mutex},
    },
};

/// The characters an identifier may spell beyond the alphanumerics.
const CHARACTERS: &[char] = &['_'];

/// The words the surface grammar claims for itself, which therefore cannot spell a segment.
const KEYWORDS: &[&str] = &[
    "let", "match", "choose", "mod", "use", "pub", "end", "false", "true", "induct", "struct",
    "foreign",
];

/// Whether `character` may appear in an identifier.
pub fn is_identifier_char(character: char) -> bool {
    CHARACTERS.contains(&character) || character.is_alphanumeric()
}

/// Whether `word` spells an identifier: a non-empty run of [`is_identifier_char`].
pub fn is_identifier(word: &str) -> bool {
    !word.is_empty() && word.chars().all(is_identifier_char)
}

/// Whether `word` is a keyword, and so cannot spell a segment.
pub fn is_keyword(word: &str) -> bool {
    KEYWORDS.contains(&word)
}

/// A resolved module path: the segment sequence from the module root (see the module docs above for why it lives in this crate). The empty qualifier *is* the root, not a degenerate case.
///
/// **A reference to the one allocation of its path**, interned once per process. A qualifier is copied and compared far more often than it is built: every free variable in every Core term names one, and the free-variable set memoized on every node is keyed by them, so an owned clone would put the cost on the kernel's hottest structure, and a shared `Rc` would keep every term on one thread. Interning makes a copy a pointer copy and equality a pointer comparison — exact rather than a fast path, since two equal paths are one allocation.
#[derive(Clone, Copy)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Qualifier {
    /// Archived as the bare segment sequence, so its derived comparisons agree with the live ones by construction; read back through the process table — see [`Interned`].
    #[archived_with(Interned)]
    segments: &'static Vec<String>,
}

/// Every path this process has named. See the `interner` module for why nothing is freed.
static PATHS: LazyLock<Table<[String], Vec<String>>> = LazyLock::new(|| Mutex::new(HashMap::new()));

/// The one allocation of the path `segments` spells, copied into the table only when this process has not named the path before.
fn interned(segments: &[String]) -> &'static Vec<String> {
    intern(&PATHS, segments, || segments.to_vec(), Vec::as_slice)
}

/// The default qualifier is the root, which is a legitimate value.
impl Default for Qualifier {
    fn default() -> Self {
        Self::empty()
    }
}

impl Qualifier {
    fn of(segments: Vec<String>) -> Self {
        Self {
            segments: interned(&segments),
        }
    }

    fn segments_slice(&self) -> &'static [String] {
        self.segments.as_slice()
    }
}

/// Identity is the segment sequence, and interning makes the allocation answer for it: two qualifiers with equal segments are one allocation.
impl PartialEq for Qualifier {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.segments, other.segments)
    }
}

impl Eq for Qualifier {}

/// Ordered by segments, never by address, so every ordered map and every sorted report comes out the same in every process; the shared allocation only answers equality sooner.
impl Ord for Qualifier {
    fn cmp(&self, other: &Self) -> Ordering {
        match ptr::eq(self.segments, other.segments) {
            true => Ordering::Equal,
            false => self.segments.cmp(other.segments),
        }
    }
}

impl PartialOrd for Qualifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Hashes the segments, not the address, so a hash is the same in every process — a term's structural hash is taken over the qualifiers it names.
impl Hash for Qualifier {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.segments.hash(state);
    }
}

/// The sharing is noise; a qualifier prints as its segments.
impl fmt::Debug for Qualifier {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_tuple("Qualifier")
            .field(&self.segments)
            .finish()
    }
}

impl Qualifier {
    /// The root qualifier — no segments. The identity for `with`, and a legitimate value (e.g. `Context::island` for items of the entry module), not an error state.
    pub fn empty() -> Self {
        Self::of(Vec::new())
    }

    /// This qualifier extended by one child `segment` — descending one module level.
    pub fn with(&self, segment: &str) -> Self {
        Self::of(
            self.segments_slice()
                .iter()
                .cloned()
                .chain([segment.to_string()])
                .collect(),
        )
    }

    /// The canonical flattened spelling — `/`-joined with a leading `/`, the empty string for the root — which is the exact string definition keys and hand-built references use, so it must match character-for-character.
    pub fn join(&self) -> String {
        // A canonical resolved identity is absolute: it carries a leading `/` so a hand-built reference (e.g. the string-literal meta-emitter's `/std/Str/…`) matches a definition's key unambiguously. The empty (root) qualifier joins to the empty string, not a bare `/`.
        match self.segments_slice().is_empty() {
            true => String::new(),
            false => format!("/{}", self.segments_slice().join("/")),
        }
    }

    /// Whether this *is* the module root — no segments. A predicate rather than an `is_empty` on the segment list, so a caller asking a structural question never has to reach for the text to ask it.
    pub fn is_root(&self) -> bool {
        self.segments.is_empty()
    }

    /// Whether this is exactly one segment — a root-level name, whose `head` and `last` coincide.
    pub fn is_single(&self) -> bool {
        self.segments_slice().len() == 1
    }

    /// The leading (root) segment. Panics on the empty qualifier.
    pub fn head(&self) -> &str {
        &self.segments_slice()[0]
    }

    /// The final segment — a binding's own name, with [`Qualifier::without_last`] as its declaring module. Panics on the empty qualifier.
    pub fn last(&self) -> &str {
        self.segments_slice().last().unwrap()
    }

    /// The segments in order, as `&str`.
    pub fn iter(&self) -> impl Iterator<Item = &str> {
        self.segments_slice().iter().map(String::as_str)
    }

    /// The raw segment list.
    pub fn segments(&self) -> &'static [String] {
        self.segments_slice()
    }

    /// The qualifier prefix — everything but the last segment — the declaring/use-site module a binding belongs to. `[a, b, c]` → `[a, b]`; a single-segment or already-empty qualifier drops to empty.
    pub fn without_last(&self) -> Qualifier {
        let segments = self.segments_slice();
        Self::of(segments[..segments.len().saturating_sub(1)].to_vec())
    }

    /// The line a report adds under an unresolved `written` that this qualifier could have meant: reached by its absolute path or its own import, and — nested below a root — through its parent's name too, `Eq/cong` once `Eq` is in scope; a root's direct child has no route shorter than the import or the absolute path. The `unbound variable` and `unresolved qualifier` reports both spell the way out with this one line, so the two cannot drift.
    pub fn reach_hint(&self, written: &str) -> String {
        let module = self.without_last();
        let import = format!("`use {}/{{{written}}};`", module.join());
        match module.segments().len() {
            0 | 1 => format!(
                "  `{written}` is `{}`: write it absolute, or {import}",
                self.join()
            ),
            _ => format!(
                "  `{written}` is `{}`: write `{parent}/{written}` if `{parent}` is imported, or {import}",
                self.join(),
                parent = module.last()
            ),
        }
    }

    /// The qualifier suffix — everything but the leading (root) segment — a root's own qualifier for content nested under it. `[a, b, c]` → `[b, c]`; a single-segment or already-empty qualifier drops to empty.
    pub fn without_first(&self) -> Qualifier {
        Self::of(self.segments_slice().iter().skip(1).cloned().collect())
    }

    /// Whether this qualifier lies within `ancestor`'s subtree — equal to it, or nested below it at any depth. The comparison is segment-wise, not textual, so `/Foobar` is not within `/Foo`; the empty qualifier is the root, and every qualifier lies within it.
    ///
    /// This is the module system's visibility intrinsic: a declaration written without `pub` in module `M` is visible exactly to the qualifiers within `M`.
    pub fn is_within(&self, ancestor: &Qualifier) -> bool {
        let (here, there) = (self.segments_slice(), ancestor.segments_slice());
        here.len() >= there.len() && here.iter().zip(there).all(|(here, there)| here == there)
    }
}

impl<S, I> From<I> for Qualifier
where
    S: Into<String>,
    I: IntoIterator<Item = S>,
{
    fn from(iter: I) -> Self {
        Self::of(iter.into_iter().map(Into::into).collect())
    }
}

/// Reads an archived qualifier back into the process table every other occurrence of the same path already uses, instead of giving each occurrence its own allocation.
///
/// The archived bytes are the bare segment sequence, so the live representation is no part of the archive format. The fixed prelude's Core carries on the order of a hundred thousand qualifier occurrences drawn from a couple of thousand distinct paths, and reading each back through the table is what keeps that a couple of thousand allocations.
#[cfg(feature = "archive")]
pub struct Interning;

/// The field adapter: `#[archived_with(Interned)]`.
#[cfg(feature = "archive")]
pub type Interned = curios_archive::Via<Interning>;

#[cfg(feature = "archive")]
impl curios_archive::Proxy<&'static Vec<String>> for Interning {
    type Archivable = Vec<String>;

    /// Borrowed, never cloned: the archived form is the `Vec` the table already holds, which is why the table holds a `Vec` rather than a slice.
    fn to_archivable(path: &&'static Vec<String>) -> impl std::borrow::Borrow<Vec<String>> {
        *path
    }

    /// The interning happens here, on the way in — the direction [`Interning`] is for.
    fn from_archivable(segments: Vec<String>) -> Result<&'static Vec<String>, String> {
        Ok(interned(&segments))
    }
}
