//! `Qualifier` is a canonical, resolved identity: a sequence of module segments rooted at the module root. It is what the resolution tables key on, and what `curios-elab`'s `Structure`/`Context`/`Definition` use to track a binding's declaring/use-site module without re-deriving structure from a flattened string. Lives here (not `curios-elab`, where it originated) as a foundational value type with no dependency of its own on the rest of the core calculus — `curios-utilities` is the shared leaf every pipeline crate already depends on.
//!
//! The sharing below is load-bearing and was measured: when `curios-elab`'s names were retyped from `String` to structured identities, an owned `Vec<String>` per qualifier made whole-program compilation markedly slower than the strings it replaced, and `Rc` sharing with a pointer-identity fast path brought it below the original. A memoized structural hash on top of the sharing was then written, measured, and removed: it was indistinguishable from the uncached version, while its `OnceCell` made `Qualifier` interior-mutable and tripped Clippy's `mutable_key_type` at 79 sites across two crates. Do not add one back without a measurement that says otherwise — the sharing was the entire win.
//!
//! **What a segment may spell lives here too**, as [`is_identifier`] and [`is_keyword`], rather than inside the lexer that used to hold the two lists. A segment's legality is a property of the identity, not of one stage's reading of it: `curios-text` refuses a keyword when it parses a path, and `curios-package` refuses one when it parses the name a package declares for itself — a name that becomes a mount prefix, and a prefix nothing can write is unreachable. One list below both is what keeps those two refusals the same refusal.
//!
//! Sharing only pays where qualifiers are *created* sharing, which for the fixed prelude is the archive: reading it back with `rkyv::with::Unshare` gave every occurrence its own allocation, so no two restored names could share a pointer and the fast path never fired for any of them. [`Interned`] reads them back through one canonical allocation per distinct path instead. Measured on `programs/hello_world.crs` in release as a small, real win across restore and erasure, with the archive bytes identical; the restore half is retaken by `curios-prelude-archive`'s `stored_prelude_measurements`, which is where a figure for it belongs. Small because the prelude's dominant cost is re-deriving what the archive could have carried, not allocating its names.

#[cfg(test)]
mod tests;

use std::{
    cmp::Ordering,
    fmt,
    hash::{Hash, Hasher},
    rc::Rc,
};

/// The characters an identifier may spell beyond the alphanumerics.
const CHARACTERS: &[char] = &['_'];

/// The words the surface grammar claims for itself, which therefore cannot spell a segment.
const KEYWORDS: &[&str] = &[
    "let", "match", "choose", "rec", "mod", "use", "pub", "end", "false", "true", "induct",
    "struct", "foreign",
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
/// The segments are shared behind an `Rc`, so cloning a qualifier is a refcount bump rather than one allocation per segment. That matters because a qualifier is copied and compared far more often than it is built: every free variable in every Core term names one, and the free-variable set memoized on every node is keyed by them, so an owned clone put the cost on the kernel's hottest structure. Sharing also gives equality and ordering a pointer-identity fast path, which fires whenever two occurrences came from the same resolution — the common case, since references resolve through one table entry.
#[derive(Clone)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Qualifier {
    /// Archived as the bare segment sequence, exactly as `rkyv::with::Unshare` would write it: putting the `Rc` in the archive would drag rkyv's `Sharing`/`Pooling` bounds into every container that holds a qualifier, and the archived form stays what it has always been, so its derived comparisons agree with the live ones by construction.
    ///
    /// It is read back *interned* rather than unshared — see [`Interned`].
    #[archived_with(Interned)]
    segments: Rc<Vec<String>>,
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
            segments: Rc::new(segments),
        }
    }

    fn segments_slice(&self) -> &[String] {
        &self.segments
    }
}

/// Identity is the segment sequence. The pointer is only ever a way to reach that answer sooner, never a different answer: two qualifiers with equal segments are equal whether or not they share an allocation.
impl PartialEq for Qualifier {
    fn eq(&self, other: &Self) -> bool {
        Rc::ptr_eq(&self.segments, &other.segments) || self.segments == other.segments
    }
}

impl Eq for Qualifier {}

impl Ord for Qualifier {
    fn cmp(&self, other: &Self) -> Ordering {
        match Rc::ptr_eq(&self.segments, &other.segments) {
            true => Ordering::Equal,
            false => self.segments.cmp(&other.segments),
        }
    }
}

impl PartialOrd for Qualifier {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Hashes the segments, not the pointer: sharing is an optimization, so two equal qualifiers must hash alike whether or not they share an allocation.
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
        // A canonical resolved identity is absolute: it carries a leading `/` so a hand-built reference (e.g. the string-literal meta-emitter's `/syn/Str/…`) matches a definition's key unambiguously. The empty (root) qualifier joins to the empty string, not a bare `/`.
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
    pub fn segments(&self) -> &[String] {
        self.segments_slice()
    }

    /// The qualifier prefix — everything but the last segment — the declaring/use-site module a binding belongs to. `[a, b, c]` → `[a, b]`; a single-segment or already-empty qualifier drops to empty.
    pub fn without_last(&self) -> Qualifier {
        let segments = self.segments_slice();
        Self::of(segments[..segments.len().saturating_sub(1)].to_vec())
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

/// Reads an archived qualifier back into the *shared* segment list every other occurrence of the same path already uses, instead of giving each occurrence its own allocation the way `rkyv::with::Unshare` does.
///
/// The archived bytes are identical either way — this wrapper resolves and serializes exactly as `Unshare` does, and differs only on the way in — so switching to it is not an archive format change.
///
/// It matters because the archive is where qualifiers are overwhelmingly *created*: the fixed prelude's Core carries on the order of a hundred thousand qualifier occurrences drawn from a couple of thousand distinct paths, so unsharing them allocates the same few paths tens of thousands of times over. Interning collapses that to one allocation per distinct path, and restores the pointer-identity fast path in [`Qualifier`]'s `PartialEq` and `Ord` for every restored name — which unsharing defeats by construction, since no two occurrences can ever share a pointer.
#[cfg(feature = "archive")]
pub struct Interning;

/// The field adapter: `#[archived_with(Interned)]`.
#[cfg(feature = "archive")]
pub type Interned = curios_archive::Via<Interning>;

#[cfg(feature = "archive")]
mod interned {
    use {
        super::Rc,
        curios_archive::Proxy,
        std::{borrow::Borrow, cell::RefCell, collections::HashSet},
    };

    thread_local! {
        /// One canonical allocation per distinct path, per thread.
        ///
        /// Thread-local rather than global because `Rc` is not `Send`, and the restored prelude is already a thread-local value. It is only ever added to, which is bounded: the entries are the distinct module paths of whatever archives this thread reads, and the fixed prelude is read once.
        static PATHS: RefCell<HashSet<Rc<Vec<String>>>> = RefCell::new(HashSet::new());
    }

    /// The shared allocation for `segments`, adopting it if this path is new.
    fn intern(segments: Vec<String>) -> Rc<Vec<String>> {
        PATHS.with_borrow_mut(|paths| match paths.get(&segments) {
            Some(shared) => Rc::clone(shared),
            None => {
                let shared = Rc::new(segments);
                paths.insert(Rc::clone(&shared));
                shared
            }
        })
    }

    impl Proxy<Rc<Vec<String>>> for super::Interning {
        type Archivable = Vec<String>;

        /// Borrowed, never cloned: the archived form is the `Vec` already inside the `Rc`, so serializing one occurrence costs nothing beyond writing it.
        fn to_archivable(path: &Rc<Vec<String>>) -> impl Borrow<Vec<String>> {
            path.as_ref()
        }

        /// The interning happens here, on the way in — which is the direction that matters, since this is where a hundred thousand occurrences become a couple of thousand allocations.
        fn from_archivable(segments: Vec<String>) -> Result<Rc<Vec<String>>, String> {
            Ok(intern(segments))
        }
    }
}
