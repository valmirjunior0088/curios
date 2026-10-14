//! Where a term was written, as the declaration that wrote it places it.
//!
//! A term's position is an [`Extent`]: two offsets into the text of the source declaration that wrote it, its [`Anchor`], with a stamp of that text. It names no file and no line, so a declaration an edit moves lowers, elaborates and stores to what it did, and two texts of a file share every declaration the edit did not touch.
//!
//! **Where an anchor's text lies is the lowering's to say**, for the text it lowered: [`Anchors`] holds, for each text of each anchor of a unit, the source it lies in, where it begins and ends there, and its stamp. An extent is resolved against it where a report is made ([`Anchors::resolve`]), and a report is the only reader a position has.
//!
//! **A definition is placed by its name where nothing in it is.** A refusal about a declaration as a whole holds no position: a cycle, a signature the lowering built around what was written, an obligation decided over its final terms. The table holds where each definition's name was written ([`Anchors::label`]), so such a report still lands on the declaration it is about.
//!
//! **A declaration is anchored by its name, and one without a name by its module.** A `satisfy` declaration is anonymous, and the name it is known by is read off its signature once every item is lowered, so its terms are written under the anchor its module's unnamed declarations share and told apart by their texts' stamps: a text is found by its anchor and its stamp together.
//!
//! **An extent of a text that has since changed resolves to the declaration.** A term may outlive the text it was written in: a declaration a recompile keeps, a signature another declaration's term was instantiated from. Its extent then carries the stamp of the earlier text, which no text of its anchor answers to, and it resolves to where the declaration now stands rather than to offsets that no longer lie on what they were taken from — where the anchor has one text, which a named declaration's does. Otherwise it resolves to nothing, as an anchor no table holds does, and its report goes unplaced.
//!
//! **An anchor is a copyable identity**, interned once per process as a path is ([`curios_utilities::Qualifier`]) and for its reason: every occurrence of every term a lowering builds holds one. The table is bounded by the declarations and the modules a process names.

#[cfg(test)]
mod tests;

use {
    crate::Global,
    curios_utilities::{Qualifier, Source, Span, Table, intern},
    std::{
        cell::RefCell,
        cmp::Ordering,
        collections::{BTreeMap, HashMap},
        fmt,
        hash::{DefaultHasher, Hash, Hasher},
        ptr,
        sync::{Arc, LazyLock, Mutex},
    },
};

/// What an anchor is the text of.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub enum Anchored {
    /// A source declaration, by the path of the first name it declares: a `let`, a group, a type with the constructors and methods it lowers to, a test.
    Declaration(Qualifier),
    /// The declarations a module writes under no name, its `satisfy` items, each a text of this one anchor.
    Unnamed(Qualifier),
    /// The term a program closes with.
    Entry,
}

/// The text an [`Extent`] is measured in: one source declaration's, one of a module's unnamed ones, or the entry's.
///
/// **A reference to the one allocation of what it anchors**, so a copy is a pointer copy and equality the pointer's.
#[derive(Clone, Copy)]
#[curios_archive::archived(derive(PartialEq, Eq, PartialOrd, Ord, Hash))]
pub struct Anchor {
    /// Held behind a shared pointer for the archive's sake: every position of a declaration names its anchor, and a shared pointer is written once for an image however many times it is held, where what it anchors would be written at each. Archived through that pointer, so the archived comparisons agree with the live ones by construction, and read back through the process table — see [`InternedAnchor`].
    #[archived_with(InternedAnchor)]
    anchored: &'static Arc<Anchored>,
}

/// Every anchor this process has named. See `curios_utilities`' interner for why nothing is freed.
static ANCHORS: LazyLock<Table<Anchored, Arc<Anchored>>> =
    LazyLock::new(|| Mutex::new(HashMap::new()));

impl Anchor {
    /// The anchor of `anchored`'s text.
    pub fn of(anchored: Anchored) -> Self {
        Self {
            anchored: intern(
                &ANCHORS,
                &anchored,
                || Arc::new(anchored),
                |shared| &**shared,
            ),
        }
    }

    /// What this is the text of.
    pub fn anchored(&self) -> &'static Anchored {
        self.anchored
    }
}

/// Identity is what is anchored, and interning makes the allocation answer for it.
impl PartialEq for Anchor {
    fn eq(&self, other: &Self) -> bool {
        ptr::eq(self.anchored, other.anchored)
    }
}

impl Eq for Anchor {}

/// Ordered by what is anchored, never by the address, so a table of anchors comes out the same in every process.
impl Ord for Anchor {
    fn cmp(&self, other: &Self) -> Ordering {
        match ptr::eq(self.anchored, other.anchored) {
            true => Ordering::Equal,
            false => self.anchored.cmp(other.anchored),
        }
    }
}

impl PartialOrd for Anchor {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

/// Hashes what is anchored, not the address, so a hash is the same in every process.
impl Hash for Anchor {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.anchored.hash(state);
    }
}

/// The sharing is noise; an anchor prints as what it anchors.
impl fmt::Debug for Anchor {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        formatter
            .debug_tuple("Anchor")
            .field(self.anchored)
            .finish()
    }
}

/// Reads an archived anchor back into the process table every other occurrence of it already uses.
pub struct InterningAnchor;

/// The field adapter: `#[archived_with(InternedAnchor)]`.
pub type InternedAnchor = curios_archive::Via<InterningAnchor>;

impl curios_archive::Proxy<&'static Arc<Anchored>> for InterningAnchor {
    type Archivable = Arc<Anchored>;

    /// Borrowed, never cloned: the archived form is the pointer the table already holds, which is what lets an image share it.
    fn to_archivable(anchored: &&'static Arc<Anchored>) -> impl std::borrow::Borrow<Arc<Anchored>> {
        *anchored
    }

    /// Through the process table, once for each run of positions that name one pointer: an image holds one pointer for each anchor, and a declaration's positions lie together in it, so most reads are of the pointer read last (`READ_LAST`).
    fn from_archivable(anchored: Arc<Anchored>) -> Result<&'static Arc<Anchored>, String> {
        READ_LAST.with(|last| {
            let mut last = last.borrow_mut();
            if let Some((read, interned)) = &*last
                && Arc::ptr_eq(read, &anchored)
            {
                return Ok(*interned);
            }

            let interned = Anchor::of(*anchored).anchored;
            *last = Some((anchored, interned));

            Ok(interned)
        })
    }
}

thread_local! {
    /// The archived pointer this thread read last, with the anchor it came back as. One entry, which holds its pointer: no other allocation takes that address while it is remembered, so the address answers for what it points at.
    static READ_LAST: RefCell<Option<(Arc<Anchored>, &'static Arc<Anchored>)>> =
        const { RefCell::new(None) };
}

/// A half-open range `[start, end)` of one text of an anchor, with the stamp of that text. `Copy`, and holds no source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[curios_archive::archived]
pub struct Extent {
    pub anchor: Anchor,
    /// Bytes from where the anchor's text begins.
    pub start: u32,
    pub end: u32,
    /// The stamp of the text the extent was taken in: what finds that text among its anchor's, and tells it apart from the one the anchor holds once it has changed.
    pub stamp: u32,
}

impl Extent {
    /// The span this extent lies on, by the first of `anchors` that holds its anchor: a unit's own table, then those of the units in its scope, whose anchors are disjoint as their names are.
    pub fn resolved(&self, anchors: &[&Anchors]) -> Option<Span> {
        anchors.iter().find_map(|anchors| anchors.resolve(self))
    }
}

/// The stamp of one text of an anchor. A function of the text alone, and compared with the stamps of the same anchor's texts and no other, which is why thirty-two bits do: every position holds one, and a stamp that met another's by chance would place a term kept past its text inside its own declaration.
fn stamp(text: &str) -> u32 {
    let mut hasher = DefaultHasher::new();
    text.hash(&mut hasher);
    let hash = hasher.finish();

    u32::try_from((hash ^ (hash >> 32)) & u64::from(u32::MAX))
        .expect("a value masked to thirty-two bits fits them")
}

/// Where one text of an anchor lies.
#[derive(Debug, Clone)]
#[curios_archive::archived]
struct Sited {
    source: Arc<Source>,
    start: usize,
    end: usize,
}

/// Where each text of each anchor of one unit lies, and where each of the unit's definitions is named: what an [`Extent`] is resolved against, and where a report about a definition is placed when it holds no position of its own.
#[derive(Debug, Clone, Default)]
#[curios_archive::archived]
pub struct Anchors {
    /// By anchor and then by stamp, so an anchor's texts are neighbours, and ordered by what is anchored, so a stored table is the same bytes whichever process wrote it.
    texts: BTreeMap<(Anchor, u32), Sited>,
    /// Where each definition's name was written, for the ones a name was written for: a constructor's tag and a method's label as much as a `let`'s, the head of a `satisfy` for the witness it declares.
    labels: BTreeMap<Global, Extent>,
}

impl Anchors {
    /// Record that `name` was written at `label`.
    pub fn name(&mut self, name: Global, label: Extent) {
        self.labels.insert(name, label);
    }

    /// The span `name` was written at, by the first of `anchors` that names it: a unit's own table, then those of the units in its scope. `None` for a definition no name was written for, and for one whose text has since changed past resolving.
    pub fn label(anchors: &[&Anchors], name: &Global) -> Option<Span> {
        anchors
            .iter()
            .find_map(|anchors| anchors.labels.get(name))?
            .resolved(anchors)
    }

    /// The text `name`'s declaration was written in, and where its name stands in it, each as a span of the source the text was read as. `None` for a definition no name was written for, and for one whose text this table no longer holds.
    pub fn written(&self, name: &Global) -> Option<(Span, Span)> {
        let label = self.labels.get(name)?;
        let sited = self.texts.get(&(label.anchor, label.stamp))?;
        let (start, end) = (label.start as usize, label.end as usize);

        (start <= end && sited.start + end <= sited.end).then(|| {
            (
                Span::new(sited.source.clone(), sited.start, sited.end),
                Span::new(sited.source.clone(), sited.start + start, sited.start + end),
            )
        })
    }

    /// Every definition a name was written for, with where it was written in its declaration's text, in the names' order: what says which definitions a text declares, a label holding its text's anchor and stamp.
    pub fn labels(&self) -> impl Iterator<Item = (&Global, &Extent)> {
        self.labels.iter()
    }

    /// Every text this table holds, by its anchor and stamp, with the span it lies on: in the table's order, which is no source's.
    pub fn texts(&self) -> impl Iterator<Item = ((Anchor, u32), Span)> {
        self.texts.iter().map(|(key, sited)| {
            (
                *key,
                Span::new(sited.source.clone(), sited.start, sited.end),
            )
        })
    }

    /// Record that `span` covers a text of `anchor`, and hand back that text as a lowering takes extents within it. A text recorded again lies where it was recorded last.
    pub fn site(&mut self, anchor: Anchor, span: &Span) -> Within {
        let stamp = stamp(&span.source.text[span.start..span.end]);
        let sited = Sited {
            source: span.source.clone(),
            start: span.start,
            end: span.end,
        };
        self.texts.insert((anchor, stamp), sited.clone());

        Within {
            anchor,
            stamp,
            sited,
        }
    }

    /// The text of `anchor` that carries `stamp`, where this table holds one: what a declaration lowered from it is known again by, since a stamp narrows and only the text decides.
    pub fn text(&self, anchor: Anchor, stamp: u32) -> Option<&str> {
        let sited = self.texts.get(&(anchor, stamp))?;

        Some(&sited.source.text[sited.start..sited.end])
    }

    /// The span `extent` lies on: its range in the text it was taken from, where this table holds that text; the whole of the anchor's text where that text is gone and the anchor has one; and nothing otherwise.
    pub fn resolve(&self, extent: &Extent) -> Option<Span> {
        let (start, end) = (extent.start as usize, extent.end as usize);
        if let Some(sited) = self.texts.get(&(extent.anchor, extent.stamp))
            && start <= end
            && sited.start + end <= sited.end
        {
            return Some(Span::new(
                sited.source.clone(),
                sited.start + start,
                sited.start + end,
            ));
        }

        let mut texts = self
            .texts
            .range((extent.anchor, u32::MIN)..=(extent.anchor, u32::MAX));
        match (texts.next(), texts.next()) {
            (Some((_, sited)), None) => {
                Some(Span::new(sited.source.clone(), sited.start, sited.end))
            }
            _ => None,
        }
    }
}

/// One text of an anchor, as a lowering takes the extent of a span that lies in it.
#[derive(Debug, Clone)]
pub struct Within {
    anchor: Anchor,
    stamp: u32,
    sited: Sited,
}

impl Within {
    /// The anchor this is a text of, and the text's stamp: what the table it was sited in holds it under.
    pub fn key(&self) -> (Anchor, u32) {
        (self.anchor, self.stamp)
    }

    /// The text itself.
    pub fn text(&self) -> &str {
        &self.sited.source.text[self.sited.start..self.sited.end]
    }

    /// The extent of `span` in this text. `None` for a span that does not lie in it: one of another source, or one reaching outside the text.
    pub fn extent(&self, span: &Span) -> Option<Extent> {
        let sited = &self.sited;
        if !Arc::ptr_eq(&sited.source, &span.source)
            || span.start < sited.start
            || span.end < span.start
            || sited.end < span.end
        {
            return None;
        }

        Some(Extent {
            anchor: self.anchor,
            start: u32::try_from(span.start - sited.start).ok()?,
            end: u32::try_from(span.end - sited.start).ok()?,
            stamp: self.stamp,
        })
    }
}
