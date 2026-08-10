//! What the fold consults before compiling a unit again.
//!
//! The store's layout and its keys belong to `curios-package`; reading and writing a `Unit` through them belongs here, because this is the crate that already has both the driver and the archiving machinery. Keeping the implementation out of `curios-package` keeps that crate — which `curios new` also uses — free of the whole compiler.
//!
//! **Taking a unit from here is believing a verdict this compiler reached earlier.** That is a change to what the compiler believes rather than a faster way to do what it already did, and the argument for it is in [SOUNDNESS.md](../../documentation/SOUNDNESS.md) under *Cached verdicts*. Everything below is the mechanism the argument is about.

use {
    curios_archive::rkyv,
    curios_package::{compiler, terms, unit, unit_key},
    curios_pipeline::Cache,
    curios_text::UnitSource,
    curios_unit::Unit,
    std::{cell::RefCell, fs, path::PathBuf},
};

/// The file a stored unit is written as, inside its keyed directory.
const STORED: &str = "unit.rkyv";

/// The store, as the fold sees it.
pub struct Verdicts {
    root: PathBuf,
    /// `None` when the compiler cannot identify itself — in which case nothing is read and nothing is written, because a verdict recorded under an identity nobody can reproduce would later be believed on behalf of a different compiler.
    compiler: Option<String>,
    /// The keys placed so far, in fold order. A unit's key covers them, because its lowering copies their cumulative universe-seed table — so the same source after a different prefix is a different unit.
    placed: RefCell<Vec<String>>,
}

impl Verdicts {
    /// The store beside `root`.
    pub fn at(root: PathBuf) -> Self {
        Self {
            compiler: compiler(&root),
            root,
            placed: RefCell::new(Vec::new()),
        }
    }

    /// What `source` would be filed under, or `None` when it may not be filed at all.
    ///
    /// A source with nothing on disk is uncacheable by content: there are no terms to hash, so there is no key that could distinguish it from any other. That is the fixed prelude, which has an archive of its own.
    fn key(&self, source: &UnitSource<'_>) -> Option<String> {
        let compiler = self.compiler.as_ref()?;

        let directories = source.directories();
        if directories.is_empty() {
            return None;
        }

        Some(unit_key(
            compiler,
            &self.placed.borrow(),
            &terms(&directories)?,
        ))
    }
}

impl Cache for Verdicts {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let key = self.key(source)?;
        let bytes = fs::read(unit(&self.root, &key).join(STORED)).ok()?;

        // A stored unit that will not read back is a store to ignore, never a compile to fail: the source it was made from is still there, and recompiling costs time rather than correctness.
        let restored = rkyv::from_bytes::<Unit, rkyv::rancor::Error>(&bytes).ok()?;

        self.placed.borrow_mut().push(key);

        Some(restored)
    }

    fn put(&self, source: &UnitSource<'_>, unit_: &Unit) {
        let Some(key) = self.key(source) else {
            return;
        };

        let directory = unit(&self.root, &key);
        let written = rkyv::to_bytes::<rkyv::rancor::Error>(unit_)
            .ok()
            .and_then(|bytes| {
                fs::create_dir_all(&directory).ok()?;
                fs::write(directory.join(STORED), &bytes).ok()
            });

        // Best effort throughout: a store that cannot be written costs the next compilation the work it would have saved, and nothing else. What it must never do is cost the verdict.
        let _ = written;

        self.placed.borrow_mut().push(key);
    }
}
