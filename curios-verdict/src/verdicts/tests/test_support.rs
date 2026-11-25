//! What the suites of `verdicts` share: projects compiled for real, twice, and the caches that watch what a fold reuses.

use {
    crate::verdicts::*,
    curios_common::{fill, test_support::*},
    curios_module::{PackageRoot, open_entrypoint},
    curios_package::{Cabinet, Governing, order},
    curios_pipeline::{Fold, Progress},
    std::{cell::RefCell, path::Path},
};

/// The entry every project here compiles: it uses the dependency, so the dependency is a unit of the compilation.
pub const ENTRY: &str = "use /std/{Fmt};\nuse /shape/{message};\n\nFmt/print(\"%\\n\")(message)\n";

/// The cabinet the project at `root` files into where nothing names another: its own.
pub fn own(root: &Path) -> Cabinet {
    Cabinet::of(root, None)
}

/// One cabinet `shared` names, which two projects file into as two projects under one named cabinet do.
pub fn shared(shared: &Path) -> Cabinet {
    Cabinet::of(shared, Some(shared))
}

/// Whether the one mounted unit of a compilation came from the cabinet.
pub fn reused(root: &Path) -> bool {
    reused_in(root, &own(root))
}

/// The same, filing into `cabinet`.
pub fn reused_in(root: &Path, cabinet: &Cabinet) -> bool {
    reused_from(root, &root.join("shape"), cabinet)
}

/// The same, for a project whose dependency is mounted from somewhere other than beside it.
pub fn reused_from(root: &Path, shape: &Path, cabinet: &Cabinet) -> bool {
    let verdicts = Verdicts::at(cabinet.clone());

    reused_through(root, shape, &verdicts)
}

/// A question's reading of the cabinet, as far as the cabinet's own pieces go: verified through an overlay, taking a disagreeing object as a baseline, and keeping what it compiled without filing it, as the `wonder` engine does with a unit the disk does not answer for.
pub struct Overlaid<'a>(pub &'a Verdicts, pub &'a Overlay);

impl Cache for Overlaid<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        self.0.get_overlaid(source, self.1)
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        self.0.earlier(source).or_else(|| {
            offered.cloned().map(|unit| Earlier {
                unit,
                after: After::Scope,
                candidates: None,
            })
        })
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        self.0.keep(source, unit);
    }
}

pub fn reused_through(root: &Path, shape: &Path, cache: &dyn Cache) -> bool {
    folded_through(root, shape, cache, Compiled::Built).contains(&"reused /shape".to_string())
}

/// What the fold did to the one mounted unit, compiled for `compiled`, through `cache`: read off the progress events, as every test here reads its answer.
pub fn folded_through(
    root: &Path,
    shape: &Path,
    cache: &dyn Cache,
    compiled: Compiled,
) -> Vec<String> {
    let governing = Governing::of(shape, None).expect("a governed package");
    let library = order(&governing)
        .expect("a resolvable library")
        .into_iter()
        .map(|unit| match compiled {
            Compiled::Built => unit,
            Compiled::Tested => unit.tested(),
        })
        .collect::<Vec<_>>();
    let (entrypoint, loader, _source) =
        open_entrypoint(&root.join("exe.crs"), None).expect("an openable entrypoint");

    let mut events = Vec::new();
    Fold::new(1_000_000, &library, Some(cache))
        .compile(
            &entrypoint,
            &loader,
            |_| {},
            |progress| match progress {
                Progress::Compiling { unit: prefix, .. } => {
                    events.push(format!("compiling {}", prefix.join()))
                }
                Progress::Recompiling { unit: prefix, .. } => {
                    events.push(format!("recompiling {}", prefix.join()));
                }
                Progress::Reused(prefix) => events.push(format!("reused {}", prefix.join())),
                _ => {}
            },
        )
        .expect("a compiling program");

    events
}

/// A directory of its own, in this file's family.
pub fn temporary(name: &str) -> Temporary {
    Temporary::new("cache", name)
}

/// A project with one dependency and an entry that uses it, at a directory of its own.
pub fn project(name: &str) -> Temporary {
    let root = temporary(name);

    write(
        &root,
        "shape/curios.toml",
        concat!(
            "compiler = \"",
            env!("CARGO_PKG_VERSION"),
            "\"\nname = \"shape\"\n"
        ),
    );
    write(&root, "shape/lib.crs", &library("first"));
    write(&root, "exe.crs", ENTRY);

    root
}

/// The dependency's library, saying `word`.
pub fn library(word: &str) -> String {
    fill!(
        "use /std/{Str};\n\npub let message: Str =\n    \"${word}\";\n",
        word
    )
}

pub fn write(root: &Path, path: &str, contents: &str) {
    let path = root.join(path);
    fs::create_dir_all(path.parent().unwrap()).unwrap();
    fs::write(path, contents).unwrap();
}

/// The bytes of every object's unit file the cabinet at `root` holds.
pub fn filed(root: &Path) -> Vec<Vec<u8>> {
    let mut filed = own(root)
        .unit_files()
        .into_iter()
        .map(|unit| fs::read(unit).unwrap())
        .collect::<Vec<_>>();
    filed.sort();

    filed
}

/// An entry with a declaration of its own, so the unit of its declarations holds something.
pub const DECLARING_ENTRY: &str = "use /std/{Fmt, Str};\nuse /shape/{message};\n\nlet twice: Str = Str/concat(message, message);\n\nFmt/print(\"%\\n\")(twice)\n";

/// The cabinet, saying what a fold did with the unit of its entry's own declarations: `reused` where the cabinet handed it back, `recompiled` where it was compiled over a baseline, `compiled` otherwise.
pub struct Watching<'a> {
    pub verdicts: &'a Verdicts,
    pub entry: RefCell<Option<&'static str>>,
}

impl Cache for Watching<'_> {
    fn get(&self, source: &UnitSource<'_>) -> Option<Unit> {
        let hit = Cache::get(self.verdicts, source);
        if source.is_entry() {
            *self.entry.borrow_mut() = Some(match hit {
                Some(_) => "reused",
                None => "compiled",
            });
        }

        hit
    }

    fn baseline(&self, source: &UnitSource<'_>, offered: Option<&Unit>) -> Option<Earlier> {
        let earlier = Cache::baseline(self.verdicts, source, offered);
        if source.is_entry() && earlier.is_some() {
            *self.entry.borrow_mut() = Some("recompiled");
        }

        earlier
    }

    fn put(&self, source: &UnitSource<'_>, unit: &Unit) {
        Cache::put(self.verdicts, source, unit);
    }
}

/// What a build's fold did with the unit of the entry `root` holds in `entry`, through a cabinet told the entry is the executable `executable`, or told nothing.
pub fn entered(root: &Path, entry: &str, executable: Option<&str>) -> &'static str {
    let verdicts = Verdicts::at(own(root));
    let verdicts = match executable {
        Some(executable) => verdicts.executing("app", executable),
        None => verdicts,
    };
    let watching = Watching {
        verdicts: &verdicts,
        entry: RefCell::new(None),
    };
    let governing = Governing::of(&root.join("shape"), None).expect("a governed package");
    let library = order(&governing).expect("a resolvable library");
    let (entrypoint, loader, _source) =
        open_entrypoint(&root.join(entry), Some(&PackageRoot::new("app", root)))
            .expect("an openable entrypoint");

    Fold::new(1_000_000, &library, Some(&watching))
        .compile(&entrypoint, &loader, |_| {}, |_| {})
        .expect("a compiling program");

    watching
        .entry
        .take()
        .expect("the entry's unit is asked for")
}

/// The packs of the cabinet at `root`, each by where it is kept: every one under its objects, whichever compiler's and at whatever depth.
pub fn packs(root: &Path) -> Vec<PathBuf> {
    let mut packs = Vec::new();
    let mut unread = vec![own(root).objects()];
    while let Some(directory) = unread.pop() {
        for entry in fs::read_dir(&directory).into_iter().flatten() {
            let path = entry.unwrap().path();
            if path.is_dir() {
                unread.push(path);
            } else if path
                .extension()
                .is_some_and(|extension| extension == "pack")
            {
                packs.push(path);
            }
        }
    }

    packs
}

/// `base`'s `x` as `body` computes it.
pub fn base_library(body: &str) -> String {
    fill!(
        "use /std/{Nat};\n\npub let x(n: Nat) -> Nat = ${body};\n",
        body
    )
}
