//! Writing one row into a manifest: what `curios pin` computes, and the only place a `curios.toml` is written back.
//!
//! **Pinning is the act, and adding is what it does the first time — which is why there is no `add` verb.** A row that names where its subject comes from and does not say what it must hash to is a row nothing can refuse a substitution against, so the hash is computed from the delivery at the moment the row is written; a row that did not exist is simply one with nothing to keep. Nothing here accepts bytes against a pin, it *derives* one — the single operation in this toolchain that trusts what arrived, which is the whole reason `--refresh` is spelled separately and reports before it overwrites.
//!
//! **The document is edited, never regenerated.** `toml_edit` writes back every byte it was not asked to change, so a manifest keeps its comments, its ordering and its formatting; and the write is verified by reading the document back and comparing it key by key against the one that went in, so a row written here cannot have disturbed a row it was not asked about. A regenerating writer would have to be trusted not to; this is checked.

#[cfg(test)]
mod tests;

use {
    crate::{
        FileHash, Governing, MANIFEST, Manifest, Store, TreeHash,
        curate::{deliver, deliver_module},
    },
    std::{
        fs,
        path::{Component, Path, PathBuf},
    },
    toml_edit::{ArrayOfTables, DocumentMut, InlineTable, Item, Table, Value},
};

/// Where a delivery lands before the digest that will name it is known, removed however this ends.
///
/// The removal is what a failed or interrupted delivery needs: the path is not a digest, so nothing would ever read it, but leaving a half-fetched tree in the store is untidy at best. A delivery that succeeds is *renamed away* from here, so the drop finds nothing.
struct Staged(PathBuf);

impl Staged {
    /// Stage a path under `store`, with nothing at it.
    fn new(store: &Store) -> Result<Self, String> {
        let path = store.staging(&std::process::id().to_string());

        let _ = fs::remove_dir_all(&path);
        let _ = fs::remove_file(&path);
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
        }

        Ok(Self(path))
    }

    fn path(&self) -> &Path {
        &self.0
    }

    /// File what was staged at `placed`, which is the digest it was just found to have.
    ///
    /// Another process may have placed the identical bytes meanwhile. Content-addressed means they are the same bytes, so losing the race costs nothing but the work — the same reading `curate` makes of the same situation.
    fn file(&self, placed: &Path) -> Result<(), String> {
        if let Some(parent) = placed.parent() {
            fs::create_dir_all(parent)
                .map_err(|error| format!("failed to create {}: {error}", parent.display()))?;
        }

        match fs::rename(&self.0, placed) {
            Ok(()) => Ok(()),
            Err(_) if placed.exists() => Ok(()),
            Err(error) => Err(format!("failed to place {}: {error}", placed.display())),
        }
    }
}

impl Drop for Staged {
    fn drop(&mut self) {
        let _ = fs::remove_dir_all(&self.0);
        let _ = fs::remove_file(&self.0);
    }
}

/// Which of a manifest's two pinned tables a row belongs to.
///
/// The two differ in what their name *is*, which is why they are one command's two subjects rather than one operation over a table name. A `[[foreign]]` row's `name` is a local handle nothing in Curios source refers to, so it is the user's to choose; a `[dependencies]` key is the package's mount prefix and the only way anything refers to it, so it is the dependency's to declare and the user's only to state. [`Subject::Dependency`] therefore checks the name it was given against the delivery, where [`Subject::Foreign`] has nothing to check it against.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Subject {
    Foreign,
    Dependency,
}

impl Subject {
    /// The subject as a refusal and a report name it.
    fn noun(self) -> &'static str {
        match self {
            Self::Foreign => "foreign module",
            Self::Dependency => "dependency",
        }
    }

    /// The `curios pin` subcommand that reaches it, for a refusal to spell the command that would work.
    fn command(self) -> &'static str {
        match self {
            Self::Foreign => "foreign",
            Self::Dependency => "dependency",
        }
    }
}

/// Where a row is to point after this.
///
/// [`Repoint::Refresh`] and [`Repoint::Revised`] both keep part of what the row already says, which is what makes them edits of an existing row rather than ways of writing one — a row that does not exist yet is refused under either, naming the form that would create it.
#[derive(Debug, Clone)]
pub enum Repoint {
    /// A file or tree the package carries, at a path relative to the manifest.
    Carried(PathBuf),
    /// One fetched from a URL. A dependency states the revision too; a foreign module has none to state.
    Fetched { url: String, rev: Option<String> },
    /// A dependency's existing URL at a new revision.
    Revised(String),
    /// Whatever the row already names, delivered again and re-pinned to what arrives.
    Refresh,
}

impl Repoint {
    /// Whether this form edits a row that must already exist, rather than writing one that need not.
    fn edits_existing(&self) -> bool {
        matches!(self, Self::Revised(_) | Self::Refresh)
    }
}

/// What a delivery turned out to be, reported before a `--refresh` is believed.
///
/// **A report rather than a check, and it says so.** Nothing verified these bytes — deriving a pin is exactly the step where there is no pin to verify against — so what this can offer is a description of what arrived and the reminder that a description is all it is. The export list is deliberately absent: reading it needs a WebAssembly decoder, which sits above this crate, and a claimed export that the module does not have is already refused by name when the program is linked.
#[derive(Debug, Clone)]
pub struct Probe {
    /// What arrived, in bytes.
    pub bytes: u64,
    /// What the bytes look like: a WebAssembly module by its magic, or what could be said instead.
    pub kind: String,
}

impl Probe {
    /// Describe `bytes` by their leading magic, which is as far as this crate can read them.
    fn of(bytes: &[u8]) -> Self {
        // The four bytes and the version word every WebAssembly module opens with. Enough to tell a module from an HTML error page a fetcher saved without being asked, which is the mistake worth catching here.
        let kind = match bytes.starts_with(b"\0asm") {
            true => "a WebAssembly module".to_string(),
            false => {
                "not a WebAssembly module — its first bytes are not the `\\0asm` magic".to_string()
            }
        };

        Self {
            bytes: bytes.len() as u64,
            kind,
        }
    }
}

/// What one `curios pin` settled.
#[derive(Debug)]
pub struct Pinned {
    /// The row's name, as it is keyed in the manifest.
    pub name: String,
    /// Whether the row was there before this.
    pub existed: bool,
    /// Each key this set, with what it said before when it said anything. Empty when the row already said all of it, which is what makes a second identical `pin` write nothing.
    pub fields: Vec<(String, Option<String>, String)>,
    /// Whether the manifest was written. False under `--check`, and false when nothing differed.
    pub wrote: bool,
    /// What a fetched delivery turned out to be, for a report to print before the pin is trusted.
    pub probe: Option<Probe>,
}

impl Pinned {
    /// Whether this would have written, which is what `--check` turns its exit code on.
    pub fn would_write(&self) -> bool {
        !self.fields.is_empty()
    }
}

/// Write what `repoint` says the row `name` should state, answering what changed.
///
/// **Nothing is written when nothing differs**, so pinning a row to what it already says is a no-op that reports one, and `check` turns the whole thing into a question — the delivery still happens, because what a row *should* say cannot be known without it, and only the write is withheld.
pub fn pin(
    governing: &Governing,
    subject: Subject,
    name: &str,
    repoint: &Repoint,
    check: bool,
) -> Result<Pinned, String> {
    let text = fs::read_to_string(&governing.manifest)
        .map_err(|error| format!("failed to read {}: {error}", governing.manifest.display()))?;
    let before = text.parse::<DocumentMut>().map_err(|error| {
        format!(
            "{} is not valid TOML: {error}",
            governing.manifest.display()
        )
    })?;

    let existed = present(&before, subject, name);
    if repoint.edits_existing() && !existed {
        return Err(format!(
            "there is no {} {name:?} in {} to re-pin; `curios pin {} {name} --url <URL>` is what writes one",
            subject.noun(),
            governing.manifest.display(),
            subject.command()
        ));
    }

    let settled = match subject {
        Subject::Foreign => settled_foreign(governing, name, repoint, &before)?,
        Subject::Dependency => settled_dependency(governing, name, repoint, &before)?,
    };

    // What the row says now, against what it would say — so a second identical pin has nothing to write and says so.
    let fields = settled
        .fields
        .iter()
        .filter_map(|(key, value)| {
            let said = stated(&before, subject, name, key);

            (said.as_deref() != Some(value.as_str())).then(|| (key.clone(), said, value.clone()))
        })
        .collect::<Vec<_>>();

    let cleared = settled
        .cleared
        .iter()
        .any(|key| stated(&before, subject, name, key).is_some());

    let pinned = Pinned {
        name: name.to_string(),
        existed,
        fields,
        wrote: false,
        probe: settled.probe,
    };

    if check || (!pinned.would_write() && !cleared && existed) {
        return Ok(pinned);
    }

    let mut after = before.clone();
    {
        let mut row = row_mut(&mut after, subject, name);
        for (key, value) in &settled.fields {
            row.set(key, value);
        }
        for key in &settled.cleared {
            row.clear(key);
        }
    }

    undisturbed(&before, &after, subject, name)?;

    fs::write(&governing.manifest, after.to_string())
        .map_err(|error| format!("failed to write {}: {error}", governing.manifest.display()))?;

    Ok(Pinned {
        wrote: true,
        ..pinned
    })
}

/// Refuse a write that changed anything but the row it was asked about.
///
/// **The write is checked rather than trusted.** `toml_edit` reproduces every byte it was not asked to change, which is why it is the writer here — but "is supposed to" is not evidence, and a manifest is the file a project cannot afford to have quietly rearranged. Comparing the two documents key by key is cheap and makes the guarantee this crate's rather than the library's.
fn undisturbed(
    before: &DocumentMut,
    after: &DocumentMut,
    subject: Subject,
    name: &str,
) -> Result<(), String> {
    let touched = match subject {
        Subject::Foreign => "foreign",
        Subject::Dependency => "dependencies",
    };

    let rendered = |document: &DocumentMut, key: &str| document.get(key).map(ToString::to_string);

    for key in before.iter().map(|(key, _)| key.to_string()).chain(
        after
            .iter()
            .map(|(key, _)| key.to_string())
            .filter(|key| before.get(key).is_none()),
    ) {
        if key != touched && rendered(before, &key) != rendered(after, &key) {
            return Err(format!(
                "writing the {} {name:?} would have changed `{key}`, which it was not asked about; nothing was written",
                subject.noun()
            ));
        }
    }

    // Inside the touched table, every row but this one is the same text it was.
    let others = |document: &DocumentMut| -> Vec<(String, String)> {
        match subject {
            Subject::Foreign => document
                .get("foreign")
                .and_then(Item::as_array_of_tables)
                .map(|tables| {
                    tables
                        .iter()
                        .filter_map(|table| {
                            let keyed = table.get("name")?.as_str()?.to_string();

                            (keyed != name).then(|| (keyed, table.to_string()))
                        })
                        .collect()
                })
                .unwrap_or_default(),
            Subject::Dependency => document
                .get("dependencies")
                .and_then(Item::as_table)
                .map(|table| {
                    table
                        .iter()
                        .filter(|(keyed, _)| *keyed != name)
                        .map(|(keyed, item)| (keyed.to_string(), item.to_string()))
                        .collect()
                })
                .unwrap_or_default(),
        }
    };

    match others(before) == others(after) {
        true => Ok(()),
        false => Err(format!(
            "writing the {} {name:?} would have changed another row in `{touched}`; nothing was written",
            subject.noun()
        )),
    }
}

/// One row, in whichever of TOML's two spellings it was written.
///
/// Both, because a manifest is somebody's file: `json = { source = "git", … }` is what the reference shows and what this writes, and `[dependencies.json]` is the same row written the other way. A writer that understood only its own spelling would silently add a second row beside the one already there.
enum Row<'a> {
    Table(&'a mut Table),
    Inline(&'a mut InlineTable),
}

impl Row<'_> {
    /// Say `value` for `key`.
    fn set(&mut self, key: &str, value: &str) {
        match self {
            Self::Table(table) => {
                table.insert(key, toml_edit::value(value));
            }
            Self::Inline(inline) => {
                inline.insert(key, value.into());
            }
        }
    }

    /// Say nothing for `key`, which is what repointing a row from one delivery to the other has to do with the key it is leaving behind.
    fn clear(&mut self, key: &str) {
        match self {
            Self::Table(table) => {
                table.remove(key);
            }
            Self::Inline(inline) => {
                inline.remove(key);
            }
        }
    }
}

/// A module's path as a row may state it: plain and relative, as an executable's `path` is.
///
/// A module the package *carries* is inside it, so the spelling is checked here rather than by the reader that would refuse it later. A dependency's `path` is held to nothing of the sort — it names a live sibling, and `../shape` is what that looks like.
fn plain(path: &Path, name: &str) -> Result<String, String> {
    match path
        .components()
        .find(|component| !matches!(component, Component::Normal(_)))
    {
        Some(component) => Err(format!(
            "the foreign module {name:?} would be pinned at {}, whose `{}` is no plain relative path; a row names a file inside the package by its path from the manifest, with no `.`, `..` or leading `/`",
            path.display(),
            component.as_os_str().to_string_lossy()
        )),
        None => Ok(path.to_string_lossy().replace('\\', "/")),
    }
}

/// Fetch `url` and file it under the digest it turns out to have, answering that digest and what arrived.
fn fetched_module(store: &Store, url: &str, subject: &str) -> Result<(FileHash, Probe), String> {
    let staged = Staged::new(store)?;

    deliver_module(staged.path(), url, subject)?;

    let bytes = fs::read(staged.path())
        .map_err(|error| format!("failed to read what was fetched from {url}: {error}"))?;
    let hash = FileHash::of_bytes(&bytes);

    staged.file(&store.foreign(&hash))?;

    Ok((hash, Probe::of(&bytes)))
}

/// Fetch `rev` of `url` and file the tree under the digest it turns out to have, answering that digest and the package it declares itself to be.
fn fetched_tree(store: &Store, url: &str, rev: &str) -> Result<(TreeHash, String), String> {
    let staged = Staged::new(store)?;

    fs::create_dir_all(staged.path())
        .map_err(|error| format!("failed to create {}: {error}", staged.path().display()))?;
    deliver(staged.path(), url, rev)?;

    let hash = TreeHash::of(staged.path())?;
    let declared = declares(staged.path())?;

    staged.file(&store.source(&hash))?;

    Ok((hash, declared))
}

/// The name the package at `directory` declares for itself.
///
/// What makes a dependency's positional name checkable at all: a `[dependencies]` key is the package's mount prefix and the only way anything refers to it, so it is the dependency's to declare and the user's only to state. Reading it here means a disagreement is refused before a row is written, rather than by the resolver once one has been.
fn declares(directory: &Path) -> Result<String, String> {
    match Manifest::from_path(&directory.join(MANIFEST))? {
        Manifest::Package(package) => Ok(package.name),
        Manifest::Umbrella(_) => Err(format!(
            "{} declares an umbrella; a dependency is a package",
            directory.display()
        )),
    }
}

/// Read one field of the row `name` keys, without borrowing it mutably.
fn stated(document: &DocumentMut, subject: Subject, name: &str, key: &str) -> Option<String> {
    let item = match subject {
        Subject::Foreign => document
            .get("foreign")?
            .as_array_of_tables()?
            .iter()
            .find(|table| table.get("name").and_then(Item::as_str) == Some(name))?
            .get(key)?
            .as_str()?,
        Subject::Dependency => match document.get("dependencies")?.as_table()?.get(name)? {
            Item::Table(table) => table.get(key)?.as_str()?,
            Item::Value(Value::InlineTable(inline)) => inline.get(key)?.as_str()?,
            _ => return None,
        },
    };

    Some(item.to_string())
}

/// Whether the row `name` keys is in `document` at all.
fn present(document: &DocumentMut, subject: Subject, name: &str) -> bool {
    match subject {
        Subject::Foreign => document
            .get("foreign")
            .and_then(Item::as_array_of_tables)
            .is_some_and(|tables| {
                tables
                    .iter()
                    .any(|table| table.get("name").and_then(Item::as_str) == Some(name))
            }),
        Subject::Dependency => document
            .get("dependencies")
            .and_then(Item::as_table)
            .is_some_and(|table| table.contains_key(name)),
    }
}

/// What a row is to say after this, and what the delivery was.
///
/// The keys a form does not set are the keys it *clears*: repointing a row from a carried module to a fetched one has to remove the `path` it is leaving behind, or the manifest states two deliveries and is refused for it.
struct Settled {
    fields: Vec<(String, String)>,
    cleared: Vec<&'static str>,
    probe: Option<Probe>,
}

/// What a `[[foreign]]` row is to say.
fn settled_foreign(
    governing: &Governing,
    name: &str,
    repoint: &Repoint,
    document: &DocumentMut,
) -> Result<Settled, String> {
    let store = governing.store();
    let carried = |path: &Path| -> Result<Settled, String> {
        let spelled = plain(path, name)?;
        let bytes = fs::read(governing.directory.join(path)).map_err(|error| {
            format!(
                "the foreign module {name:?} is not at {}: {error}",
                governing.directory.join(path).display()
            )
        })?;

        Ok(Settled {
            fields: vec![
                ("path".to_string(), spelled),
                ("hash".to_string(), FileHash::of_bytes(&bytes).to_string()),
            ],
            cleared: vec!["url"],
            probe: None,
        })
    };

    let fetched = |url: &str| -> Result<Settled, String> {
        let (hash, probe) = fetched_module(&store, url, &format!("the foreign module {name:?}"))?;

        Ok(Settled {
            fields: vec![
                ("url".to_string(), url.to_string()),
                ("hash".to_string(), hash.to_string()),
            ],
            cleared: vec!["path"],
            probe: Some(probe),
        })
    };

    match repoint {
        Repoint::Carried(path) => carried(path),
        Repoint::Fetched { url, .. } => fetched(url),
        Repoint::Revised(_) => Err(format!(
            "the foreign module {name:?} has no revision to set: a module is one file, pinned by the hash of its bytes. `curios pin foreign {name} --url <URL>` repoints it, and `--refresh` re-pins what it already names"
        )),
        // Whichever delivery the row already states, performed again. The row has one or the other, because a row stating both is refused where it is read.
        Repoint::Refresh => {
            match (
                stated(document, Subject::Foreign, name, "path"),
                stated(document, Subject::Foreign, name, "url"),
            ) {
                (Some(path), _) => carried(Path::new(&path)),
                (None, Some(url)) => fetched(&url),
                (None, None) => Err(format!(
                    "the foreign module {name:?} states neither `path` nor `url`, so there is nothing to deliver again"
                )),
            }
        }
    }
}

/// What a `[dependencies]` row is to say.
///
/// Every form but the carried one checks the name it was given against the name the delivery declares, which is the check [`Subject`] exists to make.
fn settled_dependency(
    governing: &Governing,
    name: &str,
    repoint: &Repoint,
    document: &DocumentMut,
) -> Result<Settled, String> {
    let store = governing.store();
    let agreed = |declared: &str, at: &str| -> Result<(), String> {
        match declared == name {
            true => Ok(()),
            // The resolver's own sentence, made at the moment a row would be written rather than the moment one is read.
            false => Err(format!(
                "the dependency {name:?} resolves to {at}, which declares itself {declared:?}; a package is referred to by the name it declares"
            )),
        }
    };

    let fetched = |url: &str, rev: &str| -> Result<Settled, String> {
        let (hash, declared) = fetched_tree(&store, url, rev)?;
        agreed(&declared, &format!("{url} at {rev}"))?;

        Ok(Settled {
            fields: vec![
                ("source".to_string(), "git".to_string()),
                ("url".to_string(), url.to_string()),
                ("rev".to_string(), rev.to_string()),
                ("hash".to_string(), hash.to_string()),
            ],
            cleared: vec!["path"],
            probe: None,
        })
    };

    // The existing row's own URL, for the two forms that keep it.
    let url_of = |key: &str| {
        stated(document, Subject::Dependency, name, "url").ok_or_else(|| {
            format!(
                "the dependency {name:?} states no `url`, so there is no repository for `--{key}` to deliver from; only a `source = \"git\"` row is pinned to a revision"
            )
        })
    };

    match repoint {
        // Not held to a plain relative path, unlike a module: a module is inside this package, while a `source = "path"` dependency reaches a separate project on disk — so `../shape` is exactly what it is for. A package of this project's own is a `member` of its umbrella, which is a marker row and nothing this writes.
        Repoint::Carried(path) => {
            let spelled = path.to_string_lossy().replace('\\', "/");
            let at = governing.directory.join(path);
            agreed(&declares(&at)?, &at.display().to_string())?;

            Ok(Settled {
                // A live sibling on disk has no pin, deliberately: live code has nothing to be pinned to.
                fields: vec![
                    ("source".to_string(), "path".to_string()),
                    ("path".to_string(), spelled),
                ],
                cleared: vec!["url", "rev", "hash"],
                probe: None,
            })
        }
        Repoint::Fetched { url, rev } => {
            let rev = rev.as_deref().ok_or_else(|| {
                format!(
                    "the dependency {name:?} is fetched from a repository, which is pinned to a revision: `curios pin dependency {name} --url {url} --rev <REV>`"
                )
            })?;

            fetched(url, rev)
        }
        Repoint::Revised(rev) => fetched(&url_of("rev")?, rev),
        Repoint::Refresh => {
            let rev = stated(document, Subject::Dependency, name, "rev").ok_or_else(|| {
                format!(
                    "the dependency {name:?} states no `rev`; only a row pinned to a revision has a delivery to perform again"
                )
            })?;

            fetched(&url_of("refresh")?, &rev)
        }
    }
}

/// The row `name` keys, created empty if it is not there yet.
///
/// A created row carries the one field that identifies it — a `[[foreign]]` row its `name`, since the array is keyed by nothing else — so the row is findable the moment it exists rather than only once the caller has finished filling it.
fn row_mut<'a>(document: &'a mut DocumentMut, subject: Subject, name: &str) -> Row<'a> {
    match subject {
        Subject::Foreign => {
            let tables = document
                .entry("foreign")
                .or_insert(Item::ArrayOfTables(ArrayOfTables::new()))
                .as_array_of_tables_mut()
                .expect("`foreign` is an array of tables");

            let found = tables
                .iter()
                .position(|table| table.get("name").and_then(Item::as_str) == Some(name));

            let index = found.unwrap_or_else(|| {
                let mut fresh = Table::new();
                fresh.insert("name", toml_edit::value(name));
                tables.push(fresh);

                tables.len() - 1
            });

            Row::Table(tables.get_mut(index).expect("the row just located"))
        }
        Subject::Dependency => {
            let table = document
                .entry("dependencies")
                .or_insert(Item::Table(Table::new()))
                .as_table_mut()
                .expect("`dependencies` is a table");

            // An inline table, which is how the reference spells a dependency and so how one written here reads beside one written by hand.
            let entry = table
                .entry(name)
                .or_insert(Item::Value(Value::InlineTable(InlineTable::new())));

            match entry {
                Item::Table(table) => Row::Table(table),
                Item::Value(Value::InlineTable(inline)) => Row::Inline(inline),
                other => {
                    // A `[dependencies]` key holding a string or an array is not a row this can edit, and overwriting it would discard whatever somebody meant by it.
                    unreachable!("a dependency row is a table or an inline table, not {other:?}")
                }
            }
        }
    }
}
