//! What the sources this crate elaborates must be true of, beyond elaborating.

use {
    crate::{test_support::*, with_prelude},
    curios_abi::HostOp,
    curios_core::{Bound, DefinitionKind, Global, Nominal, Term, intrinsic_former},
    curios_text::{
        GroupItem, Module, SYNTAX, TopItem, UseGroup, prepare_prelude, sys_module, sys_source,
    },
    std::{
        collections::BTreeSet,
        fs,
        path::{Path, PathBuf},
    },
};

/// Every `.crs` file `/std` is authored in, in the one tree this crate holds, its modules of tests among them.
///
/// Walked rather than listed, for the reason the build script discovers its inputs rather than naming them: a module added without being registered is a mistake `the_std_record_names_every_authored_source_and_no_other` catches against this walk.
pub(crate) fn authored() -> Vec<PathBuf> {
    let mut sources = Vec::new();
    let mut pending = Vec::from([std_directory()]);

    while let Some(directory) = pending.pop() {
        for entry in fs::read_dir(&directory).expect("a directory this crate owns") {
            let path = entry.expect("a readable entry").path();

            match path.is_dir() {
                true => pending.push(path),
                false if path.extension().is_some_and(|kind| kind == "crs") => sources.push(path),
                false => {}
            }
        }
    }

    sources.sort();
    sources
}

/// **The standard library lints clean**, lowered with its tests, as `curios lint` lints a package: a declaration only a test reads is read.
///
/// The lints are exact and always on, so the honest test of them is the largest corpus in the tree: an import nothing resolves through, a binder nothing reads or a private declaration nothing reaches in `/std` is a finding to fix there, not a rule to relax. A binder only a proof the elaborator wrote reads is used, which only elaboration says, so the lowering here is credited with what the build script's elaboration recorded; lowering and restoring are the whole cost, so this belongs in the ordinary suite. A failure renders each lint as `curios lint` would.
#[test]
fn every_authored_source_is_lint_clean() {
    // Both roots, in the order the build script folds them: `/std`'s imports resolve only against a lowered `/sys`, and an unresolved import is not a lint but a lowering failure.
    let mut sys = prepare_prelude(&sys_source(), &[], &SYNTAX)
        .unwrap_or_else(|error| panic!("/sys failed to lower: {}", error.format()));
    let mut std = prepare_prelude(&std_source().tested(), &[&sys], &SYNTAX)
        .unwrap_or_else(|error| panic!("/std failed to lower: {}", error.format()));
    // Credited as a recompile credits a declaration it reuses: with what the build's elaboration of the same sources recorded as read by a proof it wrote.
    with_prelude(|units| {
        sys.credit_reused(units[0].text(), |_| true);
        std.credit_reused(units[1].text(), |_| true);
    });
    let lints = [&sys, &std]
        .iter()
        .flat_map(|prepared| prepared.lints())
        .map(|lint| lint.render())
        .collect::<Vec<_>>();
    assert!(
        lints.is_empty(),
        "{} lints in the prelude:\n\n{}",
        lints.len(),
        lints.join("\n\n")
    );
}

/// **The standard library declares every row a host answers, as the host answers it.**
///
/// A host's rows are `foreign` declarations of `/std`, linked by name. One declared in another module than `curios-abi` states, or at another signature, is a program that does not link, found only by the program that reaches the row; this finds it for every row.
#[test]
fn the_library_declares_every_row_a_host_answers() {
    with_prelude(|units| {
        let declared = units[1].foreigns();

        for op in HostOp::ALL {
            let name = op.declared();
            let row = declared
                .get(&name)
                .unwrap_or_else(|| panic!("`/std` declares no `{name}`"));
            let (written, stated) = (row.signature(), op.signature());

            assert!(
                written == stated,
                "`{name}` is declared {written:?}, and the host answers {stated:?}"
            );
        }

        assert_eq!(
            declared.iter().count(),
            HostOp::ALL.len(),
            "`/std` declares a row no host answers"
        );
    });
}

/// The roots of `/std` that may spell each part of `/sys`: the one statement of who reads it. `curios-prelude/README.md`'s "`/std` is laid out by purity, subject and kind" gives the rule: a carrier is read by its own root, a wire code set by the root owning the rows it travels on, and the status every row answers by `internal`, which decodes it once. Every other module reaches `/sys` through these.
const SYS_READERS: &[(&str, &[&str])] = &[
    ("Bool", &["Bool"]),
    ("Byte", &["Byte"]),
    ("Bytes", &["Bytes"]),
    ("Nat", &["Nat"]),
    ("Int", &["Int"]),
    ("Flt", &["Flt"]),
    ("List", &["List"]),
    ("Option", &["Option"]),
    ("Result", &["Result"]),
    ("Cell", &["Cell"]),
    ("Channel", &["Channel"]),
    ("Io", &["Io", "internal"]),
    ("Handle", &["internal"]),
    ("event", &["internal"]),
    ("status", &["internal"]),
    ("errno", &["internal"]),
    ("open_mode", &["internal"]),
    ("file_kind", &["fs"]),
    ("stdio_mode", &["Process"]),
    ("serial_parity", &["Serial"]),
    ("serial_flow", &["Serial"]),
    ("serial_op", &["Serial"]),
    ("unicode", &["Char"]),
    ("grapheme_break", &["Char"]),
    ("indic_conjunct_break", &["Char"]),
    ("east_asian_width", &["Char"]),
];

/// One place a source of `/std` spells a part of `/sys`.
struct Spelling {
    part: String,
    /// The root of `/std` the source stands in, `lib` for the header's own declarations.
    reader: String,
    /// Where, as a reader finds it.
    place: String,
}

/// The parts `/sys` declares: the name of each item at its top, a module or a declaration.
fn sys_parts() -> BTreeSet<String> {
    sys_module(&SYNTAX)
        .items
        .iter()
        .flat_map(|item| match item {
            TopItem::Mod(declared) => vec![declared.label.to_string()],
            TopItem::Induct(group) => group
                .iter()
                .map(|declared| declared.label.to_string())
                .collect(),
            // A carrier's facade, re-exporting its module's declaration of the same name.
            TopItem::Use(_) => Vec::new(),
            other => panic!("/sys declares an item that names no part: {other:?}"),
        })
        .collect()
}

/// The part a path names, where it is written from `/sys`.
fn part_spelled(written: &str) -> Option<&str> {
    written.strip_prefix("/sys/")?.split('/').next()
}

/// The root of `/std` the file at `path` stands in.
fn reader_of(path: &Path) -> String {
    let relative = path.strip_prefix(std_directory()).unwrap_or(path);
    let first = relative
        .components()
        .next()
        .expect("a file under the library's directory");

    first
        .as_os_str()
        .to_string_lossy()
        .trim_end_matches(".crs")
        .to_string()
}

/// Where `start` lies in the text of the file at `path`, as a reader finds it.
fn place_of(path: &Path, text: &str, start: usize) -> String {
    format!(
        "{}:{}",
        path.display(),
        text[..start].matches('\n').count() + 1
    )
}

/// Every place `/std` spells a part of `/sys`: each name a declaration writes, read at its span, and each `use` line, whose names no declaration holds.
fn sys_spellings() -> Vec<Spelling> {
    let sys = prepare_prelude(&sys_source(), &[], &SYNTAX)
        .unwrap_or_else(|error| panic!("/sys failed to lower: {}", error.format()));
    let std = prepare_prelude(&std_source(), &[&sys], &SYNTAX)
        .unwrap_or_else(|error| panic!("/std failed to lower: {}", error.format()));
    let mut spellings = Vec::new();

    for text in std.texts(None, |_| true) {
        for mention in text.mentions() {
            let span = &mention.span;
            let path = span
                .source
                .path
                .as_deref()
                .expect("a source of `/std` is a file");
            if let Some(part) = part_spelled(&span.source.text[span.start..span.end]) {
                spellings.push(Spelling {
                    part: part.to_string(),
                    reader: reader_of(path),
                    place: place_of(path, &span.source.text, span.start),
                });
            }
        }
    }

    for path in authored() {
        let text = fs::read_to_string(&path).expect("a source of `/std` reads");
        let module = text
            .parse::<Module>()
            .unwrap_or_else(|error| panic!("{} parses: {error:?}", path.display()));
        let reader = reader_of(&path);

        for item in &module.items {
            let TopItem::Use(declared) = item else {
                continue;
            };
            let span = declared
                .span
                .as_ref()
                .expect("a parsed `use` has its place");
            let written = text[span.start..span.end]
                .trim_start_matches("pub")
                .trim_start();
            let Some(after) = written
                .trim_start_matches("use")
                .trim_start()
                .strip_prefix("/sys/")
            else {
                continue;
            };
            let parts = match (after.starts_with('{'), &declared.group) {
                (true, UseGroup::Named(items)) => items
                    .iter()
                    .map(|item| match item {
                        GroupItem::Mod(label) | GroupItem::Let(label) | GroupItem::Both(label) => {
                            label.to_string()
                        }
                    })
                    .collect(),
                (_, UseGroup::Glob) if after.starts_with('*') => {
                    panic!("{}: a glob from `/sys` names every part", path.display())
                }
                _ => vec![
                    after
                        .split(['/', '{', ';'])
                        .next()
                        .unwrap_or_default()
                        .to_string(),
                ],
            };
            for part in parts {
                spellings.push(Spelling {
                    part,
                    reader: reader.clone(),
                    place: place_of(&path, &text, span.start),
                });
            }
        }
    }

    spellings
}

/// The readers `part`'s row names, or none where it has no row.
fn readers(part: &str) -> &'static [&'static str] {
    SYS_READERS
        .iter()
        .find(|(name, _)| *name == part)
        .map_or(&[], |(_, readers)| *readers)
}

/// **Every part of `/sys` has one row of readers, and the roster names no part `/sys` does not declare.** A code set `curios-abi` mints is read by no module until the roster names its reader.
#[test]
fn every_part_of_sys_has_one_row_of_readers() {
    let rostered = SYS_READERS
        .iter()
        .map(|(part, _)| part.to_string())
        .collect::<BTreeSet<_>>();

    assert_eq!(rostered.len(), SYS_READERS.len(), "a part has two rows");
    assert_eq!(sys_parts(), rostered);
}

/// **A module of `/std` spells only the parts of `/sys` its root reads.** Resolved names cannot tell `/sys/Nat` written out from `Nat` reached through `/std/Nat`, so this reads what the source writes.
#[test]
fn a_std_module_spells_only_the_parts_of_sys_its_root_reads() {
    let crossings = sys_spellings()
        .into_iter()
        .filter(|spelling| !readers(&spelling.part).contains(&spelling.reader.as_str()))
        .map(|spelling| {
            format!(
                "{}: /std/{} spells /sys/{}, which {:?} read",
                spelling.place,
                spelling.reader,
                spelling.part,
                readers(&spelling.part)
            )
        })
        .collect::<Vec<_>>();

    assert!(crossings.is_empty(), "{crossings:#?}");
}

/// **Every reader the roster names spells its part**, so a row says who reads `/sys` and not who once did.
#[test]
fn every_reader_a_row_names_spells_its_part() {
    let spelled = sys_spellings()
        .into_iter()
        .map(|spelling| (spelling.part, spelling.reader))
        .collect::<BTreeSet<_>>();
    let stale = SYS_READERS
        .iter()
        .flat_map(|(part, readers)| readers.iter().map(move |reader| (*part, *reader)))
        .filter(|(part, reader)| !spelled.contains(&(part.to_string(), reader.to_string())))
        .map(|(part, reader)| format!("/std/{reader} spells no /sys/{part}"))
        .collect::<Vec<_>>();

    assert!(stale.is_empty(), "{stale:#?}");
}

/// No term the prelude holds names a type former: elaboration spells a former applied in full as its node and one short of its application as its η-form, wherever a program writes it, so a former written and a former inferred are one term (`documentation/design/theory/a-type-former-applied-in-full-is-its-node.md`). Every former either root declares is checked, nominal or intrinsic, in every definition's type and body and every nominal entry's telescopes.
#[test]
fn no_term_of_the_prelude_names_a_type_former() {
    with_prelude(|prelude| {
        let roots = prelude.iter().map(|root| root.core()).collect::<Vec<_>>();
        let nominal = roots
            .iter()
            .flat_map(|module| module.formers())
            .map(|(name, _)| name)
            .collect::<BTreeSet<_>>();
        assert!(!nominal.is_empty(), "the prelude declares formers");
        let is_former = |name: &Global| {
            nominal.contains(name)
                || intrinsic_former(name, &[]).is_some()
                || intrinsic_former(name, &[Term::type_ground()]).is_some()
        };

        let mut named = Vec::new();
        for module in &roots {
            for item in module.items.iter() {
                for definition in item.definitions() {
                    let mut mentioned = definition.mentions();
                    if let DefinitionKind::Nominal(entry) = &definition.kind {
                        mentioned.extend(entry_mentions(entry));
                    }
                    named.extend(mentioned.into_iter().filter(&is_former).map(|former| {
                        format!("{} names {}", definition.name.faithful(), former.faithful())
                    }));
                }
            }
        }

        assert!(named.is_empty(), "{named:#?}");
    });
}

/// The top-level names a nominal entry's telescopes mention by name: its arity, its result sort and, for a family, its constructors' signatures.
fn entry_mentions(entry: &Nominal) -> BTreeSet<Global> {
    let (arity, result_sort, constructors) = match entry {
        Nominal::Inductive(family) => (
            &family.arity,
            &family.result_sort,
            family
                .constructors
                .iter()
                .map(|(_, constructor)| constructor.telescope.free_vars())
                .collect::<Vec<_>>(),
        ),
        Nominal::Struct(record) | Nominal::Concept { record, .. } => {
            (&record.arity, &record.result_sort, Vec::new())
        }
    };

    let mut mentions = BTreeSet::new();
    for frees in [arity.free_vars(), result_sort.free_vars()]
        .into_iter()
        .chain(constructors)
    {
        mentions.extend(frees.iter().filter_map(|free| free.as_global().copied()));
    }
    mentions
}
