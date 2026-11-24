//! Reads the files of the Unicode Character Database the task files pin, and writes the tables `src/lib.rs` includes.
//!
//! The files are kept in `.artifacts/unicode-<version>/` beside this crate, outside Cargo's target tree, so `cargo clean` leaves them: `cargo xtask` fetches them there at the digest `xtask/tasks.toml` pins, `[data.unicode]`, before the first step of any run, and this script reads what is kept and fetches nothing, so no build links a network client under the crates that read a character's properties. A build that finds none says what keeps them.
//!
//! **The library's enums are the classes.** A run of one class is written as the variant it is, named from the database's own spelling (`Regional_Indicator` is `RegionalIndicator`, `CR` is `Cr`), so a class the database names and an enum lacks is refused where the library compiles, and this script holds no list of classes. The class a scalar no line names is the one the file's `@missing` line gives the whole range, and its runs are left out.
//!
//! **A mapping is written as records.** Each scalar a mapping changes is read off the database's rows, and the scalars are gathered in order into records, each covering scalars a step of one or two apart that become the same scalars moved as far, as `README.md` decides.
//!
//! **A decomposition is written whole.** Each scalar a decomposition changes is written with every scalar it ends at, decomposed until nothing it becomes decomposes again, and the primary composites are the pairs a canonical decomposition of two scalars names but the excluded ones, as `README.md` decides.

use std::{
    collections::BTreeMap,
    env,
    fmt::Write,
    fs,
    path::{Path, PathBuf},
};

/// The version of the database every table is built from, which names the directory its files are kept in: the version `xtask/tasks.toml` pins, where a build that names another finds nothing kept.
const VERSION: &str = "18.0.0";

/// Every file the tables are built from, by its path under the version's `ucd/` directory, which is its path where the database is kept: the conformance files beside them are test support's, `src/test_support.rs`, which no build reads.
const FILES: [&str; 9] = [
    "EastAsianWidth.txt",
    "emoji/emoji-data.txt",
    "auxiliary/GraphemeBreakProperty.txt",
    "DerivedCoreProperties.txt",
    "UnicodeData.txt",
    "PropList.txt",
    "SpecialCasing.txt",
    "CaseFolding.txt",
    "DerivedNormalizationProps.txt",
];

/// One past the last scalar value.
const SCALARS: usize = 0x11_0000;

/// Where the database is kept: beside this crate, under its name and version in the task files, where `cargo xtask` files it.
fn kept() -> PathBuf {
    Path::new(&env::var("CARGO_MANIFEST_DIR").expect("cargo sets the manifest directory"))
        .join(".artifacts")
        .join(format!("unicode-{VERSION}"))
}

/// The text of one file of the database, read where it is kept, and read again by the next build only where it changed.
fn text(path: &str) -> String {
    let kept = kept().join(path);
    println!("cargo::rerun-if-changed={}", kept.display());
    let bytes = fs::read(&kept).unwrap_or_else(|error| {
        panic!(
            "the Unicode Character Database is not kept at {}: {error}\n\nEvery `cargo xtask` run fetches it before its first step, at the version and the digest `xtask/tasks.toml` pins, and no build fetches: run `cargo xtask compiler launcher`, then build again. This crate reads version {VERSION}.",
            kept.display()
        )
    });

    String::from_utf8(bytes).unwrap_or_else(|error| panic!("{path} is not UTF-8: {error}"))
}

/// The scalars a field names: one code point, or a range written `first..last`.
fn scalars(field: &str) -> (usize, usize) {
    let parse = |hex: &str| {
        usize::from_str_radix(hex.trim(), 16).unwrap_or_else(|error| panic!("{hex:?}: {error}"))
    };

    match field.split_once("..") {
        Some((first, last)) => (parse(first), parse(last)),
        None => (parse(field), parse(field)),
    }
}

/// The variant of the library's enum a class the database spells `name` is: each part between underscores capitalized, one written in capitals alone read as a word.
fn variant(name: &str) -> String {
    name.split('_')
        .map(|part| {
            let mut letters = part.chars();
            let first = letters.next().map(|first| first.to_ascii_uppercase());
            let rest: String = match part.chars().all(|letter| letter.is_ascii_uppercase()) {
                true => letters.map(|letter| letter.to_ascii_lowercase()).collect(),
                false => letters.collect(),
            };
            first.into_iter().chain(rest.chars()).collect::<String>()
        })
        .collect()
}

/// One property's classes, scalar by scalar, from the lines of a file that `select` reads.
struct Classified {
    /// The class every scalar has, by its index among `names`.
    each: Vec<usize>,
    /// The classes named, in the order the file names them; the first is the one its `@missing` line gives the whole range.
    names: Vec<String>,
}

/// Every scalar's class of one property. `select` is given a line's fields and answers the field naming the class where the line is about this property; the `@missing` lines are read first, so the lines naming scalars override them.
fn classified(text: &str, select: impl Fn(&[&str]) -> Option<String>) -> Classified {
    let mut classes = Classified {
        each: vec![0; SCALARS],
        names: Vec::new(),
    };
    let assign = |classes: &mut Classified, fields: &[&str]| {
        if let Some(class) = select(fields) {
            let index = match classes.names.iter().position(|name| *name == class) {
                Some(index) => index,
                None => {
                    classes.names.push(class);
                    classes.names.len() - 1
                }
            };
            let (first, last) = scalars(fields[0]);
            classes.each[first..=last].fill(index);
        }
    };
    let fields = |line: &str| -> Vec<String> {
        line.split(';')
            .map(|field| field.trim().to_string())
            .collect()
    };

    for line in text.lines() {
        if let Some(missing) = line.strip_prefix("# @missing:") {
            let owned = fields(missing);
            let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
            assign(&mut classes, &refs);
        }
    }
    assert_eq!(
        classes.names.len(),
        1,
        "a property is read under one @missing line, which gives the whole range its class"
    );

    for line in text.lines() {
        let content = line.split('#').next().unwrap_or("").trim();
        if !content.is_empty() {
            let owned = fields(content);
            let refs: Vec<&str> = owned.iter().map(String::as_str).collect();
            assign(&mut classes, &refs);
        }
    }

    classes
}

/// The runs of scalars that share a class other than the missing one, as `(first, last, class)`.
fn runs(each: &[usize]) -> Vec<(usize, usize, usize)> {
    let mut runs: Vec<(usize, usize, usize)> = Vec::new();

    for (scalar, &class) in each.iter().enumerate() {
        if class == 0 {
            continue;
        }
        match runs.last_mut() {
            Some((_, last, held)) if *last + 1 == scalar && *held == class => *last = scalar,
            _ => runs.push((scalar, scalar, class)),
        }
    }

    runs
}

/// A property of several classes as the Rust it is written to: the class a scalar no run names, and the runs of each other class, its classes the variants of the library's enum `kind`.
fn classes_table(
    out: &mut String,
    constant: &str,
    kind: &str,
    property: &str,
    classes: &Classified,
) {
    let _ = writeln!(
        out,
        "/// The `{property}` class of a scalar no run of [`{constant}`] names.\npub(crate) const {constant}_MISSING: {kind} = {kind}::{};\n",
        variant(&classes.names[0])
    );
    let _ = writeln!(
        out,
        "/// Each run of scalars of one `{property}` class but the missing one.\npub(crate) static {constant}: &[(u32, u32, {kind})] = &["
    );
    for (first, last, class) in runs(&classes.each) {
        let _ = writeln!(
            out,
            "    (0x{first:X}, 0x{last:X}, {kind}::{}),",
            variant(&classes.names[class])
        );
    }
    out.push_str("];\n\n");
}

/// A binary property as the Rust it is written to: the runs of scalars that have it.
fn binary_table(out: &mut String, constant: &str, property: &str, each: &[usize]) {
    let _ = writeln!(
        out,
        "/// Each run of `{property}` scalars.\npub(crate) static {constant}: &[(u32, u32)] = &["
    );
    for (first, last, _) in runs(each) {
        let _ = writeln!(out, "    (0x{first:X}, 0x{last:X}),");
    }
    out.push_str("];\n\n");
}

/// Every scalar's place in a binary property, of a file that names the scalars that have it and states no `@missing` line for it: `1` where a scalar has it.
fn binary(text: &str, property: &str) -> Vec<usize> {
    let mut each = vec![0; SCALARS];

    for line in text.lines() {
        let content = line.split('#').next().unwrap_or("").trim();
        let mut fields = content.split(';').map(str::trim);
        if let (Some(field), Some(name)) = (fields.next(), fields.next())
            && name == property
        {
            let (first, last) = scalars(field);
            each[first..=last].fill(1);
        }
    }

    each
}

/// Every scalar's place in the general categories `categories` of `UnicodeData.txt`: `1` where a scalar's category is one of them. A range the file states as a pair of rows, its first named `<…, First>` and its last `<…, Last>`, is one category throughout.
fn general_category(text: &str, categories: &[&str]) -> Vec<usize> {
    let mut each = vec![0; SCALARS];
    let mut opened = None;

    for line in text.lines().filter(|line| !line.is_empty()) {
        let fields: Vec<&str> = line.split(';').collect();
        let (scalar, _) = scalars(fields[0]);
        let held = usize::from(categories.contains(&fields[2]));
        if fields[1].ends_with(", First>") {
            opened = Some(scalar);
        } else {
            let first = opened.take().unwrap_or(scalar);
            each[first..=scalar].fill(held);
        }
    }

    each
}

/// Each scalar a mapping changes, with the one to three scalars it becomes.
type Mapping = BTreeMap<usize, Vec<usize>>;

/// The simple mapping `UnicodeData.txt` states in its field `field`: each scalar the field names another for, to that one.
fn simple(text: &str, field: usize) -> Mapping {
    text.lines()
        .filter(|line| !line.is_empty())
        .filter_map(|line| {
            let fields: Vec<&str> = line.split(';').collect();
            (!fields[field].is_empty())
                .then(|| (scalars(fields[0]).0, vec![scalars(fields[field]).0]))
        })
        .collect()
}

/// The full mapping: `simple` with the unconditional rows of `SpecialCasing.txt`'s column `column` over it. A row conditioned on a context or a language is left out, and a row mapping a scalar to itself takes it out of the mapping.
fn full(mut simple: Mapping, special: &str, column: usize) -> Mapping {
    for line in special.lines() {
        let content = line.split('#').next().unwrap_or("").trim();
        let fields: Vec<&str> = content.split(';').map(str::trim).collect();
        if content.is_empty() || fields.get(4).is_some_and(|condition| !condition.is_empty()) {
            continue;
        }
        let scalar = scalars(fields[0]).0;
        let targets: Vec<usize> = fields[column]
            .split_whitespace()
            .map(|target| scalars(target).0)
            .collect();
        match targets == [scalar] {
            true => simple.remove(&scalar),
            false => simple.insert(scalar, targets),
        };
    }

    simple
}

/// The full case folding `CaseFolding.txt` states: its common rows and its full ones, of status `C` and `F`; the simple rows, `S`, and the Turkic ones, `T`, are left out.
fn folding(text: &str) -> Mapping {
    text.lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|content| !content.is_empty())
        .filter_map(|content| {
            let fields: Vec<&str> = content.split(';').map(str::trim).collect();
            matches!(fields[1], "C" | "F").then(|| {
                let targets = fields[2]
                    .split_whitespace()
                    .map(|target| scalars(target).0)
                    .collect();
                (scalars(fields[0]).0, targets)
            })
        })
        .collect()
}

/// One record of a mapping: the scalars `first`, `first + step`, … up to `last` each become `targets`, the first of them moved as far as the scalar is from `first`.
struct Record {
    first: usize,
    last: usize,
    step: usize,
    targets: Vec<usize>,
}

/// A mapping as the records a scan in order gathers it into: a scalar joins the record before it where it lies one or two past its last, at the record's step, and becomes what the record's scalars become moved as far.
fn records(mapping: &Mapping) -> Vec<Record> {
    let mut records: Vec<Record> = Vec::new();

    for (&scalar, targets) in mapping {
        if let Some(record) = records.last_mut() {
            let gap = scalar - record.last;
            let step = match record.first == record.last {
                true => gap,
                false => record.step,
            };
            let moved = targets[0] + record.first == record.targets[0] + scalar;
            if gap == step && gap <= 2 && moved && targets[1..] == record.targets[1..] {
                record.last = scalar;
                record.step = step;
                continue;
            }
        }
        records.push(Record {
            first: scalar,
            last: scalar,
            step: 1,
            targets: targets.clone(),
        });
    }

    records
}

/// A mapping as the Rust it is written to: its records, and, for a crate testing a reading of them, each scalar it changes with what the database's rows map it to.
fn mapping_table(out: &mut String, constant: &str, property: &str, mapping: &Mapping) {
    let _ = writeln!(
        out,
        "/// Each record of the `{property}` mapping: the scalars `first`, `first + step`, … up to `last`, and what each becomes, the first target moved as far as the scalar is from `first`.\npub(crate) static {constant}: &[(u32, u32, u8, &[u32])] = &["
    );
    for record in records(mapping) {
        let _ = writeln!(
            out,
            "    (0x{:X}, 0x{:X}, {}, &[{}]),",
            record.first,
            record.last,
            record.step,
            spelled(&record.targets)
        );
    }
    out.push_str("];\n\n");

    let _ = writeln!(
        out,
        "/// Each scalar the `{property}` mapping changes, with what the database's rows map it to.\n#[cfg(any(test, feature = \"test-support\"))]\npub(crate) static {constant}_ROWS: &[(u32, &[u32])] = &["
    );
    for (scalar, targets) in mapping {
        let _ = writeln!(out, "    (0x{scalar:X}, &[{}]),", spelled(targets));
    }
    out.push_str("];\n\n");
}

/// Scalars as the Rust that writes them, each in hexadecimal, a comma apart.
fn spelled<T: std::fmt::UpperHex>(scalars: &[T]) -> String {
    scalars
        .iter()
        .map(|scalar| format!("0x{scalar:X}"))
        .collect::<Vec<_>>()
        .join(", ")
}

/// Every scalar's canonical combining class, field 3 of `UnicodeData.txt`: `0`, a starter's, where no row names another, and a range the file states as a pair of rows one class throughout.
fn combining_classes(text: &str) -> Vec<usize> {
    let mut each = vec![0; SCALARS];
    let mut opened = None;

    for line in text.lines().filter(|line| !line.is_empty()) {
        let fields: Vec<&str> = line.split(';').collect();
        let (scalar, _) = scalars(fields[0]);
        let class = fields[3]
            .parse::<usize>()
            .unwrap_or_else(|error| panic!("{line:?}: {error}"));
        if fields[1].ends_with(", First>") {
            opened = Some(scalar);
        } else {
            let first = opened.take().unwrap_or(scalar);
            each[first..=scalar].fill(class);
        }
    }

    each
}

/// Each scalar's decomposition one level deep, field 5 of `UnicodeData.txt`: whether it is canonical, a mapping no `<tag>` opens, and the scalars it becomes. No range the file states as a pair of rows decomposes there; the Hangul syllables decompose by arithmetic, which the library does.
fn decompositions(text: &str) -> BTreeMap<usize, (bool, Vec<usize>)> {
    text.lines()
        .filter(|line| !line.is_empty())
        .filter_map(|line| {
            let fields: Vec<&str> = line.split(';').collect();
            let mut parts = fields[5].split_whitespace().peekable();
            let canonical = !parts.peek()?.starts_with('<');
            if !canonical {
                parts.next();
            }
            let targets = parts.map(|target| scalars(target).0).collect();
            Some((scalars(fields[0]).0, (canonical, targets)))
        })
        .collect()
}

/// `scalar` decomposed until nothing it becomes decomposes again: under the canonical mappings alone, or under every one where `compatible`.
fn decomposed(
    mappings: &BTreeMap<usize, (bool, Vec<usize>)>,
    scalar: usize,
    compatible: bool,
) -> Vec<usize> {
    match mappings.get(&scalar) {
        Some((canonical, targets)) if *canonical || compatible => targets
            .iter()
            .flat_map(|&target| decomposed(mappings, target, compatible))
            .collect(),
        _ => vec![scalar],
    }
}

/// The full canonical decomposition: each scalar a canonical mapping changes, with every scalar it becomes.
fn canonical_decomposition(mappings: &BTreeMap<usize, (bool, Vec<usize>)>) -> Mapping {
    mappings
        .iter()
        .filter(|(_, (canonical, _))| *canonical)
        .map(|(&scalar, _)| (scalar, decomposed(mappings, scalar, false)))
        .collect()
}

/// The full compatibility decomposition where it is other than the canonical one: each scalar whose decomposition under every mapping differs from its decomposition under the canonical ones, with every scalar it becomes.
fn compatibility_decomposition(mappings: &BTreeMap<usize, (bool, Vec<usize>)>) -> Mapping {
    mappings
        .keys()
        .filter_map(|&scalar| {
            let compatible = decomposed(mappings, scalar, true);
            (compatible != decomposed(mappings, scalar, false)).then_some((scalar, compatible))
        })
        .collect()
}

/// The primary composites: each canonical mapping of two scalars, as the pair and the scalar the pair composes to, in the order of the pairs, but a scalar `Full_Composition_Exclusion` keeps out of every composition.
fn compositions(
    mappings: &BTreeMap<usize, (bool, Vec<usize>)>,
    excluded: &[usize],
) -> Vec<(usize, usize, usize)> {
    let mut pairs: Vec<(usize, usize, usize)> = mappings
        .iter()
        .filter(|&(&scalar, (canonical, targets))| {
            *canonical && targets.len() == 2 && excluded[scalar] == 0
        })
        .map(|(&scalar, (_, targets))| (targets[0], targets[1], scalar))
        .collect();
    pairs.sort_unstable();
    pairs
}

/// The canonical combining classes as the Rust they are written to: each run of scalars of one class but `0`.
fn combining_class_table(out: &mut String, each: &[usize]) {
    out.push_str(
        "/// Each run of scalars of one `Canonical_Combining_Class` but `0`, a starter's.\npub(crate) static COMBINING_CLASS: &[(u32, u32, u8)] = &[\n",
    );
    for (first, last, class) in runs(each) {
        let _ = writeln!(out, "    (0x{first:X}, 0x{last:X}, {class}),");
    }
    out.push_str("];\n\n");
}

/// A decomposition as the Rust it is written to: each scalar it changes, with every scalar that scalar becomes.
fn decomposition_table(out: &mut String, constant: &str, what: &str, mapping: &Mapping) {
    let _ = writeln!(
        out,
        "/// Each scalar the {what} changes, with every scalar it becomes.\npub(crate) static {constant}: &[(u32, &[u32])] = &["
    );
    for (scalar, targets) in mapping {
        let _ = writeln!(out, "    (0x{scalar:X}, &[{}]),", spelled(targets));
    }
    out.push_str("];\n\n");
}

/// The primary composites as the Rust they are written to: each pair, and the scalar it composes to.
fn composition_table(out: &mut String, pairs: &[(usize, usize, usize)]) {
    out.push_str(
        "/// Each primary composite: the pair of scalars it composes from, in the order of the pairs, and itself.\npub(crate) static COMPOSITION: &[(u32, u32, u32)] = &[\n",
    );
    for (first, second, composite) in pairs {
        let _ = writeln!(out, "    (0x{first:X}, 0x{second:X}, 0x{composite:X}),");
    }
    out.push_str("];\n\n");
}

fn main() {
    println!("cargo::rerun-if-changed=build.rs");
    // Test support reads the conformance files where the database is kept, which is stated once, here.
    println!(
        "cargo::rustc-env=CURIOS_UNICODE_DATABASE={}",
        kept().display()
    );

    let [
        width,
        emoji,
        grapheme,
        derived,
        unicode_data,
        properties,
        special,
        folded,
        normalization,
    ] = FILES.map(text);

    let second = |fields: &[&str]| fields.get(1).map(|class| (*class).to_string());
    let conjunct = |fields: &[&str]| {
        (fields.get(1) == Some(&"InCB")).then(|| fields.get(2).copied().unwrap_or("").to_string())
    };

    let mut out = format!(
        "/// The version of the Unicode Character Database every table is built from.\npub const UNICODE_VERSION: &str = \"{VERSION}\";\n\n"
    );
    classes_table(
        &mut out,
        "GRAPHEME_BREAK",
        "GraphemeBreak",
        "Grapheme_Cluster_Break",
        &classified(&grapheme, second),
    );
    classes_table(
        &mut out,
        "INDIC_CONJUNCT_BREAK",
        "IndicConjunctBreak",
        "Indic_Conjunct_Break",
        &classified(&derived, conjunct),
    );
    classes_table(
        &mut out,
        "EAST_ASIAN_WIDTH",
        "EastAsianWidth",
        "East_Asian_Width",
        &classified(&width, second),
    );
    binary_table(
        &mut out,
        "EXTENDED_PICTOGRAPHIC",
        "Extended_Pictographic",
        &binary(&emoji, "Extended_Pictographic"),
    );
    binary_table(
        &mut out,
        "EMOJI_PRESENTATION",
        "Emoji_Presentation",
        &binary(&emoji, "Emoji_Presentation"),
    );
    binary_table(
        &mut out,
        "DEFAULT_IGNORABLE",
        "Default_Ignorable_Code_Point",
        &binary(&derived, "Default_Ignorable_Code_Point"),
    );
    binary_table(
        &mut out,
        "ALPHABETIC",
        "Alphabetic",
        &binary(&derived, "Alphabetic"),
    );
    binary_table(
        &mut out,
        "NUMERIC",
        "General_Category=Number",
        &general_category(&unicode_data, &["Nd", "Nl", "No"]),
    );
    binary_table(
        &mut out,
        "CONTROL",
        "General_Category=Control",
        &general_category(&unicode_data, &["Cc"]),
    );
    binary_table(
        &mut out,
        "WHITE_SPACE",
        "White_Space",
        &binary(&properties, "White_Space"),
    );
    binary_table(&mut out, "CASED", "Cased", &binary(&derived, "Cased"));
    binary_table(
        &mut out,
        "LOWERCASE",
        "Lowercase",
        &binary(&derived, "Lowercase"),
    );
    binary_table(
        &mut out,
        "UPPERCASE",
        "Uppercase",
        &binary(&derived, "Uppercase"),
    );
    binary_table(
        &mut out,
        "CASE_IGNORABLE",
        "Case_Ignorable",
        &binary(&derived, "Case_Ignorable"),
    );
    mapping_table(
        &mut out,
        "UPPER",
        "Uppercase_Mapping",
        &full(simple(&unicode_data, 12), &special, 3),
    );
    mapping_table(
        &mut out,
        "LOWER",
        "Lowercase_Mapping",
        &full(simple(&unicode_data, 13), &special, 1),
    );
    mapping_table(&mut out, "FOLD", "Case_Folding", &folding(&folded));
    combining_class_table(&mut out, &combining_classes(&unicode_data));
    let mappings = decompositions(&unicode_data);
    decomposition_table(
        &mut out,
        "CANONICAL_DECOMPOSITION",
        "full canonical decomposition",
        &canonical_decomposition(&mappings),
    );
    decomposition_table(
        &mut out,
        "COMPATIBILITY_DECOMPOSITION",
        "full compatibility decomposition, where it is other than the canonical one,",
        &compatibility_decomposition(&mappings),
    );
    composition_table(
        &mut out,
        &compositions(
            &mappings,
            &binary(&normalization, "Full_Composition_Exclusion"),
        ),
    );

    let generated =
        Path::new(&env::var("OUT_DIR").expect("cargo sets the output directory")).join("tables.rs");
    fs::write(&generated, out)
        .unwrap_or_else(|error| panic!("write {}: {error}", generated.display()));
}
