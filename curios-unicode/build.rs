//! Downloads, verifies and reads the pinned files of the Unicode Character Database, and writes the tables `src/lib.rs` includes.
//!
//! The files are fetched once and kept in `.artifacts/<version>/` beside this crate — outside Cargo's target tree, so `cargo clean` leaves them — and each is checked against its pinned SHA-256 in memory, whether it was downloaded or placed there by hand for an offline build.
//!
//! **The library's enums are the classes.** A run of one class is written as the variant it is, named from the database's own spelling (`Regional_Indicator` is `RegionalIndicator`, `CR` is `Cr`), so a class the database names and an enum lacks is refused where the library compiles, and this script holds no list of classes. The class a scalar no line names is the one the file's `@missing` line gives the whole range, and its runs are left out.
//!
//! **A mapping is written as records.** Each scalar a mapping changes is read off the database's rows, and the scalars are gathered in order into records, each covering scalars a step of one or two apart that become the same scalars moved as far, as `README.md` decides.

use {
    sha2::{Digest, Sha256},
    std::{
        collections::BTreeMap,
        env,
        fmt::Write,
        fs,
        io::Read,
        path::{Path, PathBuf},
    },
};

/// The version of the database every table is built from.
const VERSION: &str = "18.0.0";

/// Every file read, by its path under the version's `ucd/` directory, with its SHA-256.
const FILES: [(&str, &str); 9] = [
    (
        "EastAsianWidth.txt",
        "a0cf29eacd00cfcaec4381c6b7c281685f18dbb4e7ff82b4076ccb342ca839aa",
    ),
    (
        "emoji/emoji-data.txt",
        "80d00f8e616a0ef27fd6b8de3b758c06383b5d917e2977709578e68baf733bf1",
    ),
    (
        "auxiliary/GraphemeBreakProperty.txt",
        "0839dcb79e4ac639ecd538b1abf7c9d22e3f9dd265b7e182d33627aa4d75b45a",
    ),
    (
        "DerivedCoreProperties.txt",
        "09c928886a178fcafd93c29e4bd59073a058e5a100b716d425cb563ab50f68c9",
    ),
    (
        "auxiliary/GraphemeBreakTest.txt",
        "b0cf047ee94485bbdc846de2b902f5f8a815f6b674f9d04223cddadd91c9df31",
    ),
    (
        "UnicodeData.txt",
        "0736451de439ae7baf1425136617da495e09ee5afbe6e394374db7009ea08950",
    ),
    (
        "PropList.txt",
        "f438f532e8737bb8a2702126cdf9c4af5e357c58c7acf9d9eb2fc7c1a1d955d6",
    ),
    (
        "SpecialCasing.txt",
        "8538dea57c184f1ef3783885ea79677b10f6efa06423717157e63712f14d1ad2",
    ),
    (
        "CaseFolding.txt",
        "a004797658a457bec4dc11683e39f69249ea3b595b752dbea6721c4c9f587b0d",
    ),
];

/// One past the last scalar value.
const SCALARS: usize = 0x11_0000;

fn sha256_hex(bytes: &[u8]) -> String {
    Sha256::digest(bytes)
        .iter()
        .fold(String::new(), |mut hex, byte| {
            let _ = write!(hex, "{byte:02x}");
            hex
        })
}

fn url(path: &str) -> String {
    format!("https://www.unicode.org/Public/{VERSION}/ucd/{path}")
}

/// Where a file is kept between builds: its own name under the version's cache directory.
fn kept(path: &str) -> PathBuf {
    let name = Path::new(path).file_name().expect("a file name");

    Path::new(&env::var("CARGO_MANIFEST_DIR").expect("cargo sets the manifest directory"))
        .join(".artifacts")
        .join(VERSION)
        .join(name)
}

fn download(url: &str) -> Result<Vec<u8>, String> {
    let response = ureq::get(url)
        .call()
        .map_err(|error| format!("GET {url} failed: {error}"))?;
    let mut bytes = Vec::new();
    response
        .into_body()
        .into_reader()
        .read_to_end(&mut bytes)
        .map_err(|error| format!("reading the response from {url} failed: {error}"))?;

    Ok(bytes)
}

fn instructions_on_failure(path: &str, kept: &Path, sha256: &str, cause: &str) -> ! {
    panic!(
        "\n\ncould not obtain a file of the Unicode Character Database curios-unicode is built from:\n  {cause}\n\n\
        To build offline, download this file by hand:\n  {}\n\
        verify it has sha256:\n  {sha256}\n\
        and place it at:\n  {}\n\
        then re-run the build.\n\n",
        url(path),
        kept.display(),
    );
}

/// The text of one pinned file, checked against its hash: the copy an earlier build kept or a hand placed, or else a download, kept once it is checked.
fn text(path: &str, sha256: &str) -> String {
    let kept = kept(path);
    let (bytes, fetched) = match fs::read(&kept) {
        Ok(bytes) => (bytes, false),
        Err(error) if error.kind() == std::io::ErrorKind::NotFound => (
            download(&url(path))
                .unwrap_or_else(|error| instructions_on_failure(path, &kept, sha256, &error)),
            true,
        ),
        Err(error) => panic!("read {}: {error}", kept.display()),
    };

    let actual = sha256_hex(&bytes);
    if actual != sha256 {
        instructions_on_failure(
            path,
            &kept,
            sha256,
            &format!("sha256 mismatch: expected {sha256}, got {actual}"),
        );
    }

    if fetched {
        let directory = kept.parent().expect("a kept file has a directory");
        fs::create_dir_all(directory)
            .unwrap_or_else(|error| panic!("create {}: {error}", directory.display()));
        let partial = kept.with_extension("partial");
        fs::write(&partial, &bytes)
            .unwrap_or_else(|error| panic!("write {}: {error}", partial.display()));
        fs::rename(&partial, &kept)
            .unwrap_or_else(|error| panic!("keep {}: {error}", kept.display()));
    }

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
    let spelled = |targets: &[usize]| {
        targets
            .iter()
            .map(|target| format!("0x{target:X}"))
            .collect::<Vec<_>>()
            .join(", ")
    };

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

/// Every case of the conformance file: the clusters a run of scalars segments into.
fn cases(text: &str) -> Vec<Vec<Vec<u32>>> {
    text.lines()
        .map(|line| line.split('#').next().unwrap_or("").trim())
        .filter(|content| !content.is_empty())
        .map(|content| {
            let mut clusters: Vec<Vec<u32>> = Vec::new();
            for token in content.split_whitespace() {
                match token {
                    "÷" => clusters.push(Vec::new()),
                    "×" => {}
                    hex => clusters
                        .last_mut()
                        .expect("a case opens with a boundary")
                        .push(u32::from_str_radix(hex, 16).expect("a scalar in hexadecimal")),
                }
            }
            clusters.retain(|cluster| !cluster.is_empty());
            clusters
        })
        .collect()
}

fn main() {
    println!("cargo::rerun-if-changed=build.rs");

    let [
        width,
        emoji,
        grapheme,
        derived,
        conformance,
        unicode_data,
        properties,
        special,
        folded,
    ] = FILES.map(|(path, sha256)| text(path, sha256));

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

    out.push_str(
        "/// Every case of `GraphemeBreakTest.txt`: a run of scalars as the clusters it segments into.\n#[cfg(any(test, feature = \"test-support\"))]\npub(crate) static GRAPHEME_BREAK_CASES: &[&[&[u32]]] = &[\n",
    );
    for case in cases(&conformance) {
        let clusters: Vec<String> = case
            .iter()
            .map(|cluster| {
                let scalars: Vec<String> = cluster
                    .iter()
                    .map(|scalar| format!("0x{scalar:X}"))
                    .collect();
                format!("&[{}]", scalars.join(", "))
            })
            .collect();
        let _ = writeln!(out, "    &[{}],", clusters.join(", "));
    }
    out.push_str("];\n");

    let generated =
        Path::new(&env::var("OUT_DIR").expect("cargo sets the output directory")).join("tables.rs");
    fs::write(&generated, out)
        .unwrap_or_else(|error| panic!("write {}: {error}", generated.display()));
}
