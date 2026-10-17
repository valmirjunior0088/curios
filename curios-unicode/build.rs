//! Downloads, verifies and reads the pinned files of the Unicode Character Database, and writes the tables `src/lib.rs` includes.
//!
//! The files are fetched once and kept in `.artifacts/<version>/` beside this crate — outside Cargo's target tree, so `cargo clean` leaves them — and each is checked against its pinned SHA-256 in memory, whether it was downloaded or placed there by hand for an offline build.
//!
//! **The library's enums are the classes.** A run of one class is written as the variant it is, named from the database's own spelling (`Regional_Indicator` is `RegionalIndicator`, `CR` is `Cr`), so a class the database names and an enum lacks is refused where the library compiles, and this script holds no list of classes. The class a scalar no line names is the one the file's `@missing` line gives the whole range, and its runs are left out.

use {
    sha2::{Digest, Sha256},
    std::{
        env,
        fmt::Write as _,
        fs,
        io::Read,
        path::{Path, PathBuf},
    },
};

/// The version of the database every table is built from.
const VERSION: &str = "18.0.0";

/// Every file read, by its path under the version's `ucd/` directory, with its SHA-256.
const FILES: [(&str, &str); 5] = [
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

/// Every scalar's place in a binary property of `emoji-data.txt`, which names the scalars that have it and states no `@missing` line: `1` where a scalar has it.
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

    let [width, emoji, grapheme, derived, conformance] =
        FILES.map(|(path, sha256)| text(path, sha256));

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
