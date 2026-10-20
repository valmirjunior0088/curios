//! A program's page: the files a browser build writes, by the path each stands at from the page's root.
//!
//! **Three files are the page's own.** `index.html` holds the import map, the loader's script element and one `<pre>` the program's output is written into while it stands in the document, which a gateway may remove. `index.js` is the loader: the harness, what it is configured with, the program's rows, the bridge, an import of every gateway, and the `start` that runs the program. `index.wasm` is the program. No package supplies any of them, and `curios-package` refuses each of [`PAGE_FILES`] where a manifest is read.
//!
//! **The rest is laid out as the packages are.** The page's root is the program's package's root: a gateway stands at the path its row names from the package's root, or where it is fetched at `<row>.js`, a `static` entry's files at its page path, and everything a dependency's rows put in the page under the dependency's name. Every path is claimed once, and a second claim is refused naming both claimants, the page's own files among them. Nothing written is root-absolute, so the page works under any path a server puts it at.
//!
//! **What the page checks.** The loader fetches `index.wasm` with its integrity, `index.html` states the loader's, and the import map states every other JavaScript file's, each the `sha256-` form of the file's SHA-256, so a file changed after the build fails to load rather than running.
//!
//! **Pure.** [`page`] reads no disk: what a row names is read by the caller, which writes or serves what this answers.

#[cfg(test)]
mod tests;

use {
    crate::{bridge_bytes, configuration, rows},
    askama::Template,
    base64::{Engine, engine::general_purpose::STANDARD},
    curios_abi::ForeignStore,
    curios_utilities::Digest,
    serde::Serialize,
    std::collections::{BTreeMap, btree_map::Entry},
};

/// The page itself.
pub const INDEX_HTML: &str = "index.html";

/// The loader.
pub const INDEX_JS: &str = "index.js";

/// The program.
pub const INDEX_WASM: &str = "index.wasm";

/// The names the page holds for itself, which no file of a package may stand at.
pub const PAGE_FILES: [&str; 3] = [INDEX_HTML, INDEX_JS, INDEX_WASM];

/// The `id` of the `<pre>` the loader writes the program's output into.
pub const OUTPUT: &str = "output";

/// The harness, which the loader carries whole.
const HARNESS: &str = include_str!("harness.mjs");

/// One `[[foreign]]` row a page links, the program reaching one of its declarations: who declares it, its gateway, what the gateway answers, and what the row's page serves beside it.
pub struct Linked {
    /// The package that declares the row.
    pub package: String,
    /// The row's name.
    pub row: String,
    /// The gateway, where it stands from its package's root.
    pub gateway: Placed,
    /// Each declaration of the program the gateway answers, beside the name of the export that answers it.
    pub answers: Vec<(String, String)>,
    /// Each file the row's `static` table serves, beside the page path of the entry that names it.
    pub statics: Vec<(String, Placed)>,
}

/// One file a package puts in a page: where it stands from the package's root, and what it holds.
pub struct Placed {
    pub path: String,
    pub bytes: Vec<u8>,
}

/// The program a page runs: the name it is run by, which is its first argument and the page's title, the package whose root the page's root is — none for a program no package declares — its module, and its rows.
pub struct Program<'a> {
    pub name: &'a str,
    pub package: Option<&'a str>,
    pub module: Vec<u8>,
    pub foreigns: &'a ForeignStore,
}

/// The page that runs `program`, linking `linked`, as every file it holds by its path — or why no page holds them: a path two files claim.
pub fn page(
    program: Program<'_>,
    linked: Vec<Linked>,
) -> Result<BTreeMap<String, Vec<u8>>, String> {
    let mut claimed = Claims::default();
    for file in PAGE_FILES {
        claimed.reserve(file);
    }

    let mut imported = Vec::new();
    for Linked {
        package,
        row,
        gateway,
        answers,
        statics,
    } in linked
    {
        let owned = |path: &str| match program.package == Some(package.as_str()) {
            true => path.to_string(),
            false => format!("{package}/{path}"),
        };
        let of = format!("the foreign row {row:?} of {package}");

        let path = owned(&gateway.path);
        claimed.claim(&path, format!("the gateway of {of}"), gateway.bytes)?;
        imported.push(Imported {
            specifier: json(&relative(&path)),
            described: json(&Described {
                row: &row,
                package: &package,
                path: &path,
                answers: &answers,
            }),
        });

        for (entry, file) in statics {
            claimed.claim(
                &owned(&file.path),
                format!("the static entry {entry:?} of {of}"),
                file.bytes,
            )?;
        }
    }

    let mut files = claimed.files();
    let integrity = files
        .iter()
        .filter(|(path, _)| path.ends_with(".js") || path.ends_with(".mjs"))
        .map(|(path, bytes)| (relative(path), subresource(bytes)))
        .collect::<BTreeMap<_, _>>();

    let loader = Loader {
        harness: HARNESS,
        name: json(program.name),
        configuration: configuration(),
        rows: rows(program.foreigns),
        bridge: json(&STANDARD.encode(bridge_bytes())),
        integrity: json(&subresource(&program.module)),
        program: json(INDEX_WASM),
        output: json(OUTPUT),
        gateways: imported,
    }
    .render()
    .expect("the loader renders")
    .into_bytes();

    let document = Document {
        name: program.name,
        import_map: json(&ImportMap { integrity }),
        loader: INDEX_JS,
        integrity: subresource(&loader),
        output: OUTPUT,
    }
    .render()
    .expect("the page renders")
    .into_bytes();

    files.insert(INDEX_HTML.to_string(), document);
    files.insert(INDEX_JS.to_string(), loader);
    files.insert(INDEX_WASM.to_string(), program.module);

    Ok(files)
}

/// Each path of a page beside what claims it and what it holds; the page's own files are claimed with nothing in them until the rest are placed, since what they hold is computed from the rest.
#[derive(Default)]
struct Claims(BTreeMap<String, (String, Option<Vec<u8>>)>);

impl Claims {
    fn reserve(&mut self, path: &str) {
        self.0
            .insert(path.to_string(), ("the page itself".to_string(), None));
    }

    fn claim(&mut self, path: &str, claimant: String, bytes: Vec<u8>) -> Result<(), String> {
        match self.0.entry(path.to_string()) {
            Entry::Vacant(vacant) => {
                vacant.insert((claimant, Some(bytes)));

                Ok(())
            }
            Entry::Occupied(occupied) => Err(format!(
                "the page path `{path}` is claimed by {} and by {claimant}; a page holds one file at a path",
                occupied.get().0
            )),
        }
    }

    /// Every claimed file, the page's own left out.
    fn files(self) -> BTreeMap<String, Vec<u8>> {
        self.0
            .into_iter()
            .filter_map(|(path, (_, bytes))| Some((path, bytes?)))
            .collect()
    }
}

#[derive(Template)]
#[template(path = "index.js", escape = "none")]
struct Loader {
    harness: &'static str,
    name: String,
    configuration: String,
    rows: String,
    bridge: String,
    integrity: String,
    program: String,
    output: String,
    gateways: Vec<Imported>,
}

/// One gateway as the loader imports it: the specifier, and what the loader knows of it, as JSON.
struct Imported {
    specifier: String,
    described: String,
}

#[derive(Serialize)]
struct Described<'a> {
    row: &'a str,
    package: &'a str,
    path: &'a str,
    answers: &'a [(String, String)],
}

#[derive(Template)]
#[template(path = "index.html")]
struct Document<'a> {
    name: &'a str,
    import_map: String,
    loader: &'a str,
    integrity: String,
    output: &'a str,
}

#[derive(Serialize)]
struct ImportMap {
    integrity: BTreeMap<String, String>,
}

/// `value` as JSON.
fn json(value: &(impl Serialize + ?Sized)) -> String {
    serde_json::to_string(value).expect("plain data renders as JSON")
}

/// The subresource integrity of `bytes`: their SHA-256, in base64, after `sha256-`.
fn subresource(bytes: &[u8]) -> String {
    format!("sha256-{}", STANDARD.encode(Digest::of(bytes).as_bytes()))
}

/// The page path `path` as a URL relative to the page's root: `./` ahead of what [`encoded`] spells, so an import reads it as a path rather than as a name the import map would have to resolve.
fn relative(path: &str) -> String {
    format!("./{}", encoded(path))
}

/// The page path `path` as a URL's path spells it: every byte a path segment does not carry as itself percent-encoded — `<` among them, so a path spelling `</script` ends no `<script>` it is written in — and `/` between segments as itself. A server decodes a request's path back to the page path it names.
pub fn encoded(path: &str) -> String {
    let mut url = String::new();

    for byte in path.bytes() {
        match byte {
            b'A'..=b'Z'
            | b'a'..=b'z'
            | b'0'..=b'9'
            | b'-'
            | b'.'
            | b'_'
            | b'~'
            | b'/'
            | b'!'
            | b'$'
            | b'&'
            | b'\''
            | b'('
            | b')'
            | b'*'
            | b'+'
            | b','
            | b';'
            | b'='
            | b':'
            | b'@' => url.push(char::from(byte)),
            _ => url.push_str(&format!("%{byte:02X}")),
        }
    }

    url
}

/// The `Content-Type` a server gives the file at the page path `path`, by its extension, whatever its case: the page's own files', the JavaScript type a module script requires for `.js` and `.mjs`, and the common types a `static` entry serves, with `application/octet-stream` for the rest.
pub fn content_type(path: &str) -> &'static str {
    const TYPES: &[(&str, &str)] = &[
        ("html", "text/html; charset=utf-8"),
        ("js", "text/javascript; charset=utf-8"),
        ("mjs", "text/javascript; charset=utf-8"),
        ("wasm", "application/wasm"),
        ("css", "text/css; charset=utf-8"),
        ("json", "application/json"),
        ("map", "application/json"),
        ("txt", "text/plain; charset=utf-8"),
        ("svg", "image/svg+xml"),
        ("png", "image/png"),
        ("jpg", "image/jpeg"),
        ("jpeg", "image/jpeg"),
        ("gif", "image/gif"),
        ("webp", "image/webp"),
        ("avif", "image/avif"),
        ("ico", "image/x-icon"),
        ("woff", "font/woff"),
        ("woff2", "font/woff2"),
        ("ttf", "font/ttf"),
        ("otf", "font/otf"),
        ("mp3", "audio/mpeg"),
        ("ogg", "audio/ogg"),
        ("wav", "audio/wav"),
        ("mp4", "video/mp4"),
        ("webm", "video/webm"),
        ("pdf", "application/pdf"),
    ];

    let name = path.rsplit('/').next().unwrap_or(path);

    name.rsplit_once('.')
        .and_then(|(_, extension)| {
            TYPES
                .iter()
                .find(|(known, _)| known.eq_ignore_ascii_case(extension))
        })
        .map_or("application/octet-stream", |(_, content)| content)
}
