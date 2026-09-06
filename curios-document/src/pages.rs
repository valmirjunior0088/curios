//! The pages `curios document` writes from a [`Documentation`] record: one page per module at its source path, the root's doubling as the landing page, the static files every page shares, and the search index every page loads. Pages are static, read from `file://`, fetch nothing, and are complete without script: the script scrolls the rail to the page's own module, since a new page's rail starts at the top, and shows the search field, which is hidden until it runs and ranks the index over every module, declaration and member of the bundle.
//!
//! **The index is a script, written from the record.** A page read from `file://` may load a script and may fetch nothing, so the corpus a search needs is `static/index.js`, one row per module, declaration and member — its kind, the path a mark names it by, and its address from the bundle's root — assigned to a global the page script reads. It is the one file under `static/` that depends on the record, and it is written beside the constant ones. The README says why this and not the alternatives.
//!
//! **The templates hold the markup and the contexts hold the facts.** Each page is an Askama template under `templates/`, compiled into this crate, over a context [`context`] prepares from the record: every href, anchor, badge and keyword is decided in Rust, and a template loops, branches and escapes. What this module decides is the layout of the bundle and its addressing — where a module's page is, what a declaration's anchor is, and how a link from one page reaches another — because every page, whatever its design, renders into the same places. Addresses are read off the record's declarations, each at the home its marks name it under, so a declaration a facade documents is linked where it is shown. The stylesheet, the fonts and the mark are embedded with `include_bytes!` and written under `static/`, so a bundle is complete on disk and a binary needs nothing beside it.
//!
//! **The root module's page is the landing page.** A unit's root declares things of its own — `/std/print` is one — and a landing page that listed the modules and hid the root's declarations behind a second page sent every link to that page. So `index.html` is the root's page, opening with the unit's description and the module cards before the root's own declarations, and every other module's page sits at its path below.

mod context;
use context::*;

mod prose;
use prose::*;

use {
    crate::Documentation,
    askama::Template,
    curios_utilities::Qualifier,
    std::{collections::BTreeMap, fs, io, path::Path},
};

/// What every bundle carries under `static/` unchanged, by the path it is written at; the index written beside them is the record's.
const STATIC: &[(&str, &[u8])] = &[
    ("style.css", include_bytes!("../static/style.css")),
    ("script.js", include_bytes!("../static/script.js")),
    ("mark.svg", include_bytes!("../static/mark.svg")),
    (
        "fonts/geist.woff2",
        include_bytes!("../static/fonts/geist.woff2"),
    ),
    (
        "fonts/geist-mono.woff2",
        include_bytes!("../static/fonts/geist-mono.woff2"),
    ),
    (
        "fonts/faustina-italic.woff2",
        include_bytes!("../static/fonts/faustina-italic.woff2"),
    ),
    (
        "fonts/OFL-geist.txt",
        include_bytes!("../static/fonts/OFL-geist.txt"),
    ),
    (
        "fonts/OFL-geist-mono.txt",
        include_bytes!("../static/fonts/OFL-geist-mono.txt"),
    ),
    (
        "fonts/OFL-faustina.txt",
        include_bytes!("../static/fonts/OFL-faustina.txt"),
    ),
];

/// Write `record`'s pages under `directory`: `index.html` for the root, one page per other module, the static files and the search index. Files are overwritten by name and nothing else in the directory is touched. The record is the whole input: its prefix names the pages and its description opens the landing page, so a record read off a stored unit renders exactly as one read off a compilation just made.
pub fn write_documentation(record: &Documentation, directory: &Path) -> io::Result<()> {
    let bundle = Bundle::new(record);

    fs::create_dir_all(directory.join("static").join("fonts"))?;
    for (name, bytes) in STATIC {
        fs::write(directory.join("static").join(name), bytes)?;
    }
    fs::write(directory.join("static").join("index.js"), bundle.index())?;

    for module in &record.modules {
        let path = directory.join(bundle.page_path(&module.path));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let rendered = page(&bundle, module).render().map_err(io::Error::other)?;
        fs::write(path, rendered)?;
    }

    Ok(())
}

/// The record with the addressing every page shares.
struct Bundle<'a> {
    record: &'a Documentation,
    /// Where every module, declaration and member of the record is found, by the path a mark names: a module's page, a declaration's page and anchor, a member's beneath its declaration. Read off the record rather than off the path, so a declaration a facade documents is found where the facade put it, not under a module that has no page; a path that names none of them has no address, and a link to it renders as its name.
    addresses: BTreeMap<Qualifier, (String, Option<String>)>,
}

impl<'a> Bundle<'a> {
    fn new(record: &'a Documentation) -> Self {
        let mut bundle = Bundle {
            record,
            addresses: BTreeMap::new(),
        };
        for module in &record.modules {
            let page = bundle.page_path(&module.path);
            bundle
                .addresses
                .entry(module.path.clone())
                .or_insert((page.clone(), None));
            for declaration in &module.declarations {
                if declaration.name.is_empty() {
                    continue;
                }
                let path = declaration.home.with(&declaration.name);
                for member in &declaration.members {
                    bundle.addresses.entry(path.with(&member.name)).or_insert((
                        page.clone(),
                        Some(format!("{}/{}", declaration.name, member.name)),
                    ));
                }
                bundle
                    .addresses
                    .entry(path)
                    .or_insert((page.clone(), Some(declaration.name.clone())));
            }
        }
        bundle
    }

    /// Where a module's page is, relative to the bundle: the module's path under the unit's prefix with `.crs.html`, so `/json/parse/lexer` is `parse/lexer.crs.html`. The root has no path of its own under the prefix and is the landing page, `index.html`; the suffix is what keeps a module named `index` from landing on it, and a module page from ever sharing a name with a static file.
    fn page_path(&self, module: &Qualifier) -> String {
        let below = &module.segments()[self.record.prefix.segments().len()..];
        match below.is_empty() {
            true => "index.html".to_string(),
            false => format!("{}.crs.html", below.join("/")),
        }
    }

    /// How many directories below the bundle a module's page sits, which is how many `../` a link from it climbs.
    fn depth(&self, module: &Qualifier) -> usize {
        (module.segments().len() - self.record.prefix.segments().len()).saturating_sub(1)
    }

    /// A link from the page at `depth` to `referent`, or `None` when nothing in this bundle is found at that path.
    fn href(&self, depth: usize, referent: &Qualifier) -> Option<String> {
        let (page, anchor) = self.addresses.get(referent)?;
        let climb = "../".repeat(depth);
        Some(match anchor {
            Some(anchor) => format!("{climb}{page}#{anchor}"),
            None => format!("{climb}{page}"),
        })
    }

    /// The search index, as the script every page loads: `window.curiosIndex`, an array of rows, each a kind, a path and an address from the bundle's root, in the record's order — every module, then under it every named declaration with its named members beneath it. A witness has no name and no row, and neither has the superclass constraint a concept lists among its members. Every string is a keyword, a qualifier or a page name, so escaping the two characters a JavaScript string cannot hold bare is all the quoting there is.
    fn index(&self) -> String {
        let mut rows = Vec::new();
        let mut row = |kind: &str, path: &Qualifier, address: String| {
            rows.push(format!(
                "[\"{}\",\"{}\",\"{}\"]",
                escape(kind),
                escape(&path.join()),
                escape(&address)
            ));
        };
        for module in &self.record.modules {
            let page = self.page_path(&module.path);
            row("mod", &module.path, page.clone());
            for declaration in &module.declarations {
                if declaration.name.is_empty() {
                    continue;
                }
                let path = declaration.home.with(&declaration.name);
                row(
                    keyword(declaration.kind),
                    &path,
                    format!("{page}#{}", declaration.name),
                );
                for member in &declaration.members {
                    if member.name.is_empty() {
                        continue;
                    }
                    row(
                        member_kind(declaration.kind),
                        &path.with(&member.name),
                        format!("{page}#{}/{}", declaration.name, member.name),
                    );
                }
            }
        }
        format!("window.curiosIndex=[{}];\n", rows.join(","))
    }
}

/// `text` as the body of a JavaScript string literal in double quotes.
fn escape(text: &str) -> String {
    text.replace('\\', "\\\\").replace('"', "\\\"")
}

#[cfg(test)]
mod tests;
