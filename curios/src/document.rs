//! The pages `curios document` writes from a [`Documentation`] record: one page per module at its source path, the root's doubling as the landing page, and the static files every page shares. Pages are static, read from `file://`, fetch nothing, and carry no script.
//!
//! **The templates hold the markup and the contexts hold the facts.** Each page is an Askama template under `templates/`, compiled into this crate, over a context [`context`] prepares from the record: every href, anchor, badge and keyword is decided in Rust, and a template loops, branches and escapes. What this module decides is the layout of the bundle and its addressing — where a module's page is, what a declaration's anchor is, and how a link from one page reaches another — because every page, whatever its design, renders into the same places. The stylesheet, the fonts and the mark are embedded with `include_bytes!` and written under `static/`, so a bundle is complete on disk and a binary needs nothing beside it.
//!
//! **The root module's page is the landing page.** A unit's root declares things of its own — `/std/print` is one — and a landing page that listed the modules and hid the root's declarations behind a second page sent every link to that page. So `index.html` is the root's page, opening with the unit's description and the module cards before the root's own declarations, and every other module's page sits at its path below.

mod context;

mod prose;
use prose::*;

use {
    askama::Template,
    curios_text::Documentation,
    curios_utilities::Qualifier,
    std::{fs, io, path::Path},
};

/// What every bundle carries under `static/`, by the path it is written at.
const STATIC: &[(&str, &[u8])] = &[
    ("style.css", include_bytes!("../static/style.css")),
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

/// Write `record`'s pages under `directory`: `index.html` for the root, one page per other module, and the static files. Files are overwritten by name and nothing else in the directory is touched. The record is the whole input: its prefix names the pages and its description opens the landing page, so a record read off a stored unit renders exactly as one read off a compilation just made.
pub fn write_documentation(record: &Documentation, directory: &Path) -> io::Result<()> {
    let bundle = Bundle { record };

    fs::create_dir_all(directory.join("static").join("fonts"))?;
    for (name, bytes) in STATIC {
        fs::write(directory.join("static").join(name), bytes)?;
    }

    for module in &record.modules {
        let path = directory.join(bundle.page_path(&module.path));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let page = bundle.page(module).render().map_err(io::Error::other)?;
        fs::write(path, page)?;
    }

    Ok(())
}

/// The record with the addressing every page shares.
struct Bundle<'a> {
    record: &'a Documentation,
}

impl Bundle<'_> {
    /// Where a module's page is, relative to the bundle: the module's path under the unit's prefix with `.html`, so `/json/parse/lexer` is `parse/lexer.html`. The root has no path of its own under the prefix and is the landing page, `index.html`.
    fn page_path(&self, module: &Qualifier) -> String {
        let below = &module.segments()[self.record.prefix.segments().len()..];
        match below.is_empty() {
            true => "index.html".to_string(),
            false => format!("{}.html", below.join("/")),
        }
    }

    /// How many directories below the bundle a module's page sits, which is how many `../` a link from it climbs.
    fn depth(&self, module: &Qualifier) -> usize {
        (module.segments().len() - self.record.prefix.segments().len()).saturating_sub(1)
    }

    /// The page and anchor a referent within the unit is found at: the page of the longest module path containing it, and the rest of its path as the anchor — a declaration's name, or `Type/constructor` for a member. A referent that is a module links to the module's page itself.
    fn address(&self, referent: &Qualifier) -> Option<(String, Option<String>)> {
        let module = self
            .record
            .modules
            .iter()
            .filter(|module| referent.is_within(&module.path))
            .max_by_key(|module| module.path.segments().len())?;
        let page = self.page_path(&module.path);
        let rest = &referent.segments()[module.path.segments().len()..];
        match rest.is_empty() {
            true => Some((page, None)),
            false => Some((page, Some(rest.join("/")))),
        }
    }

    /// A link from the page at `depth` to `referent`, or `None` when the referent has no page in this bundle.
    fn href(&self, depth: usize, referent: &Qualifier) -> Option<String> {
        let (page, anchor) = self.address(referent)?;
        let climb = "../".repeat(depth);
        Some(match anchor {
            Some(anchor) => format!("{climb}{page}#{anchor}"),
            None => format!("{climb}{page}"),
        })
    }
}
