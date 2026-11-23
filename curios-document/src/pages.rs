//! The pages `curios document` writes from a [`Documentation`] record: one page per path that is a module, the root's doubling as the landing page, and the static files every page shares. Pages are static, read from `file://`, and fetch and run nothing: the landing page and the rail are the bundle's index, as the README says.
//!
//! **A page is the answer to its path.** A path is often a module and a declaration at once — `/std/Option` is the type its parent exposes and the module that holds what works on it — and its page opens on the declaration and then lists what the module exposes, each thing once ([A block and a page draw one answer](../../documentation/design/tooling/a-block-and-a-page-draw-one-answer.md)). So a declaration is drawn whole on one page, the page of its own path where that is a module too and otherwise the page of the module a consumer finds it in, and every other page that exposes it shows its source, the paragraph its note opens with and the way there.
//!
//! **The templates hold the markup and the contexts hold the facts.** Each page is an Askama template under `templates/`, compiled into this crate, over a context [`context`] prepares from the record: every href, anchor, fact and keyword is decided in Rust, and a template loops, branches and escapes. What this module decides is the layout of the bundle and its addressing — where a path's page is, what a declaration's anchor is, and how a link from one page reaches another — because every page, whatever its design, renders into the same places. Addresses are read off the record's declarations, each by the path its marks name it by, so a link finds a declaration where it is drawn whole. The stylesheet, the fonts and the mark are embedded with `include_bytes!` and written under `static/`, so a bundle is complete on disk and a binary needs nothing beside it.
//!
//! **The root module's page is the landing page.** A unit's root declares things of its own — `/std/print` is one — and a landing page that listed the modules and hid the root's declarations behind a second page would send every link to that page. So `index.html` is the root's page, opening with the unit's description before what the root exposes, and every other module's page sits at its path below.

mod context;
use context::*;

mod prose;
use prose::*;

use {
    crate::{Declaration, Documentation, Entry, Kind, ModuleDocumentation},
    askama::Template,
    curios_common::{Qualifier, write_whole},
    std::{collections::BTreeMap, fs, io, path::Path},
};

/// What every bundle carries under `static/` unchanged, by the path it is written at.
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

/// Write `record`'s pages under `directory`: `index.html` for the root, one page per other module and the static files. Files are overwritten by name and nothing else in the directory is touched. The record is the whole input: its prefix names the pages and its description opens the landing page, so a record read back from an image renders as the one a build just made.
pub fn write_documentation(record: &Documentation, directory: &Path) -> io::Result<()> {
    let bundle = Bundle::new(record);

    fs::create_dir_all(directory.join("static").join("fonts"))?;
    for (name, bytes) in STATIC {
        write_whole(&directory.join("static").join(name), bytes)?;
    }

    for module in &record.modules {
        let path = directory.join(bundle.page_path(&module.path));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        let rendered = page(&bundle, module).render().map_err(io::Error::other)?;
        write_whole(&path, rendered.as_bytes())?;
    }

    Ok(())
}

/// The record with the addressing every page shares.
struct Bundle<'a> {
    record: &'a Documentation,
    /// The record's modules, each by its path.
    modules: BTreeMap<Qualifier, &'a ModuleDocumentation>,
    /// The declaration each page opens on, by the page's path: the one the module above exposes under the module's own name. The root has none, which no module is above, and neither has a module that is no declaration.
    subjects: BTreeMap<Qualifier, &'a Declaration>,
    /// Where a mark's path is found: a module at its page, and a declaration where it is drawn whole, by the path every mark names it by, each of its constructors and methods beneath it. A path that names none of them has no address, and a link to it renders as its name. A field is no path a mark names, since the path of its owner and its label may be a function's, and it has none.
    addresses: BTreeMap<Qualifier, (String, Option<String>)>,
    /// The path of the page that opens on a declaration, by the path every mark names the declaration by: what a reader holds for it, `/std/Option` of the type the module `/std/Option` declares as `Option`.
    held: BTreeMap<Qualifier, Qualifier>,
}

impl<'a> Bundle<'a> {
    fn new(record: &'a Documentation) -> Self {
        let modules = record
            .modules
            .iter()
            .map(|module| (module.path, module))
            .collect::<BTreeMap<_, _>>();
        let subjects = record
            .modules
            .iter()
            .flat_map(|module| {
                module
                    .declarations()
                    .filter(|declaration| declaration.kind != Kind::Witness)
                    .map(move |declaration| (module.path.with(&declaration.name), declaration))
            })
            .filter(|(path, _)| modules.contains_key(path))
            .collect();
        let mut bundle = Bundle {
            record,
            modules,
            subjects,
            addresses: BTreeMap::new(),
            held: BTreeMap::new(),
        };

        // A module is its page, and the declaration the page opens on is whole there: the page is the address of both, where the declaration is the module's own or stands at the page's own path. One another module declares, exposed above a module that merely shares its name, keeps the address its own module's page gives it.
        for module in &record.modules {
            let page = bundle.page_path(&module.path);
            bundle.addresses.insert(module.path, (page, None));
        }
        for module in &record.modules {
            if let Some(subject) = bundle.subject(&module.path)
                && (subject.named == module.path || subject.named.without_last() == module.path)
            {
                bundle.address(&module.path, subject, true);
                bundle.held.entry(subject.named).or_insert(module.path);
            }
        }
        // Any other is whole on the page of the module a consumer finds it in.
        for module in &record.modules {
            for declaration in module.declarations() {
                let here = declaration.named == module.path.with(&declaration.name);
                if here && declaration.kind != Kind::Witness {
                    bundle.address(&module.path, declaration, false);
                }
            }
        }
        bundle
    }

    /// File where `declaration` is drawn whole, the page of `module`, as what the page opens on or as an entry of it, unless a page already draws it so.
    fn address(&mut self, module: &Qualifier, declaration: &Declaration, subject: bool) {
        let page = self.page_path(module);
        for member in &declaration.members {
            if member.name.is_empty() || declaration.kind == Kind::Structure {
                continue;
            }
            self.addresses
                .entry(declaration.named.with(&member.name))
                .or_insert((
                    page.clone(),
                    Some(member_anchor(declaration, member, subject)),
                ));
        }
        self.addresses.entry(declaration.named).or_insert((
            page,
            match subject {
                true => None,
                false => Some(declaration.name.clone()),
            },
        ));
    }

    /// The module at `path`, where the record has one.
    fn module(&self, path: &Qualifier) -> Option<&'a ModuleDocumentation> {
        self.modules.get(path).copied()
    }

    /// The declaration at `path`, which its page opens on.
    fn subject(&self, path: &Qualifier) -> Option<&'a Declaration> {
        self.subjects.get(path).copied()
    }

    /// The path a reader holds for the declaration every mark names `named`: the path of the page that opens on it, and its own where none does.
    fn held(&self, named: &Qualifier) -> Qualifier {
        self.held.get(named).copied().unwrap_or(*named)
    }

    /// What the page of `module` lists: what the module exposes, in its order, without its witnesses, which a page lists apart, and without the declaration the page opens on, where the module exposes that too.
    fn exposed(&self, module: &'a ModuleDocumentation) -> impl Iterator<Item = &'a Entry> {
        let subject = self.subject(&module.path).map(|subject| subject.named);

        module.entries.iter().filter(move |entry| match entry {
            Entry::Declaration(declaration) => {
                declaration.kind != Kind::Witness && Some(declaration.named) != subject
            }
            Entry::Exposure(_) => true,
        })
    }

    /// The child modules of `module` that are no declaration of it, in the order their `mod` lines are written.
    fn lone(&self, module: &'a ModuleDocumentation) -> impl Iterator<Item = &'a Qualifier> {
        module
            .children
            .iter()
            .filter(|child| self.subject(child).is_none())
    }

    /// The paths beneath `module` that have a page: each name it exposes that is a module too, in the order the module writes them, then its modules that are no declaration.
    fn doors(&self, module: &'a ModuleDocumentation) -> Vec<Qualifier> {
        self.exposed(module)
            .filter_map(|entry| match entry {
                Entry::Declaration(declaration) => Some(module.path.with(&declaration.name)),
                Entry::Exposure(_) => None,
            })
            .filter(|path| self.module(path).is_some())
            .chain(self.lone(module).copied())
            .collect()
    }

    /// How many names and modules `module` exposes beside the declaration its page opens on, `23 names · 2 modules`, a count of none unsaid.
    fn counts(&self, module: &'a ModuleDocumentation) -> String {
        let names = self
            .exposed(module)
            .map(|entry| match entry {
                Entry::Declaration(_) => 1,
                Entry::Exposure(line) => line.names.len(),
            })
            .sum::<usize>();
        let modules = self.lone(module).count();
        // A count and its word never part at a line's end.
        let counted = |count: usize, one: &str, many: &str| {
            let word = match count {
                1 => one,
                _ => many,
            };
            (count > 0).then(|| format!("{count}\u{a0}{word}"))
        };

        [
            counted(names, "name", "names"),
            counted(modules, "module", "modules"),
        ]
        .into_iter()
        .flatten()
        .collect::<Vec<_>>()
        .join(" · ")
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

    /// A link from the page at `depth` to `referent`, or `None` when nothing in this bundle is drawn whole at that path.
    fn href(&self, depth: usize, referent: &Qualifier) -> Option<String> {
        let (page, anchor) = self.addresses.get(referent)?;
        let climb = "../".repeat(depth);
        Some(match anchor {
            Some(anchor) => format!("{climb}{page}#{anchor}"),
            None => format!("{climb}{page}"),
        })
    }
}

#[cfg(test)]
mod tests;
