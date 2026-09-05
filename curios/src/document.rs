//! The pages `curios document` writes from a [`Documentation`] record: a landing page, one page per module at its source path, and one stylesheet. Pages are static, read from `file://`, fetch nothing, and carry no script.
//!
//! **This is the placeholder rendering.** The markup is headings and lists — enough to read an interface and follow its links — and the design of the pages is a decision this module does not make yet. What it does decide is the layout of the bundle and the addressing: where a module's page is, what a declaration's anchor is, and how a link from one page reaches another, since every later design renders the same record into the same places.

use {
    curios_text::{Declaration, Documentation, Kind, Member, ModuleDocumentation, Signature},
    curios_utilities::Qualifier,
    std::{fmt::Write, fs, io, path::Path},
};

/// The stylesheet every page links, filed under `static/`.
const STYLESHEET: &str = "static/style.css";

/// Write `record`'s pages under `directory`: `index.html`, one page per module, and the stylesheet. Files are overwritten by name and nothing else in the directory is touched. The record is the whole input: its prefix names the landing page and its description fills it, so a record read off a stored unit renders exactly as one read off a compilation just made.
pub fn write_documentation(record: &Documentation, directory: &Path) -> io::Result<()> {
    let bundle = Bundle { record };

    fs::create_dir_all(directory.join("static"))?;
    fs::write(directory.join(STYLESHEET), STYLE)?;
    fs::write(directory.join("index.html"), bundle.landing())?;

    for module in &record.modules {
        let path = directory.join(bundle.page_path(&module.path));
        if let Some(parent) = path.parent() {
            fs::create_dir_all(parent)?;
        }
        fs::write(path, bundle.page(module))?;
    }

    Ok(())
}

/// The record with the addressing every page shares.
struct Bundle<'a> {
    record: &'a Documentation,
}

impl Bundle<'_> {
    /// Where a module's page is, relative to the bundle: the module's path under the unit's prefix with `.html`, so `/json/parse/lexer` is `parse/lexer.html`. The root has no path of its own under the prefix and takes `lib.html`, the stem every package's library header has.
    fn page_path(&self, module: &Qualifier) -> String {
        let below = &module.segments()[self.record.prefix.segments().len()..];
        match below.is_empty() {
            true => "lib.html".to_string(),
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

    fn landing(&self) -> String {
        // The unit's name is its mount's one segment: the package's name, or `std`.
        let name = self.record.prefix.last();
        let mut html = String::new();
        head(&mut html, name, 0);
        let _ = writeln!(html, "<h1>{}</h1>", escape(name));
        if let Some(description) = &self.record.description {
            let _ = writeln!(html, "<p>{}</p>", escape(description));
        }
        html.push_str("<h2>Modules</h2>\n<ul>\n");
        for module in &self.record.modules {
            let _ = writeln!(
                html,
                "<li><a href=\"{}\">{}</a></li>",
                self.page_path(&module.path),
                escape(&module.path.join())
            );
        }
        html.push_str("</ul>\n");
        tail(&mut html);
        html
    }

    fn page(&self, module: &ModuleDocumentation) -> String {
        let depth = self.depth(&module.path);
        let mut html = String::new();
        head(&mut html, &module.path.join(), depth);
        let _ = writeln!(
            html,
            "<p><a href=\"{}index.html\">{}</a></p>",
            "../".repeat(depth),
            escape(&self.record.prefix.join())
        );
        let _ = writeln!(html, "<h1>{}</h1>", escape(&module.path.join()));
        prose(&mut html, module.prose.as_deref());

        if !module.children.is_empty() {
            html.push_str("<h2>Modules</h2>\n<ul>\n");
            for child in &module.children {
                let _ = writeln!(
                    html,
                    "<li><a href=\"{}{}\">{}</a></li>",
                    "../".repeat(depth),
                    self.page_path(child),
                    escape(child.last())
                );
            }
            html.push_str("</ul>\n");
        }

        if !module.declarations.is_empty() {
            html.push_str("<h2>Declarations</h2>\n<ul>\n");
            let mut witnesses = 0;
            for declaration in &module.declarations {
                self.declaration(&mut html, depth, declaration, &mut witnesses);
            }
            html.push_str("</ul>\n");
        }

        if !module.reexports.is_empty() {
            html.push_str("<h2>Re-exports</h2>\n<ul>\n");
            for reexport in &module.reexports {
                let target = escape(&reexport.referent.join());
                match reexport
                    .within
                    .then(|| self.href(depth, &reexport.referent))
                    .flatten()
                {
                    Some(href) => {
                        let _ = writeln!(
                            html,
                            "<li><code>{}</code> <a href=\"{href}\">{target}</a></li>",
                            escape(&reexport.name)
                        );
                    }
                    None => {
                        let _ = writeln!(
                            html,
                            "<li><code>{}</code> <code>{target}</code></li>",
                            escape(&reexport.name)
                        );
                    }
                }
            }
            html.push_str("</ul>\n");
        }

        tail(&mut html);
        html
    }

    fn declaration(
        &self,
        html: &mut String,
        depth: usize,
        declaration: &Declaration,
        witnesses: &mut usize,
    ) {
        // A witness is anonymous, so its anchor is its position among the module's witnesses.
        let anchor = match declaration.kind {
            Kind::Witness => {
                *witnesses += 1;
                format!("satisfy-{witnesses}")
            }
            _ => declaration.name.clone(),
        };
        let _ = write!(html, "<li id=\"{}\"><code>", escape(&anchor));
        self.signature(html, depth, &declaration.signature);
        html.push_str("</code>");
        if declaration.derived {
            html.push_str(" <em>derived</em>");
        }
        html.push('\n');
        prose(html, declaration.prose.as_deref());

        if !declaration.members.is_empty() {
            html.push_str("<ul>\n");
            for member in &declaration.members {
                self.member(html, depth, &declaration.name, member);
            }
            html.push_str("</ul>\n");
        }
        html.push_str("</li>\n");
    }

    fn member(&self, html: &mut String, depth: usize, owner: &str, member: &Member) {
        let _ = write!(
            html,
            "<li id=\"{}\"><code>",
            escape(&format!("{owner}/{}", member.name))
        );
        self.signature(html, depth, &member.signature);
        html.push_str("</code>\n");
        prose(html, member.prose.as_deref());
        html.push_str("</li>\n");
    }

    /// The signature's text with every mark within the unit made a link, and every other name left as written.
    fn signature(&self, html: &mut String, depth: usize, signature: &Signature) {
        let mut at = 0;
        for mark in &signature.marks {
            html.push_str(&escape(&signature.text[at..mark.start]));
            let name = escape(&signature.text[mark.start..mark.end]);
            match mark
                .within
                .then(|| self.href(depth, &mark.referent))
                .flatten()
            {
                Some(href) => {
                    let _ = write!(html, "<a href=\"{href}\">{name}</a>");
                }
                None => html.push_str(&name),
            }
            at = mark.end;
        }
        html.push_str(&escape(&signature.text[at..]));
    }
}

/// The opening of every page: a title, the stylesheet at the right depth, and the body's start.
fn head(html: &mut String, title: &str, depth: usize) {
    let _ = writeln!(
        html,
        "<!doctype html>\n<html lang=\"en\">\n<head>\n<meta charset=\"utf-8\">\n<title>{}</title>\n<link rel=\"stylesheet\" href=\"{}{STYLESHEET}\">\n</head>\n<body>",
        escape(title),
        "../".repeat(depth)
    );
}

fn tail(html: &mut String) {
    html.push_str("</body>\n</html>\n");
}

/// A documentation comment as paragraphs: an empty line separates them, and every line is text.
fn prose(html: &mut String, lines: Option<&[String]>) {
    let Some(lines) = lines else {
        return;
    };
    for paragraph in lines.split(|line| line.is_empty()) {
        if paragraph.is_empty() {
            continue;
        }
        let _ = writeln!(html, "<p>{}</p>", escape(&paragraph.join(" ")));
    }
}

/// Text made safe inside an element or an attribute.
fn escape(text: &str) -> String {
    let mut escaped = String::with_capacity(text.len());
    for char in text.chars() {
        match char {
            '&' => escaped.push_str("&amp;"),
            '<' => escaped.push_str("&lt;"),
            '>' => escaped.push_str("&gt;"),
            '"' => escaped.push_str("&quot;"),
            other => escaped.push(other),
        }
    }
    escaped
}

/// The one stylesheet, as much of a placeholder as the markup.
const STYLE: &str = "body { font-family: sans-serif; max-width: 60rem; margin: 2rem auto; padding: 0 1rem; }\ncode { font-family: monospace; }\nli { margin: 0.5rem 0; }\n";
