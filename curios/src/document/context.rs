//! What a page is made of, prepared in Rust so the templates hold loops and conditionals alone: every href is resolved against the page's depth, every anchor named, every badge and keyword decided here, and a template only places them. A signature arrives as segments rather than text, because the marks the record carries are byte ranges and a template cannot slice — and because the segments are what let the template escape every piece itself, so no string reaches a page unescaped.

use {
    super::{Bundle, Paragraph, Span, paragraphs, spans},
    askama::Template,
    curios_text::{Declaration, Kind, Member, ModuleDocumentation, Signature},
    curios_utilities::Qualifier,
    std::collections::BTreeMap,
};

/// The version every page's footer names: the compiler's.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// The words a signature sets apart from names and binders.
const KEYWORDS: &[&str] = &[
    "pub", "let", "induct", "struct", "concept", "satisfy", "foreign", "use", "mod",
];

/// One module's page, the root's doubling as the landing page.
#[derive(Template)]
#[template(path = "page.html")]
pub(super) struct Page {
    /// `../` per directory below the bundle: what every link to the bundle's root is prefixed with.
    pub(super) root: String,
    /// The module's path, `/std/Option`; the landing page's is the prefix itself.
    pub(super) path: String,
    pub(super) landing: bool,
    /// The unit's description on the landing page, the module's prose elsewhere.
    pub(super) lead: Vec<Paragraph>,
    pub(super) crumbs: Vec<Crumb>,
    /// The whole module tree, the root first and each parent before its children.
    pub(super) rail: Vec<RailRow>,
    /// This page's declarations, listed in the rail under the current module.
    pub(super) contents: Vec<Entry>,
    /// The child modules, as cards.
    pub(super) modules: Vec<ModuleCard>,
    pub(super) cards: Vec<Card>,
    pub(super) reexports: Vec<ReexportRow>,
    pub(super) version: &'static str,
}

/// One segment of the path a page sits under: a link for every module above it, plain text for the page itself.
pub(super) struct Crumb {
    pub(super) text: String,
    pub(super) href: Option<String>,
}

/// One module in the rail's tree.
pub(super) struct RailRow {
    /// How far below the root the module is: what the row is indented by.
    pub(super) depth: usize,
    /// The root's whole path, and every other module's last segment.
    pub(super) name: String,
    pub(super) href: String,
    pub(super) current: bool,
}

/// One declaration in the rail, under the page's module.
pub(super) struct Entry {
    pub(super) keyword: &'static str,
    /// The declared name, or a witness's signature past the keyword, since a witness has no name.
    pub(super) name: String,
    pub(super) anchor: String,
}

/// One child module on its parent's page.
pub(super) struct ModuleCard {
    pub(super) path: String,
    pub(super) href: String,
    /// The first paragraph of the module's prose.
    pub(super) gloss: Option<Paragraph>,
    /// How many declarations of each kind it holds, `1 induct · 13 let`, or nothing for an empty module.
    pub(super) counts: String,
}

/// One declaration on a page.
pub(super) struct Card {
    pub(super) anchor: String,
    pub(super) keyword: &'static str,
    /// Empty for a witness.
    pub(super) name: String,
    pub(super) badges: Vec<Badge>,
    pub(super) signature: Vec<Segment>,
    pub(super) members: Vec<MemberRow>,
    pub(super) prose: Vec<Paragraph>,
}

/// A fact about a declaration the signature does not spell: whether a representation is public, and whether the compiler wrote a body.
pub(super) struct Badge {
    pub(super) label: &'static str,
    /// The class the stylesheet draws it with.
    pub(super) tone: &'static str,
}

/// One constructor, field or method under its declaration.
pub(super) struct MemberRow {
    pub(super) anchor: String,
    pub(super) signature: Vec<Segment>,
    pub(super) prose: Vec<Paragraph>,
}

/// A piece of a printed signature: text, a keyword, a link to a declaration in this bundle, or the name of one outside it.
pub(super) enum Segment {
    Text(String),
    Keyword(String),
    Link { href: String, text: String },
    Name(String),
}

/// A `pub use`: the name and where it leads, a link when the declaration has a page in this bundle.
pub(super) struct ReexportRow {
    pub(super) name: String,
    pub(super) target: String,
    pub(super) href: Option<String>,
}

impl Bundle<'_> {
    /// The page of `module`, with every link resolved from where the page sits.
    pub(super) fn page(&self, module: &ModuleDocumentation) -> Page {
        let record = self.record;
        let depth = self.depth(&module.path);
        let root = "../".repeat(depth);
        let landing = module.path == record.prefix;

        let lead = match landing {
            true => record
                .description
                .as_deref()
                .map(|description| Paragraph {
                    spans: spans(description),
                })
                .into_iter()
                .collect(),
            false => paragraphs(module.prose.as_deref()),
        };

        // The unit, then each module above this one, each a link; the page itself is plain.
        let prefix = record.prefix.segments().len();
        let mut crumbs = vec![Crumb {
            text: record.prefix.last().to_string(),
            href: (!landing).then(|| "index.html".to_string()),
        }];
        let below = &module.path.segments()[prefix..];
        for (index, segment) in below.iter().enumerate() {
            let last = index + 1 == below.len();
            let ancestor = Qualifier::from(&module.path.segments()[..prefix + index + 1]);
            crumbs.push(Crumb {
                text: segment.to_string(),
                href: (!last).then(|| self.page_path(&ancestor)),
            });
        }

        let rail = record
            .modules
            .iter()
            .map(|listed| {
                let depth = listed.path.segments().len() - prefix;
                RailRow {
                    depth,
                    name: match depth {
                        0 => listed.path.join(),
                        _ => listed.path.last().to_string(),
                    },
                    href: self.page_path(&listed.path),
                    current: listed.path == module.path,
                }
            })
            .collect();

        let mut witnesses = 0;
        let cards = module
            .declarations
            .iter()
            .map(|declaration| self.card(depth, declaration, &mut witnesses))
            .collect::<Vec<_>>();
        let contents = cards
            .iter()
            .zip(&module.declarations)
            .map(|(card, declaration)| Entry {
                keyword: card.keyword,
                name: match declaration.kind {
                    Kind::Witness => declaration
                        .signature
                        .text
                        .trim_start_matches("satisfy ")
                        .to_string(),
                    _ => declaration.name.clone(),
                },
                anchor: card.anchor.clone(),
            })
            .collect();

        let modules = module
            .children
            .iter()
            .map(|child| {
                let listed = record.modules.iter().find(|listed| &listed.path == child);
                ModuleCard {
                    path: child.join(),
                    href: self.page_path(child),
                    gloss: listed
                        .and_then(|listed| paragraphs(listed.prose.as_deref()).into_iter().next()),
                    counts: listed
                        .map(|listed| counts(&listed.declarations))
                        .unwrap_or_default(),
                }
            })
            .collect();

        let reexports = module
            .reexports
            .iter()
            .map(|reexport| ReexportRow {
                name: reexport.name.clone(),
                target: reexport.referent.join(),
                href: reexport
                    .within
                    .then(|| self.href(depth, &reexport.referent))
                    .flatten(),
            })
            .collect();

        Page {
            root,
            path: module.path.join(),
            landing,
            lead,
            crumbs,
            rail,
            contents,
            modules,
            cards,
            reexports,
            version: VERSION,
        }
    }

    fn card(&self, depth: usize, declaration: &Declaration, witnesses: &mut usize) -> Card {
        // A witness is anonymous, so its anchor is its position among the module's witnesses.
        let anchor = match declaration.kind {
            Kind::Witness => {
                *witnesses += 1;
                format!("satisfy-{witnesses}")
            }
            _ => declaration.name.clone(),
        };

        let mut badges = Vec::new();
        if matches!(
            declaration.kind,
            Kind::Inductive | Kind::Structure | Kind::Concept
        ) {
            badges.push(match declaration.opaque {
                true => Badge {
                    label: "opaque",
                    tone: "dashed",
                },
                false => Badge {
                    label: "transparent",
                    tone: "plain",
                },
            });
        }
        if declaration.derived {
            badges.push(Badge {
                label: "derived",
                tone: "dashed",
            });
        }

        let members = declaration
            .members
            .iter()
            .map(|member| self.member(depth, &declaration.name, member))
            .collect();

        Card {
            anchor,
            keyword: keyword(declaration.kind),
            name: declaration.name.clone(),
            badges,
            signature: self.segments(depth, &declaration.signature),
            members,
            prose: paragraphs(declaration.prose.as_deref()),
        }
    }

    fn member(&self, depth: usize, owner: &str, member: &Member) -> MemberRow {
        MemberRow {
            anchor: format!("{owner}/{}", member.name),
            signature: self.segments(depth, &member.signature),
            prose: paragraphs(member.prose.as_deref()),
        }
    }

    /// The signature cut at its marks: a mark within the unit is a link, one outside it a name, and the text between them is words, of which the keywords are set apart.
    fn segments(&self, depth: usize, signature: &Signature) -> Vec<Segment> {
        let mut segments = Vec::new();
        let mut at = 0;
        for mark in &signature.marks {
            words(&signature.text[at..mark.start], &mut segments);
            let text = signature.text[mark.start..mark.end].to_string();
            match mark
                .within
                .then(|| self.href(depth, &mark.referent))
                .flatten()
            {
                Some(href) => segments.push(Segment::Link { href, text }),
                None => segments.push(Segment::Name(text)),
            }
            at = mark.end;
        }
        words(&signature.text[at..], &mut segments);
        segments
    }
}

/// The keyword a declaration is written with.
fn keyword(kind: Kind) -> &'static str {
    match kind {
        Kind::Definition => "let",
        Kind::Inductive => "induct",
        Kind::Structure => "struct",
        Kind::Concept => "concept",
        Kind::Witness => "satisfy",
        Kind::Foreign => "foreign",
    }
}

/// How many declarations of each kind, in the order the keywords are listed, skipping the kinds with none.
fn counts(declarations: &[Declaration]) -> String {
    let mut tally = BTreeMap::new();
    for declaration in declarations {
        *tally.entry(keyword(declaration.kind)).or_insert(0usize) += 1;
    }
    KEYWORDS
        .iter()
        .filter_map(|keyword| tally.get(keyword).map(|count| format!("{count} {keyword}")))
        .collect::<Vec<_>>()
        .join(" · ")
}

/// `text` as segments, every whole word among [`KEYWORDS`] set apart and everything else text, joined to the text segment before it when there is one.
fn words(text: &str, out: &mut Vec<Segment>) {
    let mut rest = text;
    while !rest.is_empty() {
        let word_end = rest
            .find(|char: char| !(char.is_ascii_alphanumeric() || char == '_'))
            .unwrap_or(rest.len());
        let (piece, after) = match word_end {
            0 => rest.split_at(
                rest.find(|char: char| char.is_ascii_alphanumeric() || char == '_')
                    .unwrap_or(rest.len()),
            ),
            _ => rest.split_at(word_end),
        };
        match KEYWORDS.contains(&piece) {
            true => out.push(Segment::Keyword(piece.to_string())),
            false => match out.last_mut() {
                Some(Segment::Text(text)) => text.push_str(piece),
                _ => out.push(Segment::Text(piece.to_string())),
            },
        }
        rest = after;
    }
}

#[cfg(test)]
mod tests;
