//! What a page is made of, prepared in Rust so the templates hold loops and conditionals alone: every href is resolved against the page's depth, every anchor named, every badge and keyword decided here, and a template only places them. A signature arrives as segments rather than text, because the marks the record carries are byte ranges and a template cannot slice — and because the segments are what let the template escape every piece itself, so no string reaches a page unescaped.

use {
    super::{Bundle, Paragraph, Span, paragraphs, spans},
    crate::{Declaration, Kind, Member, ModuleDocumentation, Signature},
    askama::Template,
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
    root: String,
    /// The module's path, `/std/Option`; the landing page's is the prefix itself.
    path: String,
    /// The unit's description on the landing page, the module's prose elsewhere.
    lead: Vec<Paragraph>,
    crumbs: Vec<Crumb>,
    /// The whole module tree, the root first and each parent before its children.
    rail: Vec<RailRow>,
    /// This page's declarations, listed in the rail under the current module.
    contents: Vec<Entry>,
    /// The child modules, as cards.
    modules: Vec<ModuleCard>,
    cards: Vec<Card>,
    /// The module's witnesses, in source order.
    witnesses: Vec<WitnessRow>,
    reexports: Vec<ReexportRow>,
    version: &'static str,
}

/// One segment of the path a page sits under: a link for every module above it, plain text for the page itself.
struct Crumb {
    text: String,
    href: Option<String>,
}

/// One module in the rail's tree.
struct RailRow {
    /// How far below the root the module is: what the row is indented by.
    depth: usize,
    /// The module's last segment — the root's too, as the crumb spells it: a mount's name is what the reader knows it by, and the leading `/` of its canonical spelling names nothing here.
    name: String,
    href: String,
    current: bool,
}

/// One named declaration in the rail, under the page's module. A witness has no name and is not listed.
struct Entry {
    keyword: &'static str,
    name: String,
    anchor: String,
}

/// One child module on its parent's page.
struct ModuleCard {
    path: String,
    href: String,
    /// The first paragraph of the module's prose.
    gloss: Option<Paragraph>,
    /// How many declarations of each kind it holds, `1 induct · 13 let`, or nothing for an empty module.
    counts: String,
}

/// One declaration on a page.
struct Card {
    anchor: String,
    keyword: &'static str,
    /// Empty for a witness.
    name: String,
    badges: Vec<Badge>,
    signature: Vec<Segment>,
    members: Vec<MemberRow>,
    /// How the members are laid out, as a class: `cases` for constructors flush with the head, `fields` for fields and methods indented under it, nothing for a declaration without a block.
    layout: &'static str,
    /// What the source writes after the head, before its members: ` {` for a structure or a concept.
    opener: &'static str,
    /// What precedes each member: `| ` before a constructor.
    lead: &'static str,
    /// What follows each member: `,` after a field or a method.
    trail: &'static str,
    /// What closes the block: `end` after constructors, `}` after fields or methods.
    closer: Option<&'static str>,
    prose: Vec<Paragraph>,
}

/// A fact about a declaration the signature does not spell: whether a representation is public, and whether the compiler wrote a body.
struct Badge {
    label: &'static str,
    /// The class the stylesheet draws it with.
    tone: &'static str,
}

/// One constructor, field or method under its declaration, or the superclass edge a concept lists among them.
struct MemberRow {
    /// `owner/member`, or none for an anonymous member, which nothing can link to.
    anchor: Option<String>,
    signature: Vec<Segment>,
    prose: Vec<Paragraph>,
}

/// A piece of a printed signature: text, a keyword, a link to a declaration in this bundle, or the name of one outside it.
enum Segment {
    Text(String),
    Keyword(String),
    Link { href: String, text: String },
    Name(String),
}

/// One `satisfy` on a page: its head with its links, whether the compiler wrote its body, and the note written above it.
struct WitnessRow {
    anchor: String,
    signature: Vec<Segment>,
    derived: bool,
    prose: Vec<Paragraph>,
}

/// A `pub use`: the name and where it leads, a link when the declaration has a page in this bundle.
struct ReexportRow {
    name: String,
    target: String,
    href: Option<String>,
}

/// The page of `module` in `bundle`, with every link resolved from where the page sits.
pub(super) fn page(bundle: &Bundle<'_>, module: &ModuleDocumentation) -> Page {
    let record = bundle.record;
    let depth = bundle.depth(&module.path);
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
            href: (!last).then(|| bundle.page_path(&ancestor)),
        });
    }

    let rail = record
        .modules
        .iter()
        .map(|listed| {
            let depth = listed.path.segments().len() - prefix;
            RailRow {
                depth,
                name: listed.path.last().to_string(),
                href: bundle.page_path(&listed.path),
                current: listed.path == module.path,
            }
        })
        .collect();

    // A witness is anonymous, so its anchor is its position among the module's witnesses; every other declaration's anchor is its name.
    let mut witnesses = Vec::new();
    let mut cards = Vec::new();
    let mut contents = Vec::new();
    for declaration in &module.declarations {
        match declaration.kind {
            Kind::Witness => {
                witnesses.push(WitnessRow {
                    anchor: format!("satisfy-{}", witnesses.len() + 1),
                    signature: segments(bundle, depth, &declaration.signature),
                    derived: declaration.derived,
                    prose: paragraphs(declaration.prose.as_deref()),
                });
            }
            _ => {
                let built = card(bundle, depth, declaration);
                contents.push(Entry {
                    keyword: built.keyword,
                    name: declaration.name.clone(),
                    anchor: built.anchor.clone(),
                });
                cards.push(built);
            }
        }
    }

    let modules = module
        .children
        .iter()
        .map(|child| {
            let listed = record.modules.iter().find(|listed| &listed.path == child);
            ModuleCard {
                path: child.join(),
                href: bundle.page_path(child),
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
                .then(|| bundle.href(depth, &reexport.referent))
                .flatten(),
        })
        .collect();

    Page {
        root,
        path: module.path.join(),
        lead,
        crumbs,
        rail,
        contents,
        modules,
        cards,
        witnesses,
        reexports,
        version: VERSION,
    }
}

fn card(bundle: &Bundle<'_>, depth: usize, declaration: &Declaration) -> Card {
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
    let members = declaration
        .members
        .iter()
        .map(|member| member_row(bundle, depth, &declaration.name, member))
        .collect();

    // The block as the source writes it, where the representation is shown: an inductive's constructors each after a bar and closed by `end`, a structure's fields and a concept's methods in braces, each with its comma. A sealed concept still lists its methods.
    let (layout, opener, lead, trail, closer) = match (declaration.kind, declaration.opaque) {
        (Kind::Inductive, false) => ("cases", "", "| ", "", Some("end")),
        (Kind::Structure, false) | (Kind::Concept, _) => ("fields", " {", "", ",", Some("}")),
        _ => ("", "", "", "", None),
    };

    Card {
        anchor: declaration.name.clone(),
        keyword: keyword(declaration.kind),
        name: declaration.name.clone(),
        badges,
        signature: segments(bundle, depth, &declaration.signature),
        members,
        layout,
        opener,
        lead,
        trail,
        closer,
        prose: paragraphs(declaration.prose.as_deref()),
    }
}

fn member_row(bundle: &Bundle<'_>, depth: usize, owner: &str, member: &Member) -> MemberRow {
    MemberRow {
        anchor: (!member.name.is_empty()).then(|| format!("{owner}/{}", member.name)),
        signature: segments(bundle, depth, &member.signature),
        prose: paragraphs(member.prose.as_deref()),
    }
}

/// The signature cut at its marks: a mark within the unit is a link, one outside it a name, and the text between them is words, of which the keywords are set apart.
fn segments(bundle: &Bundle<'_>, depth: usize, signature: &Signature) -> Vec<Segment> {
    let mut segments = Vec::new();
    let mut at = 0;
    for mark in &signature.marks {
        words(&signature.text[at..mark.start], &mut segments);
        let text = signature.text[mark.start..mark.end].to_string();
        match mark
            .within
            .then(|| bundle.href(depth, &mark.referent))
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

/// The keyword a declaration is written with.
pub(super) fn keyword(kind: Kind) -> &'static str {
    match kind {
        Kind::Definition => "let",
        Kind::Inductive => "induct",
        Kind::Structure => "struct",
        Kind::Concept => "concept",
        Kind::Witness => "satisfy",
        Kind::Foreign => "foreign",
    }
}

/// What a member of a declaration of `kind` is called in the index: a constructor a case, a field a field, a concept's method a method. The other kinds expose no members, so the word for one is never shown.
pub(super) fn member_kind(kind: Kind) -> &'static str {
    match kind {
        Kind::Inductive => "case",
        Kind::Structure => "field",
        Kind::Concept => "method",
        Kind::Definition | Kind::Witness | Kind::Foreign => "member",
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
