//! What a page is made of, prepared in Rust so the templates hold loops and conditionals alone: every href is resolved against the page's depth, every anchor named, every badge and keyword decided here, and a template only places them. A signature arrives as segments rather than text, because the marks the record carries are byte ranges and a template cannot slice — and because the segments are what let the template escape every piece itself, so no string reaches a page unescaped.

use {
    super::{Bundle, Paragraph, Span, paragraphs, spans},
    crate::{Block, Declaration, Kind, Marked, Member, ModuleDocumentation, Signature},
    askama::Template,
    curios_common::{Class, Keyword, Qualifier},
    std::collections::BTreeMap,
};

/// The version every page's footer names: the compiler's.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// One module's page, the root's doubling as the landing page.
#[derive(Template)]
#[template(path = "page.html")]
pub struct Page {
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
    version: &'static str,
}

/// One segment of the heading, which is the path the page sits under: a link for every module above it, plain text for the page itself.
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
    /// What closes the block, as the segments it is spelled by: the keyword `end` after constructors, the plain `}` after fields or methods — punctuation, like the opener, and set apart no more than it is. Empty when nothing closes it.
    closer: Vec<Segment>,
    prose: Vec<Paragraph>,
    /// The page this declaration is written on, when that is another page of this bundle — what the header says it was re-exported from.
    origin: Option<Origin>,
    /// What kind of thing a declaration adopted out of a root a consumer cannot name is. Never a path: the root has no name a reader may write, and the word is all a card can honestly say.
    chip: Option<String>,
}

/// Where a re-exported declaration is written: the path a consumer writes for it, and the link that reaches it.
struct Origin {
    text: String,
    href: Option<String>,
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

/// A piece of a printed signature: text, or a span the printer set apart, painted by its class's name and linked where it names a declaration in this bundle.
enum Segment {
    Text(String),
    Marked {
        class: &'static str,
        text: String,
        href: Option<String>,
    },
}

/// One `satisfy` on a page: its head with its links, whether the compiler wrote its body, and the note written above it.
struct WitnessRow {
    anchor: String,
    signature: Vec<Segment>,
    derived: bool,
    prose: Vec<Paragraph>,
}

/// The page of `module` in `bundle`, with every link resolved from where the page sits.
pub fn page(bundle: &Bundle<'_>, module: &ModuleDocumentation) -> Page {
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
        version: VERSION,
    }
}

fn card(bundle: &Bundle<'_>, depth: usize, declaration: &Declaration) -> Card {
    // A concept's private representation is the language's sealing, so its badge says so.
    let badge = match (declaration.kind, declaration.opaque) {
        (Kind::Inductive | Kind::Structure, true) => Some(Badge {
            label: "opaque",
            tone: "dashed",
        }),
        (Kind::Inductive | Kind::Structure, false) => Some(Badge {
            label: "transparent",
            tone: "plain",
        }),
        (Kind::Concept, true) => Some(Badge {
            label: "sealed",
            tone: "dashed",
        }),
        (Kind::Concept, false) => Some(Badge {
            label: "unsealed",
            tone: "plain",
        }),
        (Kind::Definition | Kind::Witness | Kind::Foreign, _) => None,
    };
    let badges = badge.into_iter().collect();
    let members = declaration
        .members
        .iter()
        .map(|member| member_row(bundle, depth, &declaration.name, member))
        .collect();

    // The block as the source writes it, where the representation is shown: an inductive's constructors each after a bar and closed by `end`, a structure's fields and a concept's methods in braces, each with its comma. A sealed concept still lists its methods.
    let (layout, opener, lead, trail, closer) =
        match Block::of(declaration.kind, declaration.opaque) {
            Some(block) => (
                match block.indented {
                    true => "fields",
                    false => "cases",
                },
                block.opener,
                block.lead,
                block.trail,
                // A closer that is a word of the language is set apart as one, and punctuation stays plain.
                vec![match Keyword::read(block.closer) {
                    Some(keyword) => Segment::Marked {
                        class: keyword.class().name(),
                        text: block.closer.to_string(),
                        href: None,
                    },
                    None => Segment::Text(block.closer.to_string()),
                }],
            ),
            None => ("", "", "", "", Vec::new()),
        };

    let origin = declaration.source.as_ref().map(|source| Origin {
        text: source.join(),
        href: bundle.href(depth, source),
    });

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
        origin,
        chip: declaration.chip.clone(),
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
        plain(&signature.text[at..mark.start], &mut segments);
        let text = signature.text[mark.start..mark.end].to_string();
        segments.push(match &mark.marked {
            Marked::Word(class) => Segment::Marked {
                class: class.name(),
                text,
                href: None,
            },
            // A name the compilation does not class keeps the page's own word for a name.
            Marked::Name {
                class,
                referent,
                within,
            } => Segment::Marked {
                class: class.map_or("name", Class::name),
                text,
                href: within.then(|| bundle.href(depth, referent)).flatten(),
            },
        });
        at = mark.end;
    }
    plain(&signature.text[at..], &mut segments);
    segments
}

/// The keyword a declaration is written with.
pub fn keyword(kind: Kind) -> &'static str {
    match kind {
        Kind::Definition => Keyword::Let,
        Kind::Inductive => Keyword::Induct,
        Kind::Structure => Keyword::Struct,
        Kind::Concept => Keyword::Concept,
        Kind::Witness => Keyword::Satisfy,
        Kind::Foreign => Keyword::Foreign,
    }
    .spelling()
}

/// What a member of a declaration of `kind` is called in the index: a constructor a case, a field a field, a concept's method a method. The other kinds expose no members, so the word for one is never shown.
pub fn member_kind(kind: Kind) -> &'static str {
    match kind {
        Kind::Inductive => "case",
        Kind::Structure => "field",
        Kind::Concept => "method",
        Kind::Definition | Kind::Witness | Kind::Foreign => "member",
    }
}

/// How many declarations of each kind, in the order of [`Kind`], skipping the kinds with none.
fn counts(declarations: &[Declaration]) -> String {
    let mut tally = BTreeMap::new();
    for declaration in declarations {
        *tally.entry(declaration.kind).or_insert(0usize) += 1;
    }
    tally
        .into_iter()
        // A count and its word never part at a line's end.
        .map(|(kind, count)| format!("{count}\u{a0}{}", keyword(kind)))
        .collect::<Vec<_>>()
        .join(" · ")
}

/// `text` as a plain segment, joined to the plain segment before it when there is one.
fn plain(text: &str, out: &mut Vec<Segment>) {
    if text.is_empty() {
        return;
    }
    match out.last_mut() {
        Some(Segment::Text(before)) => before.push_str(text),
        _ => out.push(Segment::Text(text.to_string())),
    }
}

#[cfg(test)]
mod tests;
