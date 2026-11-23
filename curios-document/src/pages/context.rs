//! What a page is made of, prepared in Rust so the templates hold loops and conditionals alone: every href is resolved against the page's depth, every anchor named, every fact and keyword decided here, and a template only places them. A signature arrives as lines of segments rather than text, because the marks the record carries are byte ranges and a template cannot slice — and because the segments are what let the template escape every piece itself, so no string reaches a page unescaped.
//!
//! **Everything a page says of one thing is one cluster.** The source that declares it, the facts its source does not show, its note and the way on hang on one trunk, as the parts of a report's block hang on theirs. A declaration is drawn whole where its address is: with each member's note and anchor, and all of its note. Anywhere else a module exposes it, its cluster is its source, the paragraph its note opens with, and the way to where it is whole — the page of the module its name is too, or the path that declares it.
//!
//! **An anchor is a name the page's entries cannot share.** An entry is anchored by its name and its member beneath it, `Shape/circle`, a field as a projection writes it, `Shape.radius`. The declaration a page opens on is the page itself and takes no anchor, and its members take theirs under a leading joint, `/circle` and `.radius`, which no name begins with; a witness is anchored by its head and the module that declares it, and the witnesses' sections by `satisfy`, which no declaration is named.

use {
    super::{Bundle, Paragraph, Span, paragraphs, spans, texts},
    crate::{
        Block, Declaration, Entry, ExposureLine, Kind, Marked, Member, ModuleDocumentation,
        Signature, Totality,
    },
    askama::Template,
    curios_common::{Class, Keyword, Qualifier},
};

/// The version every page's footer names: the compiler's.
const VERSION: &str = env!("CARGO_PKG_VERSION");

/// How many rows the list of what a page holds may have and still stand in view beside the page; a longer one stands at the top of the page, so that no list scrolls on its own.
const IN_VIEW: usize = 22;

/// The anchor of the section of witnesses the declaration a page opens on takes part in, or of a page's only section of them: the keyword, which no declaration is named.
const WITNESSES: &str = "satisfy";

/// The anchor of the section of the witnesses a module declares of anything else, where a page has both: no name, and no witness's head, which goes on from the keyword with a name.
const DECLARED: &str = "satisfy-here";

/// One path's page, the root's doubling as the landing page.
#[derive(Template)]
#[template(path = "page.html")]
pub struct Page {
    /// `../` per directory below the bundle: what every link to the bundle's root is prefixed with.
    root: String,
    /// The path, `/std/Option`; the landing page's is the prefix itself.
    path: String,
    /// The unit's name, as the heading's first segment spells it.
    unit: String,
    crumbs: Vec<Crumb>,
    /// The unit's description on the landing page, and elsewhere what the module says of itself, where the declaration the page opens on does not say the same.
    subtitle: Vec<Paragraph>,
    /// The declaration at the path, where the module above exposes one there.
    subject: Option<Cluster>,
    /// How many names and modules the path exposes, `23 names · 2 modules`.
    counts: String,
    /// What the path exposes, each by its name and the paragraph its note opens with, where it exposes more than one thing.
    brief: Vec<Brief>,
    /// What the path exposes in full, in the order its module writes it, the modules that are no declaration after.
    entries: Vec<Cluster>,
    /// The witnesses the declaration at the path takes part in, wherever the unit declares them, then the ones the module declares of anything else.
    sections: Vec<Section>,
    /// The unit's paths that have a page, the branch this page is on opened.
    rail: Vec<RailRow>,
    /// What the page holds, by name.
    holds: Vec<Held>,
    /// Whether the list of what the page holds is too long to stand in view ([`IN_VIEW`]).
    long: bool,
    version: &'static str,
}

/// One segment of the heading, which is the path the page sits under: a link for every path above it, plain text for the page itself.
struct Crumb {
    text: String,
    href: Option<String>,
}

/// One path in the rail.
struct RailRow {
    /// How far below the root's own paths the path is: what the row is indented by.
    depth: usize,
    /// The keyword of the declaration at the path, or `mod` where it is a module alone.
    keyword: &'static str,
    name: String,
    href: String,
    current: bool,
    /// Whether the page is at the path or beneath it: the paths beneath an open one are listed under it.
    open: bool,
}

/// One thing a page holds, in the list beside it.
struct Held {
    keyword: &'static str,
    name: String,
}

/// One thing a path exposes, in brief.
struct Brief {
    keyword: &'static str,
    name: String,
    /// The paragraph its note opens with.
    said: Option<Paragraph>,
}

/// Everything a page says of one thing: the source that declares it, the facts its source does not show, its note, and the way on.
struct Cluster {
    /// The name a link lands on; none for the declaration the page opens on, which is the page, and for a line that exposes members, which names nothing of its own.
    anchor: Option<String>,
    /// Whether the cluster is such a line, whose trunk is drawn quieter.
    exposure: bool,
    /// The head, each line as the printer laid it out.
    head: Vec<Line>,
    members: Vec<MemberBlock>,
    /// What closes the block, `end` or a brace; empty where the declaration writes no block.
    closer: Vec<Line>,
    facts: Vec<Fact>,
    prose: Vec<Paragraph>,
    /// The page of the module this name is too.
    door: Option<Door>,
    /// The path it is whole at, where this page shows it under another and that path's page is not its own module's.
    home: Option<Origin>,
}

/// One line of a signature: how far the printer indented it, and its segments, which begin past that indent.
struct Line {
    indent: usize,
    segments: Vec<Segment>,
}

impl Line {
    /// The spaces the printer indented the line by, which a page writes as they are, so that a signature copied off it is indented as its source is.
    fn lead(&self) -> String {
        " ".repeat(self.indent)
    }
}

/// One constructor, field or method in its declaration's block, or the superclass edge a concept lists among them.
struct MemberBlock {
    /// The name a link lands on ([`member_anchor`]); none for an anonymous member, and none where the block is shown apart from where its declaration is whole, which is where a member is linked.
    anchor: Option<String>,
    /// Whether the member stands indented under its head, as a field and a method do.
    deep: bool,
    lines: Vec<Line>,
    notes: Vec<Paragraph>,
}

/// A fact of a declaration its source does not show, or shows in one word easily passed over: the word a report says it in, how its pill is drawn, and what it means.
struct Fact {
    word: String,
    /// `plain` for a straight outline, `proof` for a squared one, nothing for a dashed one.
    drawing: &'static str,
    meaning: &'static str,
}

/// The way from a name to the page of the module it is too.
struct Door {
    /// The name, written after `pub mod`; none where the cluster is the module's own line, which already says it.
    name: Option<String>,
    counts: String,
    href: String,
}

/// A path a cluster points at, and the link that reaches it where the bundle draws it.
struct Origin {
    text: String,
    href: Option<String>,
}

/// A piece of a printed signature: text, a span the printer set apart, painted by its class's name and linked where it names a declaration in this bundle, or the name the signature declares. In a line ([`lines`]) a text ends at each of its spaces, which is where a page may fold the line.
enum Segment {
    Text(String),
    Marked {
        class: &'static str,
        text: String,
        href: Option<String>,
    },
    Declared(String),
}

/// One section of witnesses on a page.
struct Section {
    anchor: &'static str,
    title: &'static str,
    /// How many it lists, and of what: `12 in scope` for the witnesses a declaration takes part in, wherever they are declared.
    tally: String,
    rows: Vec<WitnessRow>,
}

/// One `satisfy` on a page: its head with its links, whether the compiler wrote its body, the note written above it, and the module that declares it, where the section lists the witnesses of more than one.
struct WitnessRow {
    anchor: String,
    lines: Vec<Line>,
    derived: bool,
    prose: Vec<Paragraph>,
    home: Option<Origin>,
}

/// The page of `module` in `bundle`, with every link resolved from where the page sits.
pub fn page(bundle: &Bundle<'_>, module: &ModuleDocumentation) -> Page {
    let record = bundle.record;
    let depth = bundle.depth(&module.path);
    let root = "../".repeat(depth);
    let landing = module.path == record.prefix;

    // The unit, then each path above this one, each a link; the page itself is plain.
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

    let subject = bundle.subject(&module.path);
    let subtitle = match (landing, subject) {
        (true, _) => record
            .description
            .as_deref()
            .map(|description| Paragraph {
                spans: spans(description),
            })
            .into_iter()
            .collect(),
        // A module's note and its declaration's are two notes of one path: the page says the first where the second does not open with it.
        (false, Some(subject))
            if texts(subject.prose.as_deref()).starts_with(&texts(module.prose.as_deref())) =>
        {
            Vec::new()
        }
        (false, _) => paragraphs(module.prose.as_deref()),
    };

    let mut entries = Vec::new();
    let mut brief = Vec::new();
    let mut holds = Vec::new();
    for listed in bundle.exposed(module) {
        match listed {
            Entry::Declaration(declaration) => {
                brief.push(Brief {
                    keyword: keyword(declaration.kind),
                    name: declaration.name.clone(),
                    said: opening(declaration.prose.as_deref()),
                });
                holds.push(Held {
                    keyword: keyword(declaration.kind),
                    name: declaration.name.clone(),
                });
                entries.push(entry(bundle, depth, module, declaration));
            }
            // The line stands among the entries where the module writes it, and names nothing a list holds.
            Entry::Exposure(line) => entries.push(exposure(bundle, depth, line)),
        }
    }
    // The modules that are no declaration stand after the names, as a block lists them.
    for child in bundle.lone(module) {
        let name = child.last().to_string();
        let said = bundle.module(child).and_then(|page| page.prose.as_deref());
        brief.push(Brief {
            keyword: Keyword::Mod.spelling(),
            name: name.clone(),
            said: opening(said),
        });
        holds.push(Held {
            keyword: Keyword::Mod.spelling(),
            name: name.clone(),
        });
        entries.push(lone(bundle, depth, child, name, said));
    }
    // One thing needs no list before it.
    if brief.len() < 2 {
        brief.clear();
    }

    // The witnesses the declaration at the path takes part in, wherever they are declared, then the ones this module declares of anything else.
    let takes_part = |declaration: &Declaration| {
        subject.is_some_and(|subject| declaration.parties.contains(&subject.named))
    };
    let witnesses = record
        .modules
        .iter()
        .flat_map(|page| {
            page.declarations()
                .map(move |declaration| (page, declaration))
        })
        .filter(|(_, declaration)| declaration.kind == Kind::Witness && takes_part(declaration))
        .map(|(page, declaration)| {
            let mut row = witness(bundle, depth, &page.path, declaration);
            // The section lists the witnesses of every module, so each row says its own.
            row.home = Some(Origin {
                text: page.path.join(),
                href: (page.path != module.path)
                    .then(|| format!("{root}{}#{}", bundle.page_path(&page.path), row.anchor)),
            });
            row
        })
        .collect::<Vec<_>>();
    let declared = module
        .declarations()
        .filter(|declaration| declaration.kind == Kind::Witness && !takes_part(declaration))
        .map(|declaration| witness(bundle, depth, &module.path, declaration))
        .collect::<Vec<_>>();
    let mut sections = Vec::new();
    if !witnesses.is_empty() {
        sections.push(Section {
            anchor: WITNESSES,
            title: "Witnesses",
            tally: format!("{}\u{a0}in scope", witnesses.len()),
            rows: witnesses,
        });
    }
    if !declared.is_empty() {
        // A page's only section of witnesses is its witnesses, without more said.
        let (anchor, title) = match sections.is_empty() {
            true => (WITNESSES, "Witnesses"),
            false => (DECLARED, "Witnesses declared here"),
        };
        sections.push(Section {
            anchor,
            title,
            tally: declared.len().to_string(),
            rows: declared,
        });
    }

    Page {
        root,
        path: module.path.join(),
        unit: record.prefix.last().to_string(),
        crumbs,
        subtitle,
        subject: subject.map(|subject| Cluster {
            anchor: None,
            ..whole(bundle, depth, subject, true)
        }),
        counts: bundle.counts(module),
        brief,
        entries,
        rail: rail(bundle, &module.path),
        long: holds.len() + sections.len() > IN_VIEW,
        holds,
        sections,
        version: VERSION,
    }
}

/// The rail of the page at `path`: the root's paths that have a page, and under each one the page is at or beneath, its own.
fn rail(bundle: &Bundle<'_>, path: &Qualifier) -> Vec<RailRow> {
    fn level(
        bundle: &Bundle<'_>,
        path: &Qualifier,
        parent: &Qualifier,
        depth: usize,
        rows: &mut Vec<RailRow>,
    ) {
        let Some(page) = bundle.module(parent) else {
            return;
        };
        for child in bundle.doors(page) {
            let open = path.is_within(&child);
            rows.push(RailRow {
                depth,
                keyword: bundle
                    .subject(&child)
                    .map_or(Keyword::Mod.spelling(), |subject| keyword(subject.kind)),
                name: child.last().to_string(),
                href: bundle.page_path(&child),
                current: child == *path,
                open,
            });
            if open {
                level(bundle, path, &child, depth + 1, rows);
            }
        }
    }

    let mut rows = Vec::new();
    level(bundle, path, &bundle.record.prefix, 0, &mut rows);
    rows
}

/// The paragraph a note opens with.
fn opening(prose: Option<&[String]>) -> Option<Paragraph> {
    paragraphs(prose).into_iter().next()
}

/// The cluster of a declaration `module` exposes: whole where this page is its address, and otherwise its source, the paragraph its note opens with and the way to where it is whole.
fn entry(
    bundle: &Bundle<'_>,
    depth: usize,
    module: &ModuleDocumentation,
    declaration: &Declaration,
) -> Cluster {
    let path = module.path.with(&declaration.name);
    let door = bundle.module(&path).map(|page| Door {
        name: Some(declaration.name.clone()),
        counts: bundle.counts(page),
        href: format!("{}{}", "../".repeat(depth), bundle.page_path(&path)),
    });
    if door.is_none() && declaration.named == path {
        return whole(bundle, depth, declaration, false);
    }

    Cluster {
        members: members(bundle, depth, declaration, None),
        prose: opening(declaration.prose.as_deref()).into_iter().collect(),
        // The page of the module the name is too is where it is whole; the path that declares it is said where it has no such page.
        home: door.is_none().then(|| Origin {
            text: bundle.held(&declaration.named).join(),
            href: bundle.href(depth, &declaration.named),
        }),
        door,
        ..whole(bundle, depth, declaration, false)
    }
}

/// The cluster of a declaration drawn whole: its head, its block with each member's note and anchor, its facts and its note. `subject` says it is what the page opens on, which decides its members' anchors.
fn whole(bundle: &Bundle<'_>, depth: usize, declaration: &Declaration, subject: bool) -> Cluster {
    let block = Block::of(declaration.kind, declaration.opaque);

    Cluster {
        anchor: Some(declaration.name.clone()),
        exposure: false,
        head: lines(
            segments(bundle, depth, &declaration.signature),
            Some(&declaration.name),
            block.map_or("", |block| block.opener),
        ),
        members: members(bundle, depth, declaration, Some(subject)),
        closer: block
            .map(|block| {
                // A closer that is a word of the language is set apart as one, and punctuation stays plain.
                vec![Line {
                    indent: 0,
                    segments: vec![match Keyword::read(block.closer) {
                        Some(keyword) => Segment::Marked {
                            class: keyword.class().name(),
                            text: block.closer.to_string(),
                            href: None,
                        },
                        None => Segment::Text(block.closer.to_string()),
                    }],
                }]
            })
            .unwrap_or_default(),
        facts: facts(declaration),
        prose: paragraphs(declaration.prose.as_deref()),
        door: None,
        home: None,
    }
}

/// The members of `declaration`'s block as the source writes them, each behind what leads it and before what trails it: an inductive's constructors each after a bar, a structure's fields and a concept's methods each with its comma. `noted` is whether the block is where the declaration is whole, and then whether that is as what the page opens on: each member carries its note and its anchor there, and neither anywhere else.
fn members(
    bundle: &Bundle<'_>,
    depth: usize,
    declaration: &Declaration,
    noted: Option<bool>,
) -> Vec<MemberBlock> {
    let Some(block) = Block::of(declaration.kind, declaration.opaque) else {
        return Vec::new();
    };

    declaration
        .members
        .iter()
        .map(|member| {
            let mut lines = lines(
                segments(bundle, depth, &member.signature),
                None,
                block.trail,
            );
            if let Some(first) = lines.first_mut()
                && !block.lead.is_empty()
            {
                first
                    .segments
                    .insert(0, Segment::Text(block.lead.to_string()));
            }

            MemberBlock {
                anchor: noted
                    .filter(|_| !member.name.is_empty())
                    .map(|subject| member_anchor(declaration, member, subject)),
                deep: block.indented,
                lines,
                notes: match noted {
                    Some(_) => paragraphs(member.prose.as_deref()),
                    None => Vec::new(),
                },
            }
        })
        .collect()
}

/// What joins a member of a declaration of `kind` to its owner: a field is written as a projection writes it, and a constructor or a method under its owner's path.
pub fn joint(kind: Kind) -> char {
    match kind {
        Kind::Structure => '.',
        _ => '/',
    }
}

/// The anchor of `member` in `owner`'s block where it is whole: under its owner's name, `Shape/circle` or `Shape.radius`, and under the joint alone, `/circle` or `.radius`, where `subject` says the owner is what the page opens on — which no entry's anchor is, since no name begins with a joint.
pub fn member_anchor(owner: &Declaration, member: &Member, subject: bool) -> String {
    let joint = joint(owner.kind);
    match subject {
        true => format!("{joint}{}", member.name),
        false => format!("{}{joint}{}", owner.name, member.name),
    }
}

/// The cluster of the line by which a module exposes a declaration's members: the line alone.
fn exposure(bundle: &Bundle<'_>, depth: usize, line: &ExposureLine) -> Cluster {
    Cluster {
        anchor: None,
        exposure: true,
        head: lines(segments(bundle, depth, &line.signature), None, ""),
        members: Vec::new(),
        closer: Vec::new(),
        facts: Vec::new(),
        prose: Vec::new(),
        door: None,
        home: None,
    }
}

/// The cluster of `child`, a module that is no declaration, named `name`: the line that declares it, the paragraph its note opens with, and the way to its page with what it exposes.
fn lone(
    bundle: &Bundle<'_>,
    depth: usize,
    child: &Qualifier,
    name: String,
    said: Option<&[String]>,
) -> Cluster {
    let word = |keyword: Keyword| Segment::Marked {
        class: keyword.class().name(),
        text: keyword.spelling().to_string(),
        href: None,
    };

    Cluster {
        anchor: Some(name.clone()),
        exposure: false,
        head: vec![Line {
            indent: 0,
            segments: vec![
                word(Keyword::Pub),
                Segment::Text(" ".to_string()),
                word(Keyword::Mod),
                Segment::Text(" ".to_string()),
                Segment::Declared(name),
            ],
        }],
        members: Vec::new(),
        closer: Vec::new(),
        facts: Vec::new(),
        prose: opening(said).into_iter().collect(),
        door: bundle.module(child).map(|page| Door {
            name: None,
            counts: bundle.counts(page),
            href: format!("{}{}", "../".repeat(depth), bundle.page_path(child)),
        }),
        home: None,
    }
}

/// The row of a witness the module `module` declares.
fn witness(
    bundle: &Bundle<'_>,
    depth: usize,
    module: &Qualifier,
    declaration: &Declaration,
) -> WitnessRow {
    WitnessRow {
        anchor: witness_anchor(module, declaration),
        lines: lines(segments(bundle, depth, &declaration.signature), None, ""),
        derived: declaration.derived,
        prose: paragraphs(declaration.prose.as_deref()),
        home: None,
    }
}

/// The anchor of a witness the module `module` declares: its head as a page prints it, with its spaces dropped, then the module, `satisfyShow(Error)@/std/Io`. A head is written in its module's names, so two modules may write one text of two types, and a page lists the witnesses of many; within a module coherence admits one witness of a concept at a head, so the two together name it on every page, whatever is declared above it.
pub fn witness_anchor(module: &Qualifier, declaration: &Declaration) -> String {
    let head = declaration
        .signature
        .text
        .split_whitespace()
        .collect::<String>();

    format!("{head}@{}", module.join())
}

/// The facts of a declaration its source does not show, in one order: how its representation stands, said both ways since the source says either by one `pub` or its absence; what the compiler says the thing is; and whether it is proven total, said both ways too.
fn facts(declaration: &Declaration) -> Vec<Fact> {
    let fact = |word: &str, drawing, meaning| Fact {
        word: word.to_string(),
        drawing,
        meaning,
    };
    let representation = match (declaration.kind, declaration.opaque) {
        (Kind::Inductive | Kind::Structure, false) => Some(fact(
            "transparent",
            "plain",
            "its representation is public: a consumer can write its constructors or fields",
        )),
        (Kind::Inductive | Kind::Structure, true) => Some(fact(
            "opaque",
            "",
            "its representation is kept: only its declaring subtree can write its constructors or fields",
        )),
        (Kind::Concept, false) => Some(fact(
            "unsealed",
            "plain",
            "its representation is public: a consumer can declare a witness of it",
        )),
        (Kind::Concept, true) => Some(fact(
            "sealed",
            "",
            "its representation is kept: only its declaring subtree can declare a witness of it",
        )),
        (Kind::Definition | Kind::Witness | Kind::Foreign, _) => None,
    };
    // What kind of thing a declaration adopted out of a root no consumer names is: the word that root's owner chose.
    let adopted = declaration.chip.as_deref().map(|word| {
        fact(
            word,
            "",
            "the compiler provides it: no source a consumer can read declares it",
        )
    });
    let proof = declaration.proof.then(|| {
        fact(
            "proof",
            "proof",
            "its type is a proposition: it states a fact, and its body proves it",
        )
    });
    let total = declaration.totality.map(|totality| match totality {
        Totality::Total => fact(
            "total",
            "plain",
            "proven total: it terminates on every input",
        ),
        Totality::Partial => fact("partial", "", "not proven total"),
        Totality::Unfounded => fact(
            "unfounded",
            "",
            "its own recursion is not shown to descend, so no type computes through it",
        ),
    });

    [representation, adopted, proof, total]
        .into_iter()
        .flatten()
        .collect()
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

/// `segments` as the lines the printer laid them out in, each with the indent it was given, its text beginning past it ([`Line::lead`]), and `trail` after the last. Where `declared` names what the signature declares, the name is set apart: it is the first text of the signature after its keywords. A text ends at each space ([`folding`]), and `trail` joins what it follows, so a line never folds before what closes it.
fn lines(segments: Vec<Segment>, declared: Option<&str>, trail: &str) -> Vec<Line> {
    let mut lines = vec![Line {
        indent: 0,
        segments: Vec::new(),
    }];
    let mut declared = declared.filter(|name| !name.is_empty());
    for segment in segments {
        let Segment::Text(text) = segment else {
            lines
                .last_mut()
                .expect("a signature has a line")
                .segments
                .push(segment);
            continue;
        };
        for (index, mut part) in text.split('\n').enumerate() {
            if index > 0 {
                let indent = part.len() - part.trim_start_matches(' ').len();
                part = &part[indent..];
                lines.push(Line {
                    indent,
                    segments: Vec::new(),
                });
            }
            let line = &mut lines.last_mut().expect("a signature has a line").segments;
            let words = part.trim_start_matches(' ');
            // The first text that is more than spaces is where the name stands, or the signature declares none by it.
            if let Some(name) = declared.filter(|_| !words.is_empty()) {
                declared = None;
                if let Some(after) = words.strip_prefix(name)
                    && !after.starts_with(|next: char| next.is_alphanumeric() || next == '_')
                {
                    folding(&part[..part.len() - words.len()], line);
                    line.push(Segment::Declared(name.to_string()));
                    folding(after, line);
                    continue;
                }
            }
            folding(part, line);
        }
    }
    if !trail.is_empty() {
        plain(
            trail,
            &mut lines.last_mut().expect("a signature has a line").segments,
        );
    }
    lines
}

/// `text` appended to `out` as the text of a line, one segment to each space: a page folds a line too wide for it after a space and nowhere else, so that an arrow, a path and a call with its arguments' opening each stay whole.
fn folding(text: &str, out: &mut Vec<Segment>) {
    for piece in text.split_inclusive(' ') {
        match out.last_mut() {
            // What follows a text that ends at no space is the rest of its word.
            Some(Segment::Text(before)) if !before.ends_with(' ') => before.push_str(piece),
            _ => out.push(Segment::Text(piece.to_string())),
        }
    }
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
