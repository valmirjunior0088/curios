//! What a unit's text names, place by place: the read an answer about a position in a file is made from, and what a documentation page links a written name to.
//!
//! **Nothing is recorded for it.** A written name lowers to a variable holding the declaration it resolved to and the extent it was written at (`Lowerer::term`), and the unit's anchors hold where each definition's own name was written (`curios_core::Anchors::labels`). So what a text names is read off the lowered items of the declaration written in it, that declaration's alone, where it is asked.
//!
//! **A text is one source declaration's, or the term a program closes with** ([`Text`]): what an extent is measured in, so a mention is found by the declaration it stands in and an edit elsewhere in its file moves none.
//!
//! **A path is one name with one place.** `Io/Reader/line` is a mention of `line` over the whole path, and no segment of it is a mention of the module it names.
//!
//! **A place no name stands at is a term's.** Every term written with a span lowers to a node placed by its extent, whatever it lowers to, so the terms a text writes are read off the walk its names are ([`Text::terms`]).
//!
//! **What no term holds at its own place has no mention.** A bare literal's head and a struct pattern's are resolved into the node they open; a pattern's constructor, a projection's label, a field written in a literal and a method reached through a witness are the elaborator's to resolve; a `use` line's names are held by the import table under no place; and a binder is no declaration.

#[cfg(test)]
mod tests;

use {
    super::{Head, Headed},
    crate::PreparedText,
    curios_common::{Qualifier, Source, Span},
    std::{
        cmp::Reverse,
        collections::{BTreeSet, HashSet},
        ptr,
    },
};

/// One name a text writes: where, and the declaration it names.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Mention {
    pub span: Span,
    pub name: curios_core::Global,
    /// What the declaration is declared as, where this is where it is named, and `None` at a use of it.
    pub declared: Option<curios_core::Declared>,
}

/// One text a unit was lowered from: a source declaration's, or the term its program closes with.
pub struct Text<'a> {
    unit: &'a PreparedText,
    /// The term the program closes with, where this is its text.
    entry: Option<&'a curios_core::Entrypoint>,
    key: (curios_core::Anchor, u32),
    span: Span,
    /// The definitions whose names are written in it, each with where, in the order they are written.
    labels: Vec<(curios_core::Global, curios_core::Extent)>,
}

impl PreparedText {
    /// The texts this unit was lowered from that lie in a source `lies` holds of, in the order they are written. `entry` is the term the unit's program closes with, where it is one: no unit holds it, and its text names nothing without it.
    ///
    /// **Read off the declarations written in those sources**, and no other's: each file `lies` holds of gives its module's body and the ones its text writes inline, by its heads, and a module no file backs — a program's own, or one supplied as a tree — gives its body wherever it is asked, its texts then held to `lies` one by one. Each body's declarations are found by the body (`Bodies`), each one's labels by the names its items declare.
    pub fn texts<'a>(
        &'a self,
        entry: Option<&'a curios_core::Entrypoint>,
        lies: impl Fn(&Source) -> bool,
    ) -> Vec<Text<'a>> {
        let anchors = self.anchors();
        let items = &self.core().items;
        let scans = self.scans();
        let bodies = self.bodies();

        // The module bodies written in the sources asked about: each file's own with the ones its text writes inline, and each no file backs.
        let mut written = Vec::new();
        for (module, scan) in scans.iter().filter(|(_, scan)| lies(&scan.source)) {
            inline_bodies(*module, &scan.heads, &mut written);
        }
        written.extend(bodies.unfiled(scans.keys()));

        // Each declaration written in them, with the names its items declare, and the program's closing term.
        let mut keys = Vec::new();
        for module in &written {
            let Some(body) = bodies.get(module) else {
                continue;
            };
            for &first in &body.firsts {
                let Some(record) = self.lowerings().get(first) else {
                    continue;
                };
                let Some(key) = record.text() else {
                    continue;
                };
                let mut names = Vec::new();
                let mut place = usize::try_from(first).ok();
                for _ in 0..record.facts().len() {
                    let Some(at) = place else {
                        break;
                    };
                    names.extend(items.names(at));
                    place = items.place_after(Some(at));
                }
                // A concept's methods are names its declaration writes, which no item declares; a family's constructors, the other members a former writes, its item declares already.
                let methods = record
                    .facts()
                    .iter()
                    .flat_map(|fact| fact.members.iter().map(|(name, _)| *name))
                    .filter(|name| !names.contains(name))
                    .collect::<Vec<_>>();
                names.extend(methods);
                keys.push((key, names));
            }
        }
        // Each `use` and `mod` line of those bodies, a text of its body's namespace.
        for module in &written {
            let namespace = curios_core::Anchor::of(curios_core::Anchored::Namespace(*module));
            keys.extend(
                anchors
                    .texts_of(namespace)
                    .into_iter()
                    .map(|(stamp, _)| ((namespace, stamp), Vec::new())),
            );
        }
        let closing = curios_core::Anchor::of(curios_core::Anchored::Entry);
        keys.extend(
            anchors
                .texts_of(closing)
                .into_iter()
                .map(|(stamp, _)| ((closing, stamp), Vec::new())),
        );

        let mut texts = keys
            .into_iter()
            .filter_map(|(key, names)| {
                let span = anchors.text_of(key)?;
                if !lies(&span.source) {
                    return None;
                }
                let mut labels = names
                    .into_iter()
                    .filter_map(|name| {
                        let label = anchors.label_of(&name)?;
                        ((label.anchor, label.stamp) == key).then_some((name, *label))
                    })
                    .collect::<Vec<_>>();
                labels.sort_by_key(|(_, label)| (label.start, label.end));

                Some(Text {
                    unit: self,
                    entry: entry
                        .filter(|_| matches!(key.0.anchored(), curios_core::Anchored::Entry)),
                    key,
                    span,
                    labels,
                })
            })
            .collect::<Vec<_>>();
        texts.sort_by(|one, other| {
            let (one, other) = (&one.span, &other.span);

            (one.source.name(), one.source.file(), one.start).cmp(&(
                other.source.name(),
                other.source.file(),
                other.start,
            ))
        });

        texts
    }
}

/// Push `module` and each module written inline in its text, `heads` being its items' heads.
fn inline_bodies(module: Qualifier, heads: &[Headed], written: &mut Vec<Qualifier>) {
    written.push(module);
    for headed in heads {
        if let Head::Mod {
            label,
            inline: Some(heads),
            ..
        } = &headed.head
        {
            inline_bodies(module.with(label), heads, written);
        }
    }
}

impl Text<'_> {
    /// Where the text lies in its source.
    pub fn span(&self) -> &Span {
        &self.span
    }

    /// The definitions this text declares, each where its name is written, in the order they are written. Nothing is read to say them: a source declaration's first is the one it is known by, and the rest are what it lowers to beside it, a constructor or a method.
    pub fn declares(&self) -> Vec<Mention> {
        self.labels
            .iter()
            .filter_map(|(name, label)| {
                Some(Mention {
                    span: self.placed(label.start, label.end)?,
                    name: *name,
                    declared: Some(self.unit.declared_as(name)?),
                })
            })
            .collect()
    }

    /// The definition a reader of this text stands in: the declaration written here, a witness by the one its head declares, and none for the term a program closes with, which stands in no definition.
    pub fn owner(&self) -> Option<curios_core::Global> {
        match self.key.0.anchored() {
            curios_core::Anchored::Declaration(path) => Some(curios_core::Global::Authored(*path)),
            curios_core::Anchored::Unnamed(_) => self.labels.first().map(|(name, _)| *name),
            curios_core::Anchored::Namespace(_) | curios_core::Anchored::Entry => None,
        }
    }

    /// Every name this text writes, in the order they are written: the definitions it declares, and each use of a declaration the lowering resolved, read off the lowered items of the declaration written here and the names it resolved into a node no term places ([`curios_core::Mints::named`]). A use may stand inside a definition's name, as the concept a `satisfy` names stands in the head that declares its witness.
    pub fn mentions(&self) -> Vec<Mention> {
        let mut written = BTreeSet::new();
        self.each_placed(|node, extent| {
            if let Some(name) = written_name(node) {
                written.insert((extent.start, extent.end, name));
            }
        });
        for mints in self.mints() {
            for (extent, name) in &mints.named {
                if (extent.anchor, extent.stamp) == self.key {
                    written.insert((extent.start, extent.end, *name));
                }
            }
        }
        for line in self.lines() {
            if let curios_core::LineNamed::Declaration(name) = line.names {
                written.insert((line.at.start, line.at.end, name));
            }
        }

        // A definition's own name may stand in a term the lowering built around it, which is no use of it.
        let declared = self
            .labels
            .iter()
            .map(|(name, label)| (label.start, label.end, *name))
            .collect::<BTreeSet<_>>();
        let uses = written
            .difference(&declared)
            .filter_map(|&(start, end, name)| {
                Some(Mention {
                    span: self.placed(start, end)?,
                    name,
                    declared: None,
                })
            });

        let mut mentions = self.declares().into_iter().chain(uses).collect::<Vec<_>>();
        mentions.sort_by_key(|mention| (mention.span.start, mention.span.end));

        mentions
    }

    /// Where every contextual word that opened a member this text writes stands, in the order they are written ([`curios_core::Mints::words`]).
    pub fn words(&self) -> Vec<Span> {
        let mut words = self
            .mints()
            .into_iter()
            .flat_map(|mints| mints.words.iter().copied())
            .filter(|extent| (extent.anchor, extent.stamp) == self.key)
            .filter_map(|extent| self.placed(extent.start, extent.end))
            .collect::<Vec<_>>();
        words.sort_by_key(|span| (span.start, span.end));
        words.dedup_by(|this, that| (this.start, this.end) == (that.start, that.end));

        words
    }

    /// Every segment of a path this text writes that reaches a module, where it is written and the module, in the order they are written ([`curios_core::Mints::modules`]).
    pub fn modules(&self) -> Vec<(Span, Qualifier)> {
        let lines = self
            .lines()
            .into_iter()
            .filter_map(|line| match line.names {
                curios_core::LineNamed::Module(module) => Some((line.at, module)),
                curios_core::LineNamed::Declaration(_) => None,
            });
        let mut modules = self
            .mints()
            .into_iter()
            .flat_map(|mints| mints.modules.iter().copied())
            .chain(lines)
            .filter(|(extent, _)| (extent.anchor, extent.stamp) == self.key)
            .filter_map(|(extent, module)| Some((self.placed(extent.start, extent.end)?, module)))
            .collect::<Vec<_>>();
        modules.sort_by_key(|(span, _)| (span.start, span.end));
        modules.dedup_by(|(this, _), (that, _)| (this.start, this.end) == (that.start, that.end));

        modules
    }

    /// Where each term this text writes stands, once a place and in the order they are written, a term holding another before it: every lowered node placed in this text, whatever it lowered to — what a place no name stands at answers.
    pub fn terms(&self) -> Vec<Span> {
        let mut placed = BTreeSet::new();
        self.each_placed(|_, extent| {
            placed.insert((extent.start, Reverse(extent.end)));
        });

        placed
            .into_iter()
            .filter_map(|(start, Reverse(end))| self.placed(start, end))
            .collect()
    }

    /// Hand `met` every lowered node written in this text with the extent it was written at, read off the lowered items of the declaration written here or the term a program closes with. A node is walked once however many terms share it, and met at each occurrence: an occurrence holds the place, and the node what stands under it.
    fn each_placed(&self, mut met: impl FnMut(&curios_core::Term, curios_core::Extent)) {
        let mut walked = HashSet::new();
        let mut read = |term: &curios_core::Term| {
            term.walk(
                &mut walked,
                |walked, node| {
                    if let Some(extent) = node.extent()
                        && (extent.anchor, extent.stamp) == self.key
                    {
                        met(node, extent);
                    }

                    match walked.insert(node.identity()) {
                        true => curios_core::Enter::Descend,
                        false => curios_core::Enter::Skip(()),
                    }
                },
                |_, _, _| (),
            );
        };
        match self.key.0.anchored() {
            curios_core::Anchored::Entry => self.entry.iter().for_each(|entry| read(&entry.body)),
            curios_core::Anchored::Declaration(_) | curios_core::Anchored::Unnamed(_) => {
                let items = self
                    .labels
                    .iter()
                    .filter_map(|(name, _)| Some(self.unit.places().place(name)?.item()))
                    .collect::<BTreeSet<_>>();
                for item in items {
                    self.unit.lowered_item(item).each_term(&mut read);
                }
            }
            // A `use` or `mod` line lowers to no item, so no lowered term holds what it writes.
            curios_core::Anchored::Namespace(_) => {}
        }
    }

    /// What the lowering minted for the declarations written here, or for the term a program closes with: once each, though a declaration's names share it.
    fn mints(&self) -> Vec<&curios_core::Mints> {
        let minted = self.unit.minted();
        match self.key.0.anchored() {
            curios_core::Anchored::Entry => vec![&minted.entry],
            curios_core::Anchored::Declaration(_) | curios_core::Anchored::Unnamed(_) => {
                let mut mints = Vec::<&curios_core::Mints>::new();
                for (name, _) in &self.labels {
                    let of = minted.of(name);
                    if !mints.iter().any(|seen| ptr::eq(*seen, of)) {
                        mints.push(of);
                    }
                }
                mints
            }
            curios_core::Anchored::Namespace(_) => Vec::new(),
        }
    }

    /// Where each written binder of the declarations written here, or of the term a program closes with, stands ([`curios_core::Mints::written`]): none on a `use` or a `mod` line, which binds nothing.
    pub fn binders(
        &self,
    ) -> Vec<(
        curios_core::WrittenBinder,
        curios_core::Extent,
        curios_core::Binds,
    )> {
        self.mints()
            .into_iter()
            .flat_map(|mints| mints.written.iter().copied())
            .collect()
    }

    /// What this text names, where it is a `use` or a `mod` line: of what its body's lines name, the names placed in this one.
    fn lines(&self) -> Vec<&curios_core::LineName> {
        let lines = match self.key.0.anchored() {
            curios_core::Anchored::Namespace(module) => self.unit.imports().lines(module),
            curios_core::Anchored::Declaration(_)
            | curios_core::Anchored::Unnamed(_)
            | curios_core::Anchored::Entry => &[],
        };

        lines
            .iter()
            .filter(|line| (line.at.anchor, line.at.stamp) == self.key)
            .collect()
    }

    /// The span of the text's bytes from `start` to `end`. `None` for a range that does not lie in it.
    fn placed(&self, start: u32, end: u32) -> Option<Span> {
        let (start, end) = (start as usize, end as usize);
        let within = start <= end && self.span.start + end <= self.span.end;

        within.then(|| {
            Span::new(
                self.span.source.clone(),
                self.span.start + start,
                self.span.start + end,
            )
        })
    }
}

/// The name `node` writes: a declaration's, or a method's path, its concept's extended by the field's label.
fn written_name(node: &curios_core::Term) -> Option<curios_core::Global> {
    match &**node {
        curios_core::Subterm::Transient(curios_core::Transient::Name(name))
        | curios_core::Subterm::Instance(curios_core::Instance { name, .. }) => Some(*name),
        curios_core::Subterm::Transient(curios_core::Transient::Method(curios_core::Method {
            concept,
            field,
        })) => Some(curios_core::Global::Authored(
            concept.qualifier()?.with(field),
        )),
        _ => None,
    }
}
