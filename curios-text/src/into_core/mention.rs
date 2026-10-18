//! What a unit's text names, place by place: the read an answer about a position in a file is made from, and what a documentation page links a written name to.
//!
//! **Nothing is recorded for it.** A written name lowers to a variable holding the declaration it resolved to and the extent it was written at (`Lowerer::term`), and the unit's anchors hold where each definition's own name was written (`curios_core::Anchors::labels`). So what a text names is read off the lowered items of the declaration written in it, that declaration's alone, where it is asked.
//!
//! **A text is one source declaration's, or the term a program closes with** ([`Text`]): what an extent is measured in, so a mention is found by the declaration it stands in and an edit elsewhere in its file moves none.
//!
//! **A path is one name with one place.** `Io/Reader/line` is a mention of `line` over the whole path, and no segment of it is a mention of the module it names.
//!
//! **What no term holds at its own place has no mention.** A bare literal's head and a struct pattern's are resolved into the node they open; a pattern's constructor, a projection's label, a field written in a literal and a method reached through a witness are the elaborator's to resolve; a `use` line's names are held by the import table under no place; and a binder is no declaration.

#[cfg(test)]
mod tests;

use {
    super::{Head, Headed},
    crate::PreparedText,
    curios_utilities::{Qualifier, Source, Span},
    std::collections::{BTreeSet, HashSet},
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
                    names.extend(items.names(at).into_iter().copied());
                    place = items.place_after(Some(at));
                }
                keys.push((key, names));
            }
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
            let path = |text: &Text| text.span.source.path.clone();

            path(one)
                .cmp(&path(other))
                .then(one.span.start.cmp(&other.span.start))
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

    /// Every name this text writes, in the order they are written: the definitions it declares, and each use of a declaration the lowering resolved, read off the lowered items of the declaration written here. A use may stand inside a definition's name, as the concept a `satisfy` names stands in the head that declares its witness.
    pub fn mentions(&self) -> Vec<Mention> {
        let mut found = (HashSet::new(), BTreeSet::new());
        let mut read = |term: &curios_core::Term| self.named(term, &mut found);
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
        }

        // A definition's own name may stand in a term the lowering built around it, which is no use of it.
        let declared = self
            .labels
            .iter()
            .map(|(name, label)| (label.start, label.end, *name))
            .collect::<BTreeSet<_>>();
        let uses = found
            .1
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

    /// Record in `found` every variable of `term` that names a declaration and was written in this text, by its extent there. A node is walked once however many terms share it, and recorded at each occurrence: an occurrence holds the place, and the node what stands under it.
    fn named(
        &self,
        term: &curios_core::Term,
        found: &mut (HashSet<usize>, BTreeSet<(u32, u32, curios_core::Global)>),
    ) {
        term.walk(
            found,
            |(walked, names), node| {
                if let Some(extent) = node.extent()
                    && (extent.anchor, extent.stamp) == self.key
                    && let curios_core::Subterm::Var(var) = &**node
                    && let Some(name) = var.as_free().and_then(|free| free.as_global())
                {
                    names.insert((extent.start, extent.end, *name));
                }

                match walked.insert(node.identity()) {
                    true => curios_core::Enter::Descend,
                    false => curios_core::Enter::Skip(()),
                }
            },
            |_, _, _| (),
        );
    }
}
