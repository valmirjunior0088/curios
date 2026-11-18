//! A nominal declaration in its form: a family's, a record's and a concept's.
//!
//! **A family is `induct /F(params): (indices) -> pub Sort`, a case to a line, `| tag(payload): (targets)`, and `end`**; a record is `struct /R(params): pub Sort { fields }` and a concept `concept /C(params): pub Sort { fields }`, a superclass edge among a concept's fields written `use T`. A form builds its declaration's entry, which its former and a family's constructors are made of ([`curios_core::Made`]), so nothing the module holds is written elsewhere. The inner `pub` is the entry's `rep_public`.
//!
//! **Each binder is written by its word, under its mark**, `@n: Nat`, or as its type alone where nothing reads it, and a later type reads it by that word: a family's cases read its parameters by the head's words, a case holding its payload under its family's parameters and none of its own.

use {
    super::{
        Fields, LabelRead, OpenedEntry, ReadError, Reader, Refusal, TelescopeEntry, core_read,
        malformed, parameter_entry, proving_telescope, telescope_read, value_list, value_module,
        value_word,
    },
    crate::{
        CasePayloadParam, CoreHead, CoreText, DecoratorField, Frame, FuncTypeParam, Rendered,
        Shown, TopCase, TopConcept, TopInduct, TopStruct, sub,
    },
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Bound, Constructors, Cursor, Free, Global, InductDecl, InductParam, Label, Polarity,
        StructDecl, Telescope, Term, UniverseContext, Variance,
    },
};

/// One binder of a declaration's telescope: its mark, its type, the variable a later type reads it by, whether the rest of its own telescope reads it, and whether it has a hint or is so read.
pub struct DeclaredBinder {
    pub mark: Plicity,
    pub type_: Term,
    pub label: Free,
    /// The label Core keeps for the binder, which a whole write states beside its word.
    pub kept: Option<Label>,
    pub variable: Term,
    pub read: bool,
    pub named: bool,
}

impl DeclaredBinder {
    /// Whether this binder is read as a value: in place of a superclass edge's word, which resolution reads it by, its word.
    fn variable_is(&self, term: &Term) -> bool {
        term.mentions_free(&self.label)
    }

    /// What a whole write states of this binder's label beside its word.
    fn core<D: Rendered>(&self, frame: Frame<'_, D>) -> Vec<DecoratorField<D>> {
        frame.label_fields(self.kept.as_ref())
    }

    /// The word this binder is written by where it has a hint or `read` says something reads it, `_` where nothing does and its decorator states something, which stands before a word and reads back as no hint, and none where it is written as its type alone.
    fn word<D: Rendered>(&self, read: bool, frame: Frame<'_, D>) -> Option<String> {
        match self.named || read {
            true => Some(frame.spelling.label(&self.label)),
            false => (!self.core(frame).is_empty()).then(|| "_".to_string()),
        }
    }

    /// This binder under its mark: by its word where it has a hint or `read` says something reads it, and as its type alone otherwise.
    pub fn written<D: Rendered>(&self, read: bool, frame: Frame<'_, D>) -> FuncTypeParam<D> {
        FuncTypeParam {
            plicity: self.mark,
            core: self.core(frame),
            label: self.word(read, frame),
            type_: sub(self.type_.clone(), frame),
        }
    }
}

/// The entries of a telescope from `cursor` to its end, each opened at the variable of a word of its own.
pub fn binders<D: Rendered, B: Bound>(
    cursor: &mut Cursor<'_, B>,
    frame: Frame<'_, D>,
) -> Vec<DeclaredBinder> {
    let mut binders = Vec::new();
    while let (Some((hint, type_)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let kept = cursor.label().copied();
        let label = frame.label(cursor.label());
        let read = cursor.binder_used();
        let named = hint.is_some() || read;
        let variable = Term::free_var(&label);
        cursor.advance(variable.clone());
        binders.push(DeclaredBinder {
            mark,
            type_,
            label,
            kept,
            variable,
            read,
            named,
        });
    }

    binders
}

/// The head a declaration of `name` is written under, its decorator empty: the caller states what it sets.
fn head_of<D: Rendered>(name: &Global) -> CoreHead<D> {
    CoreHead {
        fields: Vec::new(),
        name: *name,
    }
}

/// The label a declaration of `name` declares: its path's last segment, or a witness's identity.
fn label_of(name: &Global) -> crate::Label {
    crate::Label::from(match name {
        Global::Authored(path) => path.last().to_string(),
        Global::Witness(id) => id.to_string(),
    })
}

/// A family's form: its head, then each case; `end` is the printer's.
pub fn family_declared<D: Rendered>(
    name: &Global,
    entry: &InductDecl,
    frame: Frame<'_, D>,
) -> TopInduct<D> {
    let mut cursor = entry.arity().cursor();
    let params = binders(&mut cursor, frame);
    let indices = cursor
        .body()
        .expect("a cursor past every entry holds its payload");
    let indices = binders(&mut indices.cursor(), frame);
    // A parameter a case reads is written by its word, which the case reads it by.
    let variables = params
        .iter()
        .map(|param| param.variable.clone())
        .collect::<Vec<_>>();
    let payloads = entry
        .constructors()
        .iter()
        .map(|(_, case)| case.open(&variables))
        .collect::<Vec<_>>();
    let read = params
        .iter()
        .map(|param| payloads.iter().any(|payload| reads(payload, param)))
        .collect::<Vec<_>>();
    let written = params
        .iter()
        .zip(read)
        .map(|(param, read)| param.written(read, frame))
        .collect();
    let indexed = !indices.is_empty();
    // Each part written in the order the text holds it, the head before the cases, which is the order a reader meets a node a part shares with a later one.
    let index_types = indices
        .iter()
        .map(|index| {
            (
                index.core(frame),
                index.word(false, frame),
                sub(index.type_.clone(), frame),
            )
        })
        .collect();
    let result_sort = sub(entry.result_sort().clone(), frame);
    let cases = entry
        .constructors()
        .iter()
        .zip(payloads)
        .map(|((tag, _), payload)| case_written(tag, &payload, indexed, frame))
        .collect::<Vec<_>>();

    TopInduct {
        doc: None,
        core: head_of(name),
        vis_pub: false,
        rep_pub: entry.rep_public(),
        label: label_of(name),
        params: written,
        indices: index_types,
        result_sort,
        cases,
    }
}

/// Whether a case whose payload, at the family's parameters, is `payload` reads the family's parameter `param`.
fn reads(payload: &Telescope<Vec<Term>>, param: &DeclaredBinder) -> bool {
    let mut cursor = payload.cursor();
    while let Some((_, type_)) = cursor.entry() {
        if param.variable_is(&type_) {
            return true;
        }
        cursor.advance(Term::free_var(&Free::local(u64::MAX, None)));
    }

    cursor
        .body()
        .is_some_and(|targets| targets.iter().any(|target| param.variable_is(target)))
}

/// A case: its tag over its payload, read at its family's parameters, then its targets where its family has indices.
fn case_written<D: Rendered>(
    tag: &Atom,
    payload: &Telescope<Vec<Term>>,
    indexed: bool,
    frame: Frame<'_, D>,
) -> TopCase<D> {
    let mut cursor = payload.cursor();
    let payload = binders(&mut cursor, frame)
        .iter()
        .map(|binder| CasePayloadParam {
            plicity: binder.mark,
            core: binder.core(frame),
            label: binder.word(false, frame),
            type_: sub(binder.type_.clone(), frame),
        })
        .collect();
    let targets = cursor.body().unwrap_or_default();
    let target = (indexed || !targets.is_empty()).then(|| {
        targets
            .into_iter()
            .map(|target| sub(target, frame))
            .collect()
    });
    TopCase {
        doc: None,
        label: crate::Label::from(tag.as_string()),
        payload,
        target,
    }
}

/// A record's form: its parameters, its sort and its fields, each field under its mark and by the word it is written by where it has one, with the word each is written by in order — empty for a superclass edge and for a field nothing names.
pub struct RecordWritten<D: Rendered = Shown> {
    pub params: Vec<FuncTypeParam<D>>,
    pub rep_pub: bool,
    pub result_sort: crate::Term<D>,
    pub fields: Vec<crate::StructField<D>>,
    pub words: Vec<String>,
}

impl<D: Rendered> RecordWritten<D> {
    /// The record as a `struct` declaration of `name`.
    pub fn structure(self, name: &Global) -> TopStruct<D> {
        TopStruct {
            doc: None,
            core: head_of(name),
            vis_pub: false,
            rep_pub: self.rep_pub,
            label: label_of(name),
            params: self.params,
            result_sort: self.result_sort,
            fields: self.fields,
        }
    }

    /// The record as a `concept` declaration of `name`.
    pub fn concept(self, name: &Global) -> TopConcept<D> {
        TopConcept {
            doc: None,
            core: head_of(name),
            vis_pub: false,
            rep_pub: self.rep_pub,
            label: label_of(name),
            params: self.params,
            result_sort: self.result_sort,
            fields: self
                .fields
                .into_iter()
                .map(|field| crate::ConceptField {
                    doc: None,
                    plicity: field.plicity,
                    core: field.param.core,
                    label: field.param.label,
                    func_params: None,
                    type_: field.param.type_,
                })
                .collect(),
        }
    }
}

/// A record's form.
pub fn record_declared<D: Rendered>(record: &StructDecl, frame: Frame<'_, D>) -> RecordWritten<D> {
    let mut cursor = record.arity.cursor();
    let params = binders(&mut cursor, frame);
    let fields = cursor
        .body()
        .expect("a cursor past every entry holds its payload");
    let fields = binders(&mut fields.cursor(), frame);
    // A parameter a field reads is written by its word, which the field reads it by.
    let params = params
        .iter()
        .map(|param| {
            let read = fields.iter().any(|field| param.variable_is(&field.type_));
            param.written(read, frame)
        })
        .collect::<Vec<_>>();
    let words = fields
        .iter()
        .map(|field| match (field.mark, field.named) {
            (Plicity::Witness, _) | (_, false) => String::new(),
            (_, true) => frame.spelling.label(&field.label),
        })
        .collect();
    // A superclass edge is a field a program never names, which resolution reaches: its word is written where a later field reads it, as a witness binder's is.
    let written = fields
        .iter()
        .map(|field| {
            let label = match field.mark {
                Plicity::Witness => field.word(field.read, frame),
                _ => field.word(false, frame),
            };
            crate::StructField {
                doc: None,
                plicity: field.mark,
                param: crate::TupleTypeParam {
                    core: field.core(frame),
                    label: label.map(crate::Label::from),
                    func_params: None,
                    type_: sub(field.type_.clone(), frame),
                },
            }
        })
        .collect();

    RecordWritten {
        params,
        rep_pub: record.rep_public,
        result_sort: sub(record.result_sort.clone(), frame),
        fields: written,
        words,
    }
}

/// The parts of an entry its decorator sets, each in place over its template: the module its representation belongs to, and the polarities and the variances its elaboration filed — what the writer's `entry_fields` writes of an entry beside its form.
pub fn entry_fields_read(
    fields: &mut Fields<'_>,
    module: &mut Qualifier,
    polarities: &mut Vec<Polarity>,
    variances: &mut Vec<Variance>,
) -> Result<(), ReadError> {
    if let Some(value) = fields.take("module") {
        *module = value_module(value)?;
    }
    if let Some(value) = fields.take("polarities") {
        *polarities = value_list(value)?
            .iter()
            .map(|polarity| {
                let word = value_word(polarity)?;
                [
                    Polarity::Unused,
                    Polarity::Strict,
                    Polarity::Pos,
                    Polarity::Neg,
                    Polarity::Mixed,
                ]
                .into_iter()
                .find(|known| format!("{known:?}") == word)
                .ok_or_else(|| malformed("a polarity, as Core names it", polarity))
            })
            .collect::<Result<_, _>>()?;
    }
    if let Some(value) = fields.take("variances") {
        *variances = value_list(value)?
            .iter()
            .map(|variance| match value_word(variance)? {
                "Irrelevant" => Ok(Variance::Irrelevant),
                "Invariant" => Ok(Variance::Invariant),
                _ => Err(malformed("a variance, as Core names it", variance)),
            })
            .collect::<Result<_, _>>()?;
    }

    Ok(())
}

/// The module a declaration's representation belongs to where its decorator states none: its name's parent.
fn parent(name: &Global) -> Qualifier {
    name.qualifier()
        .map(Qualifier::without_last)
        .unwrap_or_default()
}

/// A family's entry from its form: its parameters by their words, its indices and the sort it lands in, then each case, which reads the parameters by the head's words and not the indices.
pub fn family_entry_read(
    family: &TopInduct<CoreText>,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<InductDecl, ReadError> {
    let params = family
        .params
        .iter()
        .map(parameter_entry)
        .collect::<Result<Vec<_>, _>>()?;
    let indices = family
        .indices
        .iter()
        .map(|(core, label, type_)| {
            let label = LabelRead::of(core, label.as_deref().unwrap_or("_"), type_.span())?;
            Ok((Plicity::Explicit, label, type_))
        })
        .collect::<Result<Vec<TelescopeEntry<'_>>, ReadError>>()?;
    let (read, (indices_read, cases)) = telescope_read(&params, reader, &mut |reader, params| {
        let (indices, ()) = telescope_read(&indices, reader, &mut |_, _| Ok(()))?;
        let cases = family
            .cases
            .iter()
            .map(|case| case_read(case, params, reader))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((indices, cases))
    })?;
    let indices_built = Telescope::build(
        indices_read
            .into_iter()
            .map(|(_, binder, type_)| (binder, type_)),
        (),
    );

    let arity = proving_telescope(
        Telescope::build_marked(read, proving_telescope(indices_built, &indices)),
        &params,
    );
    let cases = Constructors::try_from(cases).map_err(|repeated| {
        ReadError::at(
            Refusal::RepeatedCase(repeated.tag.to_string()),
            family.cases[repeated.position].label.span(),
        )
    })?;
    let result_sort = core_read(&family.result_sort, reader)?;
    let mut module = parent(name);
    let (mut polarities, mut variances) = (Vec::new(), Vec::new());
    entry_fields_read(fields, &mut module, &mut polarities, &mut variances)?;

    Ok(InductDecl::new(
        scheme.clone(),
        arity,
        cases,
        result_sort,
        module,
        family.rep_pub,
    )
    .with_polarities(polarities)
    .with_variances(variances))
}

/// A case: its payload by its own words, over its family's parameters, which it reads by the head's words, then its targets.
fn case_read(
    case: &TopCase<CoreText>,
    params: &[OpenedEntry],
    reader: &mut Reader<'_>,
) -> Result<(Atom, InductParam), ReadError> {
    let payload = case
        .payload
        .iter()
        .map(|param| {
            let label = LabelRead::of(
                &param.core,
                param.label.as_deref().unwrap_or("_"),
                param.type_.span(),
            )?;
            Ok((param.plicity, label, &param.type_))
        })
        .collect::<Result<Vec<TelescopeEntry<'_>>, ReadError>>()?;
    let (payload_read, targets) = telescope_read(&payload, reader, &mut |reader, _| {
        case.target
            .iter()
            .flatten()
            .map(|target| core_read(target, reader))
            .collect::<Result<Vec<_>, _>>()
    })?;

    let telescope = Telescope::build_marked(payload_read, targets);
    let telescope = match payload.iter().any(|(_, label, _)| label.proof) {
        true => telescope.with_proofs(
            &payload
                .iter()
                .map(|(_, label, _)| label.proof)
                .collect::<Vec<_>>(),
        ),
        false => telescope,
    };
    let params = params
        .iter()
        .map(|(_, binder, _)| binder)
        .collect::<Vec<_>>();

    Ok((
        Atom::from(case.label.as_str()),
        InductParam::over(&params, telescope),
    ))
}

/// A record's entry from a structure's form: its parameters by their words, the sort it lands in, and its fields, each plain or under `@` and by its label where it has one.
pub fn record_entry_read(
    record: &TopStruct<CoreText>,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<StructDecl, ReadError> {
    let members = record
        .fields
        .iter()
        .map(|field| match &field.param.func_params {
            Some(_) => Err(ReadError::at(
                Refusal::ProgramForm("a field's signature sugar"),
                field.param.type_.span(),
            )),
            None => {
                let word = field
                    .param
                    .label
                    .as_ref()
                    .map_or("_", |label| label.as_str());
                let label = LabelRead::of(&field.param.core, word, field.param.type_.span())?;
                Ok((field.plicity, label, &field.param.type_))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;

    let mut entry = entry_with_fields(&record.params, &record.result_sort, &members, reader)?;
    entry_template(&mut entry, record.rep_pub, name, scheme, fields)?;

    Ok(entry)
}

/// A record's entry from a concept's form: its fields, each method by its label and each superclass edge under `use` with no word; and the words its methods are written by, an edge's empty.
pub fn concept_entry_read(
    concept: &TopConcept<CoreText>,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<(StructDecl, Vec<String>), ReadError> {
    let members = concept
        .fields
        .iter()
        .map(|field| match &field.func_params {
            Some(_) => Err(ReadError::at(
                Refusal::ProgramForm("a method's signature sugar"),
                field.type_.span(),
            )),
            None => {
                let word = field.label.as_ref().map_or("_", |label| label.as_str());
                let label = LabelRead::of(&field.core, word, field.type_.span())?;
                Ok((field.plicity, label, &field.type_))
            }
        })
        .collect::<Result<Vec<_>, _>>()?;
    // A superclass edge is named by no field label of the concept's, whatever word a later field reads it by.
    let words = concept
        .fields
        .iter()
        .map(|field| match (field.plicity, &field.label) {
            (Plicity::Explicit, Some(label)) => label.to_string(),
            _ => String::new(),
        })
        .collect();
    let mut record = entry_with_fields(&concept.params, &concept.result_sort, &members, reader)?;
    entry_template(&mut record, concept.rep_pub, name, scheme, fields)?;

    Ok((record, words))
}

/// A record's entry at its template — its representation's visibility as the form writes it, its name's parent as its module, no polarities or variances, the scheme its form states — then each part its decorator sets.
fn entry_template(
    record: &mut StructDecl,
    rep_public: bool,
    name: &Global,
    scheme: &UniverseContext,
    fields: &mut Fields<'_>,
) -> Result<(), ReadError> {
    record.rep_public = rep_public;
    record.module = parent(name);
    record.universe_context = scheme.clone();
    entry_fields_read(
        fields,
        &mut record.module,
        &mut record.polarities,
        &mut record.variances,
    )
}

/// A record's entry as its form spells it: its parameters, its fields under them, and the sort it lands in, every other part empty for [`entry_template`] to set.
fn entry_with_fields(
    params: &[crate::FuncTypeParam<CoreText>],
    result_sort: &crate::Term<CoreText>,
    fields: &[TelescopeEntry<'_>],
    reader: &mut Reader<'_>,
) -> Result<StructDecl, ReadError> {
    let params = params
        .iter()
        .map(parameter_entry)
        .collect::<Result<Vec<_>, _>>()?;
    let (read, fields_read) = telescope_read(&params, reader, &mut |reader, _| {
        telescope_read(fields, reader, &mut |_, _| Ok(()))
            .map(|(read, ())| proving_telescope(Telescope::build_marked(read, ()), fields))
    })?;

    Ok(StructDecl {
        universe_context: UniverseContext::empty(),
        arity: proving_telescope(Telescope::build_marked(read, fields_read), &params),
        result_sort: core_read(result_sort, reader)?,
        module: Qualifier::empty(),
        rep_public: false,
        polarities: Vec::new(),
        variances: Vec::new(),
    })
}
