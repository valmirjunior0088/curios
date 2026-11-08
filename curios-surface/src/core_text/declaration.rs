//! A nominal declaration in its form: a family's, a record's and a concept's.
//!
//! **A family is `induct /F(params): (indices) -> pub Sort`, a case to a line, `| tag(payload): (targets)`, and `end`**; a record is `struct /R(params): pub Sort { fields }` and a concept `concept /C(params): pub Sort { fields }`, a superclass edge among a concept's fields written `use T`. A form builds its declaration's entry and its former ([`curios_core::Definition::former`]), and a family's its constructors ([`curios_core::Definition::constructors`]), so the module's items those make are written nowhere else. The inner `pub` is the entry's `rep_public`.
//!
//! **Each binder is written by its word, under its mark**, `@n: Nat`, or as its type alone where nothing reads it, and a later type reads it by that word: a family's cases read its parameters by the head's words. A case's parameters are its family's, each hidden as a value constructor takes it, and a case whose parameters are not states them as its signature holds them, under their marks: `| #{params = [@Type, use /Show(A)]} tag(…)`.

use {
    super::{
        Fields, OpenedEntry, ReadError, Reader, Refusal, Tree, core_read, malformed,
        telescope_read, universe_context_read, value, value_fields, value_list, value_module,
        value_term, value_unmarked, value_word,
    },
    crate::{
        CasePayloadParam, CoreHead, CoreText, CoreValue, Frame, FuncTypeParam, Shown, TopCase,
        TopConcept, TopInduct, TopStruct, decoration_fields, sub,
    },
    curios_common::{Plicity, Qualifier},
    curios_core::{
        Atom, Bound, Cursor, Free, Global, InductDecl, InductParam, Polarity, StructDecl,
        Telescope, Term, UniverseContext, Variance,
    },
};

/// One binder of a declaration's telescope: its mark, its type, the variable a later type reads it by, whether the rest of its own telescope reads it, and whether it has a hint or is so read.
pub struct Binder {
    pub mark: Plicity,
    pub type_: Term,
    pub label: Free,
    pub variable: Term,
    pub read: bool,
    pub named: bool,
}

impl Binder {
    /// Whether this binder is read as a value: in place of a superclass edge's word, which resolution reads it by, its word.
    fn variable_is(&self, term: &Term) -> bool {
        term.mentions_free(&self.label)
    }

    /// The word this binder is written by where it has a hint or `read` says something reads it, and none where it is written as its type alone.
    fn word(&self, read: bool, frame: Frame) -> Option<String> {
        (self.named || read).then(|| frame.spelling.label(&self.label))
    }

    /// This binder under its mark: by its word where it has a hint or `read` says something reads it, and as its type alone otherwise.
    pub fn written(&self, read: bool, frame: Frame) -> FuncTypeParam<Shown> {
        FuncTypeParam {
            plicity: self.mark,
            label: self.word(read, frame),
            type_: sub(self.type_.clone(), frame),
        }
    }
}

/// The entries of a telescope from `cursor` to its end, each opened at the variable of a word of its own.
pub fn binders<B: Bound>(cursor: &mut Cursor<'_, B>, frame: Frame) -> Vec<Binder> {
    let mut binders = Vec::new();
    while let (Some((hint, type_)), Some(mark)) = (cursor.entry(), cursor.mark()) {
        let label = frame.label(cursor.label());
        let read = cursor.binder_used();
        let named = hint.is_some() || read;
        let variable = Term::free_var(&label);
        cursor.advance(variable.clone());
        binders.push(Binder {
            mark,
            type_,
            label,
            variable,
            read,
            named,
        });
    }

    binders
}

/// What a value constructor takes a family's parameter under: a `use` one a witness slot, every other one hidden.
pub fn hidden(mark: Plicity) -> Plicity {
    match mark {
        Plicity::Witness => Plicity::Witness,
        Plicity::Explicit | Plicity::Implicit => Plicity::Implicit,
    }
}

/// The head a declaration of `name` is written under, its decorator empty: the caller states what it sets.
fn head_of(name: &Global) -> CoreHead<Shown> {
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
pub fn family_declared(name: &Global, entry: &InductDecl, frame: Frame) -> TopInduct<Shown> {
    let mut cursor = entry.arity.cursor();
    let params = binders(&mut cursor, frame);
    let indices = cursor
        .body()
        .expect("a cursor past every entry holds its payload");
    let indices = binders(&mut indices.cursor(), frame);
    // A parameter a case reads is written by its word, which the case reads it by.
    let read = params
        .iter()
        .map(|param| {
            entry
                .constructors
                .iter()
                .any(|(_, signature)| reads(&signature.telescope, &params, param))
        })
        .collect::<Vec<_>>();
    let written = params
        .iter()
        .zip(read)
        .map(|(param, read)| param.written(read, frame))
        .collect();
    let indexed = !indices.is_empty();
    let cases = entry
        .constructors
        .iter()
        .map(|(tag, signature)| case_written(tag, signature, &params, indexed, frame))
        .collect::<Vec<_>>();

    TopInduct {
        doc: None,
        core: head_of(name),
        vis_pub: false,
        rep_pub: entry.rep_public,
        label: label_of(name),
        params: written,
        indices: indices
            .iter()
            .map(|index| (index.word(false, frame), sub(index.type_.clone(), frame)))
            .collect(),
        result_sort: sub(entry.result_sort.clone(), frame),
        cases,
    }
}

/// Whether a case whose signature is `telescope` reads the family's parameter `param` past its own parameters, which are read at `params`.
fn reads(telescope: &Telescope<Vec<Term>>, params: &[Binder], param: &Binder) -> bool {
    let mut cursor = telescope.cursor();
    for each in params {
        if cursor.entry().is_none() {
            return false;
        }
        cursor.advance(each.variable.clone());
    }
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

/// A case: its tag over its payload, then its targets where its family has indices, under the parameters it states where they are not its family's.
fn case_written(
    tag: &Atom,
    signature: &InductParam,
    params: &[Binder],
    indexed: bool,
    frame: Frame,
) -> TopCase<Shown> {
    let mut cursor = signature.telescope.cursor();
    let mut stated = Vec::with_capacity(params.len());
    let mut own = true;
    for param in params {
        let (Some((_, type_)), Some(mark)) = (cursor.entry(), cursor.mark()) else {
            own = false;
            break;
        };
        own &= type_ == param.type_ && mark == hidden(param.mark);
        stated.push((mark, value(sub(type_, frame))));
        cursor.advance(param.variable.clone());
    }
    let payload = binders(&mut cursor, frame)
        .iter()
        .map(|binder| CasePayloadParam {
            plicity: binder.mark,
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
    let fields = match own {
        true => Vec::new(),
        false => decoration_fields(vec![("params", CoreValue::List(stated))]),
    };

    TopCase {
        doc: None,
        core: fields,
        label: crate::Label::from(tag.as_string()),
        payload,
        target,
    }
}

/// A record's form: its parameters, its sort and its fields, each field under its mark and by the word it is written by where it has one, with the word each is written by in order — empty for a superclass edge and for a field nothing names.
pub struct RecordWritten {
    pub params: Vec<FuncTypeParam<Shown>>,
    pub rep_pub: bool,
    pub result_sort: Tree,
    pub fields: Vec<(Plicity, Option<String>, Tree)>,
    pub words: Vec<String>,
}

impl RecordWritten {
    /// The record as a `struct` declaration of `name`.
    pub fn structure(self, name: &Global) -> TopStruct<Shown> {
        TopStruct {
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
                .map(|(plicity, label, type_)| crate::StructField {
                    doc: None,
                    plicity,
                    param: crate::TupleTypeParam {
                        label: label.map(crate::Label::from),
                        func_params: None,
                        type_,
                    },
                })
                .collect(),
        }
    }

    /// The record as a `concept` declaration of `name`.
    pub fn concept(self, name: &Global) -> TopConcept<Shown> {
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
                .map(|(plicity, label, type_)| crate::ConceptField {
                    doc: None,
                    plicity,
                    label: label.map(crate::Label::from),
                    func_params: None,
                    type_,
                })
                .collect(),
        }
    }
}

/// A record's form.
pub fn record_declared(record: &StructDecl, frame: Frame) -> RecordWritten {
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
                Plicity::Witness => field.read.then(|| frame.spelling.label(&field.label)),
                _ => field.word(false, frame),
            };
            (field.mark, label, sub(field.type_.clone(), frame))
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

/// The parts of an entry its decorator sets, each in place over its template: the module its representation belongs to, the polarities and the variances its elaboration filed, and its scheme apart from its former's — what the writer's `entry_fields` writes of an entry beside its form.
pub fn entry_fields_read(
    fields: &mut Fields<'_>,
    universe_context: &mut UniverseContext,
    module: &mut Qualifier,
    polarities: &mut Vec<Polarity>,
    variances: &mut Vec<Variance>,
) -> Result<(), ReadError> {
    if let Some(value) = fields.take("module") {
        *module = value_module(value)?;
    }
    if let Some(value) = fields.take("polarities") {
        *polarities = value_unmarked(value)?
            .into_iter()
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
        *variances = value_unmarked(value)?
            .into_iter()
            .map(|variance| match value_word(variance)? {
                "Irrelevant" => Ok(Variance::Irrelevant),
                "Invariant" => Ok(Variance::Invariant),
                _ => Err(malformed("a variance, as Core names it", variance)),
            })
            .collect::<Result<_, _>>()?;
    }
    if let Some(value) = fields.take("entry") {
        let mut entry = value_fields(value)?;
        if let Some(context) = entry.take("universe_context") {
            *universe_context = universe_context_read(context)?;
        }
        entry.finish()?;
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
        .map(|param| {
            (
                param.plicity,
                param.label.as_deref().unwrap_or("_"),
                &param.type_,
            )
        })
        .collect::<Vec<_>>();
    let indices = family
        .indices
        .iter()
        .map(|(label, type_)| (Plicity::Explicit, label.as_deref().unwrap_or("_"), type_))
        .collect::<Vec<_>>();
    let (params, (indices, cases)) = telescope_read(&params, reader, &mut |reader, params| {
        let (indices, ()) = telescope_read(&indices, reader, &mut |_, _| Ok(()))?;
        let cases = family
            .cases
            .iter()
            .map(|case| case_read(case, params, reader))
            .collect::<Result<Vec<_>, _>>()?;
        Ok((indices, cases))
    })?;
    let indices = Telescope::build(
        indices
            .into_iter()
            .map(|(_, binder, type_)| (binder, type_)),
        (),
    );

    let mut entry = InductDecl {
        universe_context: scheme.clone(),
        arity: Telescope::build_marked(params, indices),
        constructors: cases.into(),
        result_sort: core_read(&family.result_sort, reader)?,
        module: parent(name),
        rep_public: family.rep_pub,
        polarities: Vec::new(),
        variances: Vec::new(),
    };
    entry_fields_read(
        fields,
        &mut entry.universe_context,
        &mut entry.module,
        &mut entry.polarities,
        &mut entry.variances,
    )?;

    Ok(entry)
}

/// A case: its parameters — its family's, each hidden as a value constructor takes it, or those its decorator states, under the head's words — then its payload by its own words, then its targets.
fn case_read(
    case: &TopCase<CoreText>,
    params: &[OpenedEntry],
    reader: &mut Reader<'_>,
) -> Result<(Atom, InductParam), ReadError> {
    let mut fields = Fields::of(&case.core, case.label.span())?;
    let own = match fields.take("params") {
        None => params
            .iter()
            .map(|(mark, binder, type_)| (hidden(*mark), *binder, type_.clone()))
            .collect::<Vec<_>>(),
        Some(value) => {
            let stated = value_list(value)?;
            if stated.len() > params.len() {
                return Err(malformed("no more parameters than its family's", value));
            }
            stated
                .iter()
                .zip(params)
                .map(|((mark, entry), (_, binder, _))| {
                    Ok((*mark, *binder, core_read(&value_term(entry)?, reader)?))
                })
                .collect::<Result<Vec<_>, ReadError>>()?
        }
    };
    fields.finish()?;
    let payload = case
        .payload
        .iter()
        .map(|param| {
            (
                param.plicity,
                param.label.as_deref().unwrap_or("_"),
                &param.type_,
            )
        })
        .collect::<Vec<_>>();
    let (payload, targets) = telescope_read(&payload, reader, &mut |reader, _| {
        case.target
            .iter()
            .flatten()
            .map(|target| core_read(target, reader))
            .collect::<Result<Vec<_>, _>>()
    })?;

    Ok((
        Atom::from(case.label.as_str()),
        InductParam {
            telescope: Telescope::build_marked(own.into_iter().chain(payload), targets),
        },
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
            None => Ok((
                field.plicity,
                field
                    .param
                    .label
                    .as_ref()
                    .map_or("_", |label| label.as_str()),
                &field.param.type_,
            )),
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
            None => Ok((
                field.plicity,
                field.label.as_ref().map_or("_", |label| label.as_str()),
                &field.type_,
            )),
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

/// A record's entry at its template — its representation's visibility as the form writes it, its name's parent as its module, no polarities or variances, its former's scheme — then each part its decorator sets.
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
        &mut record.universe_context,
        &mut record.module,
        &mut record.polarities,
        &mut record.variances,
    )
}

/// A record's entry as its form spells it: its parameters, its fields under them, and the sort it lands in, every other part empty for [`entry_template`] to set.
fn entry_with_fields(
    params: &[crate::FuncTypeParam<CoreText>],
    result_sort: &crate::Term<CoreText>,
    fields: &[(Plicity, &str, &crate::Term<CoreText>)],
    reader: &mut Reader<'_>,
) -> Result<StructDecl, ReadError> {
    let params = params
        .iter()
        .map(|param| {
            (
                param.plicity,
                param.label.as_deref().unwrap_or("_"),
                &param.type_,
            )
        })
        .collect::<Vec<_>>();
    let (params, fields) = telescope_read(&params, reader, &mut |reader, _| {
        telescope_read(fields, reader, &mut |_, _| Ok(()))
            .map(|(fields, ())| Telescope::build_marked(fields, ()))
    })?;

    Ok(StructDecl {
        universe_context: UniverseContext::empty(),
        arity: Telescope::build_marked(params, fields),
        result_sort: core_read(result_sort, reader)?,
        module: Qualifier::empty(),
        rep_public: false,
        polarities: Vec::new(),
        variances: Vec::new(),
    })
}
