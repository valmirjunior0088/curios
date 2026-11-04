//! Core text read back into Core: the state a read threads, what it refuses, and the decorator's fields as each node takes them.
//!
//! **A form builds its template and a decorator sets what differs.** Each node's reader stands beside its writer, in the module of its kind, and builds what its form builds, reading off the decorator each field the node holds where the decorator states one; a field no node takes is refused, by name and where it stands. So the shortest text is the canonical one, and what the printer writes reads back as the term it printed.
//!
//! **A word an enclosing binder binds is its variable.** A binder is opened at a free local of its own, minted with its word as its hint, and closed over once its scope is read; a word no binder binds is a free local, one for each word, since a local's number means something only in the declaration that minted it.
//!
//! **What Core text refuses is a form whose meaning needs typing or resolution**: an operator through a concept, `!`, `choose`, a held place or a hole, a string, a name only resolution could place. Each is refused by name, where it stands, unless a decorator states the node it is — an elaboration-only form written to be refused by a checker.

use {
    super::{
        bound_read, family_read, foreign_read, function_read, function_type_read, infix_read,
        instance_read, intrinsic_read, let_read, literal_read, match_read, metavar_read,
        node_transient_read, packed_read, projection_read, rec_read, record_read, sort_read,
        tuple_read, tuple_type_read, variant_read,
    },
    crate::{
        Apply, CoreForm, CoreText, CoreValue, Decorated, DecoratorField, FormScope, Literal,
        NumLit, ProofLiteral, StructLit, StructLitEntry, Subterm, Term, TupleField, TupleType,
    },
    curios_abi::ForeignFunction,
    curios_common::{Plicity, Qualifier, Sign, Span, recurse},
    curios_core::{Free, Global, Intrinsic, Nat, Quality, QualityParam},
    std::{collections::HashMap, fmt, sync::Arc},
};

/// Why a text of Core text makes no Core, and where.
#[derive(Debug, Clone, PartialEq)]
pub struct ReadError {
    pub refusal: Refusal,
    pub span: Option<Span>,
}

/// What a reader refuses.
#[derive(Debug, Clone, PartialEq)]
pub enum Refusal {
    /// A form a program writes and Core text does not: its meaning is the elaborator's to decide.
    ProgramForm(&'static str),
    /// A field the decorator states that the node it attaches to does not hold.
    UnknownField(String),
    /// A field the decorator states twice.
    RepeatedField(String),
    /// A `node` no form of this shape fills.
    UnknownNode(String),
    /// A value of a shape its field does not take.
    Malformed(&'static str),
    /// An intrinsic no variant of Core's is named by.
    UnknownIntrinsic(String),
    /// A name Core text writes by its absolute path, or a binder's word, written relative.
    RelativeName(String),
    /// A form that implies a definition, read with no scope to make it against.
    NoScope(&'static str),
    /// A declaration a form implies from, which the text and its scope do not hold.
    Undeclared(String),
}

impl fmt::Display for ReadError {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.refusal {
            Refusal::ProgramForm(form) => write!(
                formatter,
                "{form} is a program's, and Core text writes the node it elaborates to"
            ),
            Refusal::UnknownField(field) => {
                write!(
                    formatter,
                    "a decorator states `{field}`, which this node does not hold"
                )
            }
            Refusal::RepeatedField(field) => {
                write!(formatter, "a decorator states `{field}` twice")
            }
            Refusal::UnknownNode(node) => {
                write!(formatter, "`node = {node}` names no node this form fills")
            }
            Refusal::Malformed(expected) => write!(formatter, "expected {expected}"),
            Refusal::UnknownIntrinsic(variant) => {
                write!(formatter, "`%{variant}` names no intrinsic of Core")
            }
            Refusal::RelativeName(name) => write!(
                formatter,
                "`{name}` is neither an absolute path nor a binder's word"
            ),
            Refusal::NoScope(form) => write!(
                formatter,
                "{form} is made against the scope a reader is handed, and none was"
            ),
            Refusal::Undeclared(name) => write!(formatter, "`{name}` is declared nowhere in scope"),
        }
    }
}

impl ReadError {
    pub fn at(refusal: Refusal, span: Option<&Span>) -> Self {
        Self {
            refusal,
            span: span.cloned(),
        }
    }
}

/// What a read threads: the binders in scope by their words, innermost last, the free locals the text names, and what a form that implies a definition is made against.
pub struct Reader<'s> {
    scope: Option<&'s FormScope>,
    /// The rows a foreign call names, by their declarations: the scope's, and the text's own forms.
    rows: HashMap<Global, Arc<ForeignFunction>>,
    bound: Vec<(String, Free)>,
    free: HashMap<String, Free>,
    minted: u64,
}

impl<'s> Reader<'s> {
    /// A reader of a text standing over `scope`, where it is handed one.
    pub fn new(scope: Option<&'s FormScope>) -> Self {
        Self {
            scope,
            rows: scope.map(|scope| scope.rows.clone()).unwrap_or_default(),
            bound: Vec::new(),
            free: HashMap::new(),
            minted: 0,
        }
    }

    /// What forms are made against, or the refusal of `form` where there is no scope.
    pub fn scope(
        &self,
        form: &'static str,
        span: Option<&Span>,
    ) -> Result<&'s FormScope, ReadError> {
        self.scope
            .ok_or_else(|| ReadError::at(Refusal::NoScope(form), span))
    }

    /// The row the foreign declaration `name` states.
    pub fn row(&self, name: &Global) -> Option<&Arc<ForeignFunction>> {
        self.rows.get(name)
    }

    /// Note the row `name` states, which a call later in the text names.
    pub fn declare_row(&mut self, name: Global, row: Arc<ForeignFunction>) {
        self.rows.insert(name, row);
    }

    /// A fresh local, under `word` where it has one.
    pub fn fresh(&mut self, word: Option<&str>) -> Free {
        let free = Free::local(self.minted, word);
        self.minted += 1;
        free
    }

    /// `read` with a binder in scope for each of `words`, in order, each opened at a local of its own, which `read` is handed to close over: `_` binds nothing a later word reads.
    pub fn binding<R>(&mut self, words: &[&str], read: impl FnOnce(&mut Self, &[Free]) -> R) -> R {
        let depth = self.bound.len();
        let frees = words
            .iter()
            .map(|word| {
                let free = self.fresh(Some(*word).filter(|word| *word != "_"));
                if *word != "_" {
                    self.bound.push((word.to_string(), free));
                }
                free
            })
            .collect::<Vec<_>>();
        let read = read(self, &frees);
        self.bound.truncate(depth);

        read
    }

    /// The local `word` names: the innermost binder's that binds it, or a free local of its own.
    pub fn word(&mut self, word: &str) -> Free {
        if let Some((_, free)) = self.bound.iter().rev().find(|(bound, _)| bound == word) {
            return *free;
        }
        if let Some(free) = self.free.get(word) {
            return *free;
        }
        let free = self.fresh(Some(word));
        self.free.insert(word.to_string(), free);
        free
    }
}

/// The fields a decorator states of one node, each taken by the reader of what it sets; one no reader takes is refused.
pub struct Fields<'t> {
    fields: Vec<(&'t DecoratorField, bool)>,
    span: Option<&'t Span>,
}

impl<'t> Fields<'t> {
    /// No field: an undecorated form.
    pub fn none() -> Self {
        Self {
            fields: Vec::new(),
            span: None,
        }
    }

    /// The fields `fields` states, each once, of the node written at `span`.
    pub fn of(fields: &'t [DecoratorField], span: Option<&'t Span>) -> Result<Self, ReadError> {
        for (index, field) in fields.iter().enumerate() {
            if fields[..index]
                .iter()
                .any(|earlier| earlier.label == field.label)
            {
                return Err(ReadError::at(
                    Refusal::RepeatedField(field.label.to_string()),
                    field.label.span().or(span),
                ));
            }
        }

        Ok(Self {
            fields: fields.iter().map(|field| (field, false)).collect(),
            span,
        })
    }

    /// The value of the field `label`, where the decorator states one, taken.
    pub fn take(&mut self, label: &str) -> Option<&'t CoreValue> {
        let (field, taken) = self
            .fields
            .iter_mut()
            .find(|(field, _)| field.label.as_str() == label)?;
        *taken = true;

        Some(&field.value)
    }

    /// The refusal of the first field no reader took.
    pub fn finish(self) -> Result<(), ReadError> {
        match self.fields.into_iter().find(|(_, taken)| !taken) {
            Some((field, _)) => Err(ReadError::at(
                Refusal::UnknownField(field.label.to_string()),
                field.label.span().or(self.span),
            )),
            None => Ok(()),
        }
    }
}

/// The refusal of a value of the wrong shape, where it is written.
pub fn malformed(expected: &'static str, value: &CoreValue) -> ReadError {
    ReadError::at(Refusal::Malformed(expected), value_span(value))
}

/// Where a value is written, where it is a term.
pub fn value_span(value: &CoreValue) -> Option<&Span> {
    match value {
        CoreValue::Term(term) => term.span(),
        CoreValue::Fields(_) | CoreValue::List(_) | CoreValue::Root => None,
    }
}

/// The term a field holds: a term, or `{}`, which a field that holds a term reads as the empty tuple type.
pub fn value_term(value: &CoreValue) -> Result<Term<CoreText>, ReadError> {
    match value {
        CoreValue::Term(term) => Ok(term.clone()),
        CoreValue::Fields(fields) if fields.is_empty() => {
            Ok(Subterm::TupleType(TupleType { fields: Vec::new() }).into())
        }
        _ => Err(malformed("a term", value)),
    }
}

/// The entries of a list a field holds, each under its mark.
pub fn value_list(value: &CoreValue) -> Result<&[(Plicity, CoreValue)], ReadError> {
    match value {
        CoreValue::List(entries) => Ok(entries),
        _ => Err(malformed("a list, `[…]`", value)),
    }
}

/// The entries of a list a field holds, none under a mark.
pub fn value_unmarked(value: &CoreValue) -> Result<Vec<&CoreValue>, ReadError> {
    value_list(value)?
        .iter()
        .map(|(mark, entry)| match mark {
            Plicity::Explicit => Ok(entry),
            _ => Err(malformed("an entry under no mark", entry)),
        })
        .collect()
}

/// The fields of a struct a field holds.
pub fn value_fields(value: &CoreValue) -> Result<Fields<'_>, ReadError> {
    match value {
        CoreValue::Fields(fields) => Fields::of(fields, None),
        _ => Err(malformed("a struct, `{…}`", value)),
    }
}

/// The word a field holds, a variant's name without its fields.
pub fn value_word(value: &CoreValue) -> Result<&str, ReadError> {
    match value {
        CoreValue::Term(term) => term_word(term).ok_or_else(|| malformed("a word", value)),
        _ => Err(malformed("a word", value)),
    }
}

/// The word `term` is, where it is a single segment's name.
pub fn term_word(term: &Term<CoreText>) -> Option<&str> {
    match term.as_subterm() {
        Subterm::Name(name) if !name.is_abs() && name.is_single() => Some(name.head()),
        _ => None,
    }
}

/// A variant a field holds: its word, and the terms it holds where it is called over them, `Struct(/P, 0)`.
pub fn value_variant(value: &CoreValue) -> Result<(&str, Vec<&Term<CoreText>>), ReadError> {
    let CoreValue::Term(term) = value else {
        return Err(malformed("a variant", value));
    };
    if let Some(word) = term_word(term) {
        return Ok((word, Vec::new()));
    }
    match term.as_subterm() {
        Subterm::Apply(Apply { head, arguments }) => {
            let word = term_word(head).ok_or_else(|| malformed("a variant", value))?;
            let held = arguments
                .iter()
                .map(|argument| match argument.plicity {
                    Plicity::Explicit => Ok(&argument.term),
                    _ => Err(malformed("a variant's field under no mark", value)),
                })
                .collect::<Result<_, _>>()?;
            Ok((word, held))
        }
        _ => Err(malformed("a variant", value)),
    }
}

/// The numeral a field holds.
pub fn value_number(value: &CoreValue) -> Result<u64, ReadError> {
    match value {
        CoreValue::Term(term) => term_number(term).ok_or_else(|| malformed("a numeral", value)),
        _ => Err(malformed("a numeral", value)),
    }
}

/// The numeral `term` is, where it is an unsigned one a `u64` holds.
pub fn term_number(term: &Term<CoreText>) -> Option<u64> {
    match term.as_subterm() {
        Subterm::NumLit(NumLit {
            magnitude,
            sign: Sign::Unmarked,
            ..
        }) => u64::try_from(magnitude).ok(),
        _ => None,
    }
}

/// The truth value a field holds.
pub fn value_bool(value: &CoreValue) -> Result<bool, ReadError> {
    match value {
        CoreValue::Term(term) => match term.as_subterm() {
            Subterm::Literal(Literal::Bool(value)) => Ok(*value),
            _ => Err(malformed("`true` or `false`", value)),
        },
        _ => Err(malformed("`true` or `false`", value)),
    }
}

/// The text a field holds, `"…"`.
pub fn value_string(value: &CoreValue) -> Result<String, ReadError> {
    match value {
        CoreValue::Term(term) => {
            term_string(term).ok_or_else(|| malformed("a string, `\"…\"`", value))
        }
        _ => Err(malformed("a string, `\"…\"`", value)),
    }
}

/// The text `term` is, where it is a string.
pub fn term_string(term: &Term<CoreText>) -> Option<String> {
    match term.as_subterm() {
        Subterm::ProofLiteral(ProofLiteral::Str(text)) => Some(text.value.clone()),
        _ => None,
    }
}

/// The global a field names: by its absolute path, or a witness by its identity.
pub fn value_global(value: &CoreValue) -> Result<Global, ReadError> {
    match value {
        CoreValue::Term(term) => {
            term_global(term).ok_or_else(|| malformed("an absolute name", value))
        }
        _ => Err(malformed("an absolute name", value)),
    }
}

/// The global `term` names, where it is an absolute path or a witness.
pub fn term_global(term: &Term<CoreText>) -> Option<Global> {
    match term.as_subterm() {
        Subterm::Name(name) if name.is_abs() => Some(Global::Authored(*name.qualifier())),
        Subterm::Core(CoreForm::Witness(id)) => Some(Global::Witness(*id)),
        _ => None,
    }
}

/// The module a field names: by its path, or the root, `/`.
pub fn value_module(value: &CoreValue) -> Result<Qualifier, ReadError> {
    match value {
        CoreValue::Root => Ok(Qualifier::empty()),
        CoreValue::Term(term) => match term.as_subterm() {
            Subterm::Name(name) if name.is_abs() => Ok(*name.qualifier()),
            _ => Err(malformed("a module's absolute path", value)),
        },
        _ => Err(malformed("a module's absolute path", value)),
    }
}

/// `term` as the Core node its form builds, under the decorator it is written under where it is one.
pub fn core_read(
    term: &Term<CoreText>,
    reader: &mut Reader<'_>,
) -> Result<curios_core::Term, ReadError> {
    recurse(|| match term.as_subterm() {
        Subterm::Core(CoreForm::Decorated(Decorated {
            fields,
            term: operand,
        })) => {
            let mut fields = Fields::of(fields, term.span())?;
            let read = match fields.take("node") {
                Some(node) => node_read(node, operand, &mut fields, reader)?,
                None => form_read(operand, &mut fields, reader)?,
            };
            fields.finish()?;
            Ok(read)
        }
        _ => form_read(term, &mut Fields::none(), reader),
    })
}

/// What `term`'s form builds, each field `fields` states of it set.
fn form_read(
    term: &Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<curios_core::Term, ReadError> {
    let span = term.span();
    let refused = |form: &'static str| Err(ReadError::at(Refusal::ProgramForm(form), span));
    match term.as_subterm() {
        Subterm::Type => sort_read(Quality::Type, fields),
        Subterm::Prop => sort_read(Quality::Prop, fields),
        Subterm::Sort => sort_read(Quality::Param(QualityParam(0)), fields),
        Subterm::Name(name) if name.is_abs() => {
            instance_read(Global::Authored(*name.qualifier()), fields)
        }
        Subterm::Name(name) if name.is_single() => {
            Ok(curios_core::Term::free_var(&reader.word(name.head())))
        }
        Subterm::Name(name) => Err(ReadError::at(
            Refusal::RelativeName(name.qualifier().join()),
            span,
        )),
        Subterm::Held => bound_read(fields, span),
        Subterm::Goal => metavar_read(fields, reader, span),
        Subterm::Core(CoreForm::Witness(id)) => instance_read(Global::Witness(*id), fields),
        Subterm::Core(CoreForm::Intrinsic(intrinsic)) => intrinsic_read(intrinsic, reader, span),
        Subterm::Core(CoreForm::Decorated(_)) => Err(ReadError::at(
            Refusal::Malformed("one decorator over a node: a node's fields are stated together"),
            span,
        )),
        Subterm::Literal(Literal::Bool(value)) => {
            Ok(curios_core::Term::intrinsic(Intrinsic::Bool(*value)))
        }
        Subterm::Literal(Literal::Bytes(segments)) => packed_read(segments, span),
        Subterm::NumLit(NumLit {
            magnitude,
            sign: Sign::Unmarked,
            ..
        }) => Ok(curios_core::Term::intrinsic(Intrinsic::Nat(Nat::new(
            magnitude.clone(),
        )))),
        Subterm::FuncType(function) => function_type_read(function, reader),
        Subterm::Func(function) => function_read(function, reader),
        Subterm::Apply(apply) => {
            let Apply { head, arguments } = apply;
            Ok(curios_core::Term::apply_marked(
                core_read(head, reader)?,
                arguments
                    .iter()
                    .map(|argument| Ok((argument.plicity, core_read(&argument.term, reader)?)))
                    .collect::<Result<Vec<_>, ReadError>>()?,
            ))
        }
        Subterm::TupleType(tuple) => tuple_type_read(tuple, reader),
        Subterm::Tuple(tuple) => tuple_read(tuple, reader),
        Subterm::Proj(projection) => projection_read(projection, fields, reader),
        Subterm::StructLit(literal) => literal_read(literal, fields, reader),
        Subterm::Match(elimination) => match_read(elimination, fields, reader),
        Subterm::Let(block) => let_read(block, reader),
        Subterm::NumLit(_) => refused("a signed numeral"),
        Subterm::Literal(Literal::Flt(_)) => refused("a decimal"),
        Subterm::Literal(Literal::List(_)) => refused("a list literal"),
        Subterm::Infix(_) => refused("an operator"),
        Subterm::Bang(_) => refused("a `!`"),
        Subterm::Choose(_) => refused("a `choose`"),
        Subterm::Hole => refused("a hole"),
        Subterm::Derive => refused("a derived body"),
        Subterm::ProofLiteral(ProofLiteral::Str(_)) => refused("a string"),
        Subterm::ProofLiteral(ProofLiteral::Char(_)) => refused("a character"),
        Subterm::Intrinsic(_) => refused("an intrinsic node a program plants"),
    }
}

/// The node `node` names, filled by `operand`'s form: a nominal node by the call of its declaration's definition, a local group by its `let`, and an elaboration-only form by the spelling it was read from.
fn node_read(
    node: &CoreValue,
    operand: &Term<CoreText>,
    fields: &mut Fields<'_>,
    reader: &mut Reader<'_>,
) -> Result<curios_core::Term, ReadError> {
    let name = value_word(node)?;
    match name {
        "Variant" => variant_read(operand, fields, reader),
        "InductType" => family_read(operand, fields, reader),
        "StructType" => record_read(operand, fields, reader),
        "Foreign" => foreign_read(operand, reader),
        "Rec" => rec_read(operand, reader),
        "Infix" => infix_read(operand, reader),
        "NumLit" | "Bang" | "Ascribed" | "Derive" | "Name" | "Method" | "Matrix" => {
            node_transient_read(name, operand, fields, reader)
        }
        _ => Err(ReadError::at(
            Refusal::UnknownNode(name.to_string()),
            value_span(node).or(operand.span()),
        )),
    }
}

/// The fields of a variant written as a structure's literal is, `Implicit { func = …, binder = None }`, each taken by its label; one no reader takes is refused.
pub struct LiteralFields<'t> {
    entries: Vec<(&'t str, &'t Term<CoreText>, bool)>,
    span: Option<&'t Span>,
}

impl<'t> LiteralFields<'t> {
    /// The term the field `label` holds, taken, or the refusal of its absence.
    pub fn term(&mut self, label: &str) -> Result<&'t Term<CoreText>, ReadError> {
        let span = self.span;
        let (_, term, taken) = self
            .entries
            .iter_mut()
            .find(|(written, _, _)| *written == label)
            .ok_or_else(|| ReadError::at(Refusal::Malformed("a variant's every field"), span))?;
        *taken = true;

        Ok(term)
    }

    /// The refusal of the first field no reader took.
    pub fn finish(self) -> Result<(), ReadError> {
        match self.entries.into_iter().find(|(_, _, taken)| !taken) {
            Some((label, term, _)) => Err(ReadError::at(
                Refusal::UnknownField(label.to_string()),
                term.span().or(self.span),
            )),
            None => Ok(()),
        }
    }
}

/// The variant a literal of fields is written under, where its head is a single word: `Implicit` in `Implicit { … }`.
pub fn literal_variant(literal: &StructLit<CoreText>) -> Option<&str> {
    (!literal.head.is_abs() && literal.head.is_single()).then(|| literal.head.head())
}

/// The fields `literal` writes, each by its label: a variant's fields are each written, and named.
pub fn literal_fields(literal: &StructLit<CoreText>) -> Result<LiteralFields<'_>, ReadError> {
    let span = literal.head.span();
    let entries = literal
        .entries
        .iter()
        .map(|entry| match entry {
            StructLitEntry::Field(TupleField {
                label: Some(label),
                func_params: None,
                value,
            }) => Ok((label.as_str(), value, false)),
            _ => Err(ReadError::at(
                Refusal::Malformed("a variant's field, `label = value`"),
                span,
            )),
        })
        .collect::<Result<_, _>>()?;
    if literal.params.is_some() {
        return Err(ReadError::at(
            Refusal::Malformed("a variant's fields, its name taking no arguments"),
            span,
        ));
    }

    Ok(LiteralFields { entries, span })
}
