//! The translation of a Core term back to the surface tree: what a report's term is printed through.
//!
//! The inverse of lowering and elaboration, for a reader standing at one place. A term a report shows is translated here and printed by [`print_term`], the printer that formats source, so what a reader is shown is a form the grammar produces: a match as its patterns, a projection by its label, a witness as resolution would restore it. `curios-core`'s own printer prints what is read about the compiler, a dump and a kernel's refusal, and the term a translation declines.
//!
//! **A translation is whole or it is none.** [`from_core`] hands back a surface term, or `None`: the term is past what a report prints ([`curios_core::Minting::elides`]), or holds a form no program spells — a witness resolution would not put back, a hidden member with no label read off a value, a handle the host minted. The surface tree gains no node for what is left out, and a report that is handed `None` shows the term as the compiler holds it and says so.
//!
//! **A witness is left out where resolution would put it back, decided over what each candidate is a witness of.** [`restorable`] runs resolution's three rules at the reader's position ([`resolved`]): a binder, or a superclass path off one, answers a goal where its application is the goal's, is passed over where the two certainly differ — a rigid head on each side, and not one head ([`rigid_head`]) — and stops the walk where neither is known, the witness then being written. The walk reduces nothing, so a difference is known by heads alone, and it does not hold what an enclosing arm's pattern solved.
//!
//! **It reads what the Core printer reads**: the reader's [`curios_core::Spelling`], which names each global as resolution finds it from where the reader stands and each binder as the report's rename spells it, and the lexicons of the units in scope, for a nominal family's marks, a structure's fields, a constructor's payload and a concept's methods. Binders are labelled through the render's [`curios_core::Minting`] in the order this walk opens them, which is the order its dry run opened them in.

use {
    crate::{
        Apply, Argument, BinPattern, BinSegment, Field, Func, FuncParam, FuncType, FuncTypeParam,
        Infix, Label, Let, LetBinding, LetGroup, LetSignature, ListEntry, ListPattern, Literal,
        Match, MatchPattern, MatchPatternField, MatrixArm, Name, NatLiteral, NatPattern, NumLit,
        Pattern, PatternField, Proj, ProofLiteral, Radix, StrLit, StructLit, StructLitEntry,
        Subterm, Term, Tuple, TupleField, TupleType, TupleTypeParam, print_term, spell,
    },
    curios_core::Bound,
    curios_num::{Floating, Grain, Integer, Natural},
    curios_print::Printer,
    curios_utilities::{InfixOp, Plicity, Qualifier, Sign, recurse},
    std::{cell::Cell, rc::Rc},
};

/// The reader's printer a pipeline hands a report ([`curios_core::Spelling::with_reader_printer`]): a term translated back to the surface tree and printed as source is.
pub struct Translation;

impl curios_core::ReaderPrinter for Translation {
    fn print(
        &self,
        term: &curios_core::Term,
        spelling: &Rc<curios_core::Spelling>,
        mint: &curios_core::Minting,
    ) -> Option<Printer> {
        from_core(term, spelling, mint).map(print_term)
    }

    fn spell(
        &self,
        spellings: &curios_core::Spellings,
        island: &Qualifier,
        owner: Option<&curios_core::Global>,
        global: &curios_core::Global,
    ) -> Option<String> {
        spell(spellings, island, owner, global)
    }
}

/// `term` as a reader standing where `spelling` does writes it, its binders labelled through `mint`, or `None` where it cannot be written.
pub fn from_core(
    term: &curios_core::Term,
    spelling: &Rc<curios_core::Spelling>,
    mint: &curios_core::Minting,
) -> Option<Term> {
    // A level is no part of what a program writes, so a render asked to show the instances is the compiler's own: the report that asks has found two terms no source tells apart.
    if !spelling.erases_universes() {
        return None;
    }
    let frame = Frame {
        spelling,
        mint,
        witnesses: None,
        definitions: None,
        inferred: false,
    };

    within_reader(term.clone(), frame, &spelling.reader().witnesses)
}

/// The state the walk threads: the render-constant spelling, the render's mint, and the witness binders in scope.
#[derive(Clone, Copy)]
struct Frame<'a> {
    spelling: &'a Rc<curios_core::Spelling>,
    mint: &'a curios_core::Minting,
    /// The innermost witness binder in scope, chained outward. Each node lives on the stack of the arm that opened its binder.
    witnesses: Option<&'a WitnessNode<'a>>,
    /// The innermost group of local definitions in scope, chained outward as the witness binders are.
    definitions: Option<&'a Definitions<'a>>,
    /// Whether the term this frame is handed stands where no expected type reaches it: the head of a call or of a projection, a scrutinee. What a checked position would supply is then written, a lambda's parameter types and a match's motive. Read by the node the frame is handed to and by none below it ([`sub`], [`unchecked`]).
    inferred: bool,
}

impl<'a> Frame<'a> {
    /// This frame with `node` innermost in its witness scope.
    fn with_witness<'b>(self, node: &'b WitnessNode<'b>) -> Frame<'b>
    where
        'a: 'b,
    {
        Frame {
            spelling: self.spelling,
            mint: self.mint,
            witnesses: Some(node),
            definitions: self.definitions,
            inferred: self.inferred,
        }
    }

    /// This frame with `group` innermost among its local definitions.
    fn with_definitions<'b>(self, group: &'b Definitions<'b>) -> Frame<'b>
    where
        'a: 'b,
    {
        Frame {
            spelling: self.spelling,
            mint: self.mint,
            witnesses: self.witnesses,
            definitions: Some(group),
            inferred: self.inferred,
        }
    }

    /// Whether `label` is a local definition in scope, and so no binder.
    fn defines(&self, label: &curios_core::Free) -> bool {
        std::iter::successors(self.definitions, |group| group.parent)
            .any(|group| group.labels.contains(label))
    }

    fn table(&self) -> Option<&'a curios_core::WitnessSpelling> {
        self.spelling.witness_table()
    }

    /// The word a binder or a reference to one is written as.
    fn word(&self, label: &curios_core::Free) -> String {
        self.spelling.label(label)
    }
}

/// One witness binder in scope: its label, the concept application its type names where the table knows the concept, whether a reference to it reached the tree, and the binder outside it.
struct WitnessNode<'a> {
    label: curios_core::Free,
    concept: Option<(curios_core::Global, Vec<curios_core::Term>)>,
    used: Cell<bool>,
    parent: Option<&'a WitnessNode<'a>>,
}

impl<'a> WitnessNode<'a> {
    fn new(label: curios_core::Free, type_: &curios_core::Term, frame: Frame<'a>) -> Self {
        Self {
            label,
            concept: frame.table().and_then(|table| table.application(type_)),
            used: Cell::new(false),
            parent: frame.witnesses,
        }
    }

    /// What this binder answers, where the table knows its concept.
    fn answered(&self) -> Option<Answered> {
        let (concept, arguments) = self.concept.clone()?;

        Some(Answered {
            concept,
            arguments: Some(arguments),
        })
    }
}

/// The names one `let` or recursive group binds, and the group outside it: what tells a local definition, which stands for its value, from a binder.
struct Definitions<'a> {
    labels: &'a [curios_core::Free],
    parent: Option<&'a Definitions<'a>>,
}

/// The witness binders in scope, innermost first.
fn chain<'a>(first: Option<&'a WitnessNode<'a>>) -> impl Iterator<Item = &'a WitnessNode<'a>> {
    std::iter::successors(first, |node| node.parent)
}

/// [`tree`] under the reader's witness binders, outermost first, each node on this stack while the rest of the walk borrows it.
fn within_reader(
    term: curios_core::Term,
    frame: Frame,
    binders: &[(curios_core::Free, curios_core::Term)],
) -> Option<Term> {
    match binders.split_first() {
        None => sub(term, frame),
        Some(((label, type_), rest)) => {
            let node = WitnessNode::new(*label, type_, frame);

            within_reader(term, frame.with_witness(&node), rest)
        }
    }
}

/// A child of the tree, standing where its type is expected, or `None` past what a report prints: the one place the walk is bounded, and the one place its depth is guarded.
fn sub(term: curios_core::Term, frame: Frame) -> Option<Term> {
    placed(term, frame, false)
}

/// [`sub`] for a child no expected type reaches: the head of a call or of a projection, a scrutinee.
fn unchecked(term: curios_core::Term, frame: Frame) -> Option<Term> {
    placed(term, frame, true)
}

fn placed(term: curios_core::Term, frame: Frame, inferred: bool) -> Option<Term> {
    if frame.mint.elides(&term) {
        return None;
    }

    recurse(|| tree(term, Frame { inferred, ..frame }))
}

/// A name as it is written: a path from the root where it leads with a slash, a path from the scope otherwise.
fn named(spelled: &str) -> Term {
    Subterm::Name(name_of(spelled)).into()
}

/// [`named`], as the name itself.
fn name_of(spelled: &str) -> Name {
    let (absolute, path) = match spelled.strip_prefix('/') {
        Some(path) => (true, path),
        None => (false, spelled),
    };

    Name::new(absolute, Qualifier::from(path.split('/')))
}

/// A declaration as the reader writes it. A witness has no name a program writes, so one reaching here is one resolution would not put back.
fn global(name: &curios_core::Global, frame: Frame) -> Option<Term> {
    match name {
        curios_core::Global::Authored(_) => Some(named(&frame.spelling.symbol(name))),
        _ => None,
    }
}

/// A numeral nobody wrote, at its natural width.
fn numeral(magnitude: Natural, sign: Sign) -> Term {
    Subterm::NumLit(NumLit {
        magnitude,
        radix: Radix::Dec(0),
        sign,
    })
    .into()
}

fn apply(head: Term, arguments: Vec<Argument>) -> Term {
    Subterm::Apply(Apply { head, arguments }).into()
}

fn argument(plicity: Plicity, term: Term) -> Argument {
    Argument { term, plicity }
}

/// The wildcard that holds a hidden member's place: `use _`, `@_`.
fn placeholder() -> Term {
    named("_")
}

/// One node of the tree: every Core form as the surface writes it.
fn tree(term: curios_core::Term, frame: Frame) -> Option<Term> {
    use curios_core::Subterm as Core;

    Some(match curios_core::Term::unwrap_or_clone(term) {
        // A level is implicit in the surface and settles by where the word stands, so a sort is its word.
        Core::Sort(quality, _) => match quality {
            curios_core::Quality::Type => Subterm::Type.into(),
            curios_core::Quality::Prop => Subterm::Prop.into(),
            curios_core::Quality::Param(_) | curios_core::Quality::Meta(_) => Subterm::Sort.into(),
        },
        // An instance is the name it is of: no source states the levels.
        Core::Instance(instance) => sub(instance.head.to_term(), frame)?,
        Core::Intrinsic(intrinsic) => {
            let form = curios_core::intrinsic_form(intrinsic, frame.spelling.keeps_grouping());

            recurse(|| written_intrinsic(form, frame))?
        }
        // A call across the wire reads as a call of its declaration, the type a row that does not return is handed under its mark. A row of no operand is still called.
        Core::Foreign(function, declared, mut arguments) => {
            let explicits = arguments.split_off(usize::from(function.diverges()));

            apply(
                global(&declared, frame)?,
                marked(arguments, explicits, frame)?,
            )
        }
        Core::FuncType(curios_core::FuncType { telescope }) => {
            let marks = telescope.marks();
            let mut params = Vec::with_capacity(telescope.len());
            let output = parameter_types(telescope.cursor(), &marks, frame, &mut params)?;

            Subterm::FuncType(FuncType {
                params: params
                    .into_iter()
                    .map(|param| param.expect("every slot is filled"))
                    .collect(),
                output,
            })
            .into()
        }
        Core::Func(curios_core::Func { telescope }) => {
            let marks = telescope.marks();
            // A type-former lambda reads as the former it eta-expands, where a program writes that former.
            if let Some(former) = curios_core::former_eta(&telescope, &marks)
                && writable_former(&former, telescope.terminal())
            {
                return written_former(former, frame);
            }
            let mut params = Vec::with_capacity(telescope.len());
            let body = lambda_parameters(
                telescope.cursor(),
                &marks,
                frame,
                frame.inferred,
                &mut params,
            )?;

            Subterm::Func(Func {
                params: params
                    .into_iter()
                    .map(|param| param.expect("every slot is filled"))
                    .collect(),
                body,
            })
            .into()
        }
        Core::Apply(curios_core::Apply { head, arguments }) => {
            if let Core::Proj(curios_core::Proj {
                head: witness,
                field: curios_core::Field::Struct(_, index),
            }) = &*head
                && let Some(method) = method(witness, *index, Some(arguments.as_slice()), frame)
            {
                return method;
            }

            apply(unchecked(head, frame)?, call_arguments(arguments, frame)?)
        }
        Core::TupleType(curios_core::TupleType { telescope }) => {
            let mut fields = Vec::with_capacity(telescope.len());
            let mut cursor = telescope.cursor();
            while let Some((hint, type_)) = cursor.entry() {
                let label = frame.mint.cursor_label(&cursor);
                frame.mint.note_tuple_label(&label);
                // An unnameable label nothing references is left out, so the field reads the way source wrote it.
                let named = hint.is_some() || cursor.binder_used();
                fields.push(TupleTypeParam {
                    label: named.then(|| Label::from(frame.word(&label))),
                    func_params: None,
                    type_: sub(type_, frame)?,
                });
                cursor.advance(curios_core::Term::free_var(&label));
            }

            Subterm::TupleType(TupleType { fields }).into()
        }
        Core::Tuple(curios_core::Tuple { fields, names }) => {
            let mut names = names.into_iter().chain(std::iter::repeat(None));
            let fields = fields
                .into_iter()
                .map(|field| {
                    Some(TupleField {
                        label: names.next().flatten(),
                        func_params: None,
                        value: sub(field, frame)?,
                    })
                })
                .collect::<Option<Vec<_>>>()?;

            Subterm::Tuple(Tuple { fields }).into()
        }
        Core::Proj(curios_core::Proj { head, field }) => {
            if let curios_core::Field::Struct(_, index) = &field
                && let Some(method) = method(&head, *index, None, frame)
            {
                return method;
            }
            let field = written_field(&field, frame)?;

            Subterm::Proj(Proj {
                head: unchecked(head, frame)?,
                field,
            })
            .into()
        }
        // The parameters, then the indices, one call each where the family has both, as its type former is applied at a use site: `Sized(Nat)(1)`.
        Core::InductType(curios_core::InductType {
            name,
            params,
            indices,
            ..
        }) => {
            let marks = frame
                .spelling
                .nominal_marks(&name, params.len() + indices.len());
            let offset = params.len();
            let mut written = global(&name, frame)?;
            for (group, offset) in [(params, 0), (indices, offset)] {
                if !group.is_empty() {
                    written = apply(written, nominal_arguments(group, marks, offset, frame)?);
                }
            }

            written
        }
        // The constructor called, the type's parameters left to where it stands: `Result/success(42)`.
        Core::Variant(curios_core::Variant {
            name,
            params,
            tag,
            payload,
            ..
        }) => {
            let constructor = name
                .qualifier()
                .map(|path| curios_core::Global::Authored(path.with(&tag.as_string())))?;
            // The marks a constructor is called by lead with the family's parameters, every one hidden, and end with its payload's.
            let called = frame.spelling.payload_marks(&name, &tag);
            let leading = called.map_or(0, |marks| marks.len().saturating_sub(payload.len()));
            let mark = |slot: usize, otherwise: Plicity| {
                called
                    .and_then(|marks| marks.get(slot))
                    .copied()
                    .unwrap_or(otherwise)
            };
            // Called with no argument where it has no payload, as a program writes one: the bare name is the constructor, and `Option/none()` its value.
            let head = global(&constructor, frame)?;
            // The family's parameters are left to where the value stands, and written where nothing there states them, and where the payload opens with a hidden member: that one stands in the parameters' run, and written alone it would be read as the first of them.
            let opens_hidden =
                !payload.is_empty() && mark(leading, Plicity::Explicit) != Plicity::Explicit;
            let stated = match (frame.inferred || opens_hidden) && params.len() == leading {
                true => params,
                false => Vec::new(),
            };
            let members = stated
                .into_iter()
                .enumerate()
                .map(|(slot, parameter)| (mark(slot, Plicity::Implicit), parameter))
                .chain(
                    payload
                        .into_iter()
                        .enumerate()
                        .map(|(slot, member)| (mark(leading + slot, Plicity::Explicit), member)),
                )
                .collect();

            apply(head, hidden_run(members, frame)?)
        }
        Core::StructType(curios_core::StructType { name, params, .. }) => {
            let marks = frame.spelling.nominal_marks(&name, params.len());
            let head = global(&name, frame)?;
            match params.is_empty() {
                true => head,
                false => apply(head, nominal_arguments(params, marks, 0, frame)?),
            }
        }
        Core::Struct(literal) => written_struct(literal, frame)?,
        Core::Match(elimination) => written_match(elimination, frame)?,
        Core::Let(curios_core::Let { bindings, tail, .. }) => {
            let labels = frame.mint.scope_labels(&tail);
            let group = Definitions {
                labels: &labels,
                parent: frame.definitions,
            };
            let frame = frame.with_definitions(&group);
            let terms = labels
                .iter()
                .map(curios_core::Term::free_var)
                .collect::<Vec<_>>();
            let refs = terms.iter().collect::<Vec<_>>();
            let mut groups = Vec::with_capacity(bindings.len());
            for (index, binding) in bindings.iter().enumerate() {
                let type_ = binding.type_().release(&refs[..index]);
                let value = binding.value().release(&refs[..index]);
                groups.push(LetGroup {
                    members: vec![LetBinding {
                        binder: Pattern::Binder(Label::from(frame.word(&labels[index]))),
                        signature: LetSignature::Name {
                            type_: Some(sub(type_, frame)?),
                            body: sub(value, frame)?,
                        },
                    }],
                });
            }

            Subterm::Let(Let {
                groups,
                tail: sub(tail.open(&refs), frame)?,
            })
            .into()
        }
        // A recursive group is the `let … and …;` whose members see one another.
        Core::Rec(curios_core::Rec { group, tail }) => {
            let labels = frame.mint.scope_labels(&tail);
            let defined = Definitions {
                labels: &labels,
                parent: frame.definitions,
            };
            let frame = frame.with_definitions(&defined);
            let terms = labels
                .iter()
                .map(curios_core::Term::free_var)
                .collect::<Vec<_>>();
            let refs = terms.iter().collect::<Vec<_>>();
            let members = group
                .iter()
                .cloned()
                .enumerate()
                .map(|(index, member)| {
                    Some(LetBinding {
                        binder: Pattern::Binder(Label::from(frame.word(&labels[index]))),
                        signature: LetSignature::Name {
                            type_: Some(sub(member.type_.open(&refs), frame)?),
                            body: sub(member.body.open(&refs), frame)?,
                        },
                    })
                })
                .collect::<Option<Vec<_>>>()?;

            Subterm::Let(Let {
                groups: vec![LetGroup { members }],
                tail: sub(tail.open(&refs), frame)?,
            })
            .into()
        }
        Core::Var(var) => {
            let name = var.as_free()?;
            // A reference reaching the tree is what keeps a witness binder's name.
            if let Some(node) = chain(frame.witnesses).find(|node| &node.label == name) {
                node.used.set(true);
            }
            match name {
                curios_core::Free::Global(name) => global(name, frame)?,
                local => named(&frame.word(local)),
            }
        }
        Core::Transient(curios_core::Transient::NumLit(curios_core::NumLit::Number {
            magnitude,
            sign,
        })) => numeral(magnitude, sign),
        Core::Transient(curios_core::Transient::NumLit(curios_core::NumLit::Character(
            character,
        ))) => Subterm::ProofLiteral(ProofLiteral::Char(character)).into(),
        Core::Transient(curios_core::Transient::Infix(curios_core::Infix { op, left, right })) => {
            infix(op, sub(left, frame)?, sub(right, frame)?)
        }
        Core::Transient(curios_core::Transient::Derive) => Subterm::Derive.into(),
        // A sequencing site holds its continuation hoisted and an ascription a type beside a term: no source spells either.
        Core::Transient(curios_core::Transient::Bang(_) | curios_core::Transient::Ascribed(_)) => {
            return None;
        }
        Core::Transient(curios_core::Transient::Matrix(matrix)) => written_matrix(matrix, frame)?,
        // A goal is what a program writes for a term it leaves open, whatever elaboration knows of it.
        Core::Metavar(_) => Subterm::Goal.into(),
    })
}

/// A match whose patterns are not yet compiled, as the `match` it was lowered from: each row as its pattern over the names its body binds. A `choose` bind arm reads back as the match of two arms it stands for.
///
/// The motive's binders are opened ahead of the scrutinee's and the arms' after both, the order `curios-core`'s printer opens them in.
fn written_matrix(matrix: curios_core::Matrix, frame: Frame) -> Option<Term> {
    let curios_core::Matrix { head, motive, rows } = matrix;

    let motive = match motive {
        Some(motive) => Some(sub(motive, frame)?),
        None => None,
    };
    let head = unchecked(head, frame)?;
    let arms = rows
        .into_iter()
        .map(|curios_core::Row { pattern, body }| {
            let curios_core::Subterm::Func(function) = &*body else {
                return None;
            };
            let mut params = Vec::with_capacity(function.telescope.len());
            let body = lambda_parameters(
                function.telescope.cursor(),
                &function.telescope.marks(),
                frame,
                false,
                &mut params,
            )?;
            let mut names = params.into_iter().map(|param| {
                match param.expect("every slot is filled").pattern {
                    Pattern::Binder(name) => name,
                    Pattern::Tuple(_) | Pattern::Struct { .. } => {
                        unreachable!("a lambda's parameter is read back as a name")
                    }
                }
            });

            Some(MatrixArm {
                pattern: written_pattern(&pattern, &mut names),
                body,
                span: None,
            })
        })
        .collect::<Option<Vec<_>>>()?;

    Some(Subterm::Match(Match { head, motive, arms }).into())
}

/// A row's pattern as a program writes it, each binder under the next of `names`.
fn written_pattern(
    pattern: &curios_core::Pattern,
    names: &mut dyn Iterator<Item = Label>,
) -> MatchPattern {
    let name = next_name;

    match pattern {
        curios_core::Pattern::Binder => MatchPattern::Binder(name(names)),
        curios_core::Pattern::Variant { tag, args } => MatchPattern::Variant {
            tag: tag.as_string(),
            args: args
                .iter()
                .map(|(plicity, arg)| (*plicity, written_pattern(arg, names)))
                .collect(),
        },
        curios_core::Pattern::Tuple(fields) => MatchPattern::Tuple(written_fields(fields, names)),
        curios_core::Pattern::Struct { head, fields, .. } => MatchPattern::Struct {
            head: name_of(head),
            fields: written_fields(fields, names),
        },
        curios_core::Pattern::Bool(value) => MatchPattern::Bool(*value),
        curios_core::Pattern::Zero => MatchPattern::Nat(NatPattern::Zero),
        curios_core::Pattern::Succ(hypothesis) => {
            let pred_label = name(names);

            MatchPattern::Nat(NatPattern::Succ {
                pred_label,
                ih: hypothesis.as_ref().map(|bind| written_bind(bind, names)),
            })
        }
        curios_core::Pattern::Literal(value) => {
            MatchPattern::Nat(NatPattern::Lit(NatLiteral(value.clone(), Radix::Dec(0))))
        }
        curios_core::Pattern::Character(character) => MatchPattern::Char(*character),
        curios_core::Pattern::Nil => MatchPattern::List(ListPattern::Nil),
        curios_core::Pattern::Cons(hypothesis) => {
            let head_label = name(names);
            let tail_label = name(names);

            MatchPattern::List(ListPattern::Cons {
                head_label,
                tail_label,
                ih: hypothesis.as_ref().map(|bind| written_bind(bind, names)),
            })
        }
        curios_core::Pattern::End(grain) => MatchPattern::Bin(BinPattern::End(*grain)),
        curios_core::Pattern::Atom(grain, hypothesis) => {
            let head_label = name(names);
            let tail_label = name(names);

            MatchPattern::Bin(BinPattern::Atom {
                grain: *grain,
                head_label,
                tail_label,
                ih: hypothesis.as_ref().map(|bind| written_bind(bind, names)),
            })
        }
    }
}

/// The next of a row's names, or `_` past the last: a row's body binds one name for each binder its pattern writes.
fn next_name(names: &mut dyn Iterator<Item = Label>) -> Label {
    names.next().unwrap_or_else(|| Label::from("_".to_string()))
}

fn written_fields(
    fields: &[(Option<String>, curios_core::Pattern)],
    names: &mut dyn Iterator<Item = Label>,
) -> Vec<MatchPatternField> {
    fields
        .iter()
        .map(|(label, field)| MatchPatternField {
            label: label.clone(),
            value: written_pattern(field, names),
        })
        .collect()
}

/// What a fold's hypothesis is bound by, as a program writes it, each binder under the next of `names`.
fn written_bind(bind: &curios_core::Bind, names: &mut dyn Iterator<Item = Label>) -> Pattern {
    match bind {
        curios_core::Bind::Binder => Pattern::Binder(next_name(names)),
        curios_core::Bind::Fields(fields) => Pattern::Tuple(written_binds(fields, names)),
        curios_core::Bind::Struct { head, fields, .. } => Pattern::Struct {
            head: name_of(head),
            fields: written_binds(fields, names),
        },
    }
}

/// The fields of a hypothesis's pattern, each as [`written_bind`] writes it.
fn written_binds(
    fields: &[(Option<String>, curios_core::Bind)],
    names: &mut dyn Iterator<Item = Label>,
) -> Vec<PatternField> {
    fields
        .iter()
        .map(|(label, field)| PatternField {
            label: label.clone(),
            value: written_bind(field, names),
        })
        .collect()
}

fn infix(op: InfixOp, left: Term, right: Term) -> Term {
    Subterm::Infix(Infix { op, left, right }).into()
}

/// The arguments of a call of a `/sys` declaration: the type arguments under their mark, then the written ones.
fn marked(
    implicits: Vec<curios_core::Term>,
    explicits: Vec<curios_core::Term>,
    frame: Frame,
) -> Option<Vec<Argument>> {
    implicits
        .into_iter()
        .map(|term| (Plicity::Implicit, term))
        .chain(explicits.into_iter().map(|term| (Plicity::Explicit, term)))
        .map(|(plicity, term)| Some(argument(plicity, sub(term, frame)?)))
        .collect()
}

/// A function type's parameters from `cursor` on, each written into `params` in order, then its output: every entry type under the binders before it, and a witness binder's node in scope for everything after it.
fn parameter_types(
    mut cursor: curios_core::Cursor<'_, curios_core::Term>,
    marks: &[Plicity],
    frame: Frame,
    params: &mut Vec<Option<FuncTypeParam>>,
) -> Option<Term> {
    let Some((hint, type_)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let label = frame.mint.cursor_label(&cursor);
    let plicity = marks
        .get(cursor.args().len())
        .copied()
        .unwrap_or(Plicity::Explicit);
    // A hintless binder is one the compiler minted, so its label is written only where the rest of the telescope refers to it: `(B) -> C` reads as written.
    let named = hint.is_some() || cursor.binder_used();
    let written = sub(type_.clone(), frame)?;
    let slot = params.len();
    params.push(None);
    cursor.advance(curios_core::Term::free_var(&label));

    let (output, named) = match plicity == Plicity::Witness {
        true => {
            let node = WitnessNode::new(label, &type_, frame);
            let output = parameter_types(cursor, marks, frame.with_witness(&node), params)?;
            // The grammar writes no name beside the witness mark, so a reference resolution would not restore has nothing to refer to.
            if node.used.get() {
                return None;
            }

            (output, false)
        }
        false => (parameter_types(cursor, marks, frame, params)?, named),
    };

    params[slot] = Some(FuncTypeParam {
        plicity,
        label: named.then(|| frame.word(&label)),
        type_: written,
    });
    Some(output)
}

/// A lambda's parameters from `cursor` on, written into `params`, then its body, a witness binder's node in scope for it. A lambda's witness member is `use _`, as the surface writes it: one the body still refers to by a name resolution would not restore is a term no source spells.
///
/// `annotated` writes each parameter's type beside it: what a lambda needs where no expected type states them. The grammar writes none beside a witness, so a lambda taking one is written only where its type is expected.
fn lambda_parameters(
    mut cursor: curios_core::Cursor<'_, curios_core::Term>,
    marks: &[Plicity],
    frame: Frame,
    annotated: bool,
    params: &mut Vec<Option<FuncParam>>,
) -> Option<Term> {
    let Some((hint, type_)) = cursor.entry() else {
        return sub(cursor.body().expect("a cursor past every entry"), frame);
    };
    let label = frame.mint.cursor_label(&cursor);
    let plicity = marks
        .get(cursor.args().len())
        .copied()
        .unwrap_or(Plicity::Explicit);
    // Asked before the cursor moves past the binder it asks about.
    let named = hint.is_some() || cursor.binder_used();
    let word = frame.word(&label);
    let annotation = match (annotated, plicity) {
        (false, _) => None,
        (true, Plicity::Witness) => return None,
        (true, _) => Some(sub(type_.clone(), frame)?),
    };
    let slot = params.len();
    params.push(None);
    cursor.advance(curios_core::Term::free_var(&label));

    let (body, named) = match plicity == Plicity::Witness {
        true => {
            let node = WitnessNode::new(label, &type_, frame);
            let body =
                lambda_parameters(cursor, marks, frame.with_witness(&node), annotated, params)?;
            if node.used.get() {
                return None;
            }

            (body, false)
        }
        false => (
            lambda_parameters(cursor, marks, frame, annotated, params)?,
            named,
        ),
    };

    params[slot] = Some(FuncParam {
        plicity,
        pattern: Pattern::Binder(Label::from(match named {
            true => word,
            false => "_".to_string(),
        })),
        annotation,
    });
    Some(body)
}

/// A call's arguments as a reader writes them, each under its mark: a `use` argument resolution would restore is left out where nothing hidden is written after it in its run, and holds its place as `use _` where something is, since hidden members are written in order from the first of their run and a later one written past a slot left out would be read at that slot.
fn call_arguments(arguments: Vec<curios_core::Argument>, frame: Frame) -> Option<Vec<Argument>> {
    let members = arguments
        .into_iter()
        .map(|argument| (argument.plicity, argument.term))
        .collect();

    hidden_run(members, frame)
}

/// The members of one telescope filled at a site — a call's arguments, a literal's entries — each under its mark, by the rule [`call_arguments`] states.
fn hidden_run(members: Vec<(Plicity, curios_core::Term)>, frame: Frame) -> Option<Vec<Argument>> {
    let restored = members
        .iter()
        .map(|(plicity, term)| *plicity == Plicity::Witness && restorable(term, frame))
        .collect::<Vec<_>>();

    // Read from the back: whether a hidden member is written later in the run a position stands in.
    let mut written_after = vec![false; members.len()];
    let mut later = false;
    for (index, (plicity, _)) in members.iter().enumerate().rev() {
        written_after[index] = later;
        later = *plicity != Plicity::Explicit && (later || !restored[index]);
    }

    let mut written = Vec::with_capacity(members.len());
    for (((plicity, term), restored), written_after) in
        members.into_iter().zip(restored).zip(written_after)
    {
        match (restored, written_after) {
            (true, false) => {}
            (true, true) => written.push(argument(Plicity::Witness, placeholder())),
            (false, _) => written.push(argument(plicity, sub(term, frame)?)),
        }
    }

    Some(written)
}

/// A nominal type's arguments from position `offset` of its declaration on, each marked as the declaration marks it, and a witness resolution would restore left out.
fn nominal_arguments(
    group: Vec<curios_core::Term>,
    marks: Option<&[Plicity]>,
    offset: usize,
    frame: Frame,
) -> Option<Vec<Argument>> {
    let members = group
        .into_iter()
        .enumerate()
        .map(|(index, term)| {
            let mark = marks.and_then(|marks| marks.get(offset + index)).copied();

            (mark.unwrap_or(Plicity::Explicit), term)
        })
        .collect();

    hidden_run(members, frame)
}

/// Whether a program writes the former a lambda eta-expands: its bare name, where the binders took every argument, or an indexed family at its parameters, which takes its indices in a call of their own. A family at some of its parameters is no call the grammar has — `Result(Error)` for `(A) => Result(Error, A)` — and the lambda is written as it stands.
fn writable_former(former: &curios_core::FormerEta, terminal: &curios_core::Term) -> bool {
    match former {
        curios_core::FormerEta::Intrinsic(_) => true,
        curios_core::FormerEta::Nominal(_, prefix, _) => {
            prefix.is_empty()
                || matches!(
                    &**terminal,
                    curios_core::Subterm::InductType(curios_core::InductType { indices, .. })
                        if !indices.is_empty()
                )
        }
    }
}

/// A type former a lambda eta-expands: the name alone where the binders took every argument, the application to what they left otherwise, each argument under the mark the declaration gives it.
fn written_former(former: curios_core::FormerEta, frame: Frame) -> Option<Term> {
    match former {
        curios_core::FormerEta::Intrinsic(path) => {
            global(&curios_core::intrinsic_global(path), frame)
        }
        curios_core::FormerEta::Nominal(name, prefix, arity) => {
            let marks = frame.spelling.nominal_marks(&name, arity);
            let head = global(&name, frame)?;
            match prefix.is_empty() {
                true => Some(head),
                false => Some(apply(head, nominal_arguments(prefix, marks, 0, frame)?)),
            }
        }
    }
}

/// The field a projection reads, as it is written: a tuple's by its place, and a structure's by its label, or by its place among the plain fields where it has none. A hidden field with no label has no spelling: a position counts the plain ones.
fn written_field(field: &curios_core::Field, frame: Frame) -> Option<Field> {
    match field {
        curios_core::Field::Tuple(index) | curios_core::Field::Position(index) => {
            Some(Field::Index(*index))
        }
        curios_core::Field::Label(label) => Some(Field::Label(label.clone())),
        curios_core::Field::Struct(structure, slot) => {
            let fields = frame.spelling.structure_fields(structure)?;
            let (mark, label) = fields.get(*slot)?;
            match (label, mark) {
                (Some(label), _) => Some(Field::Label(label.clone())),
                (None, Plicity::Explicit) => {
                    let position = fields[..*slot]
                        .iter()
                        .filter(|(mark, _)| *mark == Plicity::Explicit)
                        .count();

                    Some(Field::Index(position))
                }
                (None, _) => None,
            }
        }
    }
}

/// An intrinsic as its one table writes it ([`curios_core::intrinsic_form`]): a declaration of `/sys` named as the reader reaches it, a literal as the grammar spells it.
fn written_intrinsic(form: curios_core::IntrinsicForm, frame: Frame) -> Option<Term> {
    use curios_core::IntrinsicForm as Form;

    let sys = |path: &str| global(&curios_core::intrinsic_global(path), frame);

    Some(match form {
        Form::Named(path) => sys(path)?,
        Form::Call {
            path,
            implicits,
            explicits,
        } => {
            let arguments = marked(implicits, explicits, frame)?;
            // A row with no parameters is a constant in `/sys`, named the way a constant is.
            match arguments.is_empty() {
                true => sys(&path)?,
                false => apply(sys(&path)?, arguments),
            }
        }
        Form::Infix(op, left, right) => infix(op, sub(left, frame)?, sub(right, frame)?),
        Form::Bool(value) => Subterm::Literal(Literal::Bool(value)).into(),
        Form::Nat(value) => numeral(value, Sign::Unmarked),
        Form::NatAbove { tail, floor } => infix(
            InfixOp::Add,
            sub(tail, frame)?,
            numeral(floor, Sign::Unmarked),
        ),
        Form::Byte(value) => Subterm::NumLit(NumLit {
            magnitude: Natural::from(value),
            radix: Radix::Hex(2),
            sign: Sign::Unmarked,
        })
        .into(),
        Form::Int(value) => {
            let sign = match value < Integer::from(0i32) {
                true => Sign::Negative,
                false => Sign::Positive,
            };

            numeral(value.magnitude(), sign)
        }
        Form::Flt(value) => written_flt(value, frame)?,
        Form::Packed(grain, entries) => {
            let segments = entries
                .into_iter()
                .map(|entry| written_segment(entry, frame))
                .collect::<Option<Vec<_>>>()?;

            Subterm::Literal(Literal::Bin(grain, segments)).into()
        }
        Form::List(entries) => {
            let entries = entries
                .into_iter()
                .map(|entry| {
                    Some(match entry {
                        curios_core::LiteralEntry::Spread(run) => {
                            ListEntry::Spread(sub(run, frame)?)
                        }
                        curios_core::LiteralEntry::Item(item) => ListEntry::Elem(sub(item, frame)?),
                        // A list holds no packed atom.
                        curios_core::LiteralEntry::Bit(_) | curios_core::LiteralEntry::Byte(_) => {
                            return None;
                        }
                    })
                })
                .collect::<Option<Vec<_>>>()?;

            Subterm::Literal(Literal::List(entries)).into()
        }
        // A token the host minted, which no source spells.
        Form::Handle(_) => return None,
    })
}

/// One entry of a packed literal: a constant atom as the numeral it is written by, a term's atom, or a spread.
fn written_segment(entry: curios_core::LiteralEntry, frame: Frame) -> Option<BinSegment> {
    Some(match entry {
        curios_core::LiteralEntry::Bit(bit) => {
            BinSegment::Atom(numeral(Natural::from(u8::from(bit)), Sign::Unmarked))
        }
        curios_core::LiteralEntry::Byte(byte) => BinSegment::Atom(
            Subterm::NumLit(NumLit {
                magnitude: Natural::from(byte),
                radix: Radix::Hex(0),
                sign: Sign::Unmarked,
            })
            .into(),
        ),
        curios_core::LiteralEntry::Item(item) => BinSegment::Atom(sub(item, frame)?),
        curios_core::LiteralEntry::Spread(run) => BinSegment::Spread(sub(run, frame)?),
    })
}

/// A `Flt` as what reads back as it, bit for bit: the literal where one spells it, and any other NaN as the call building it from its bytes.
fn written_flt(value: Floating, frame: Frame) -> Option<Term> {
    let spelled =
        value.is_finite() || !value.is_nan() || value.abs().to_bits() == Floating::nan().to_bits();
    if spelled {
        return Some(Subterm::Literal(Literal::Flt(value)).into());
    }

    let bytes =
        curios_core::Term::intrinsic(curios_core::Intrinsic::Bin(Grain::X, value.to_le_bytes()));

    Some(apply(
        global(&curios_core::intrinsic_global("Flt/of_le_bytes"), frame)?,
        vec![argument(Plicity::Explicit, sub(bytes, frame)?)],
    ))
}

/// A concept application as the walk holds one: the concept, and the arguments it is applied at where they could be read.
#[derive(Clone)]
struct Answered {
    concept: curios_core::Global,
    arguments: Option<Vec<curios_core::Term>>,
}

/// What a witness argument is, as resolution sees one.
enum WitnessForm<'a> {
    /// A binder in scope, or a superclass path off one: the fields the path projects, the first off the binder, and what it answers.
    Local {
        node: &'a WitnessNode<'a>,
        path: Vec<usize>,
        answered: Answered,
    },
    /// A witness of the global table, and what it answers, its arguments unread where its declared telescope could not be opened.
    Global(Answered),
    /// A witness goal still open, which resolution is what it waits on.
    Pending,
}

/// Every argument along an application spine, outermost call last: `C(a)(b)` gives `[a, b]`.
fn spine_arguments(term: &curios_core::Term) -> Vec<curios_core::Term> {
    match &**term {
        curios_core::Subterm::Apply(curios_core::Apply { head, arguments }) => {
            let mut spine = spine_arguments(head);
            spine.extend(arguments.iter().map(|argument| argument.term.clone()));
            spine
        }
        _ => Vec::new(),
    }
}

/// Classify `term` as a witness, when it is one: a binder in scope, a superclass path off one, a global witness — bare, at a universe instance, or applied to its own hidden arguments — or an open witness goal.
fn witness_form<'a>(term: &curios_core::Term, frame: Frame<'a>) -> Option<WitnessForm<'a>> {
    use curios_core::Subterm as Core;

    let table = frame.table()?;
    match &**term {
        Core::Metavar(curios_core::Metavar {
            origin: curios_core::MetavarOrigin::Witness(_),
            ..
        }) => Some(WitnessForm::Pending),
        Core::Var(var) if matches!(var.as_free(), Some(curios_core::Free::Local(_))) => {
            let label = var.as_free()?;
            let node = chain(frame.witnesses).find(|node| &node.label == label)?;

            Some(WitnessForm::Local {
                node,
                path: Vec::new(),
                answered: node.answered()?,
            })
        }
        Core::Proj(curios_core::Proj {
            head,
            field: curios_core::Field::Struct(_, index),
        }) => match witness_form(head, frame)? {
            WitnessForm::Local {
                node,
                mut path,
                answered,
            } => {
                let Edge::Reached(answered) = edge(table, &answered, head, *index)? else {
                    return None;
                };
                path.push(*index);

                Some(WitnessForm::Local {
                    node,
                    path,
                    answered,
                })
            }
            WitnessForm::Global(_) | WitnessForm::Pending => None,
        },
        Core::Var(_) | Core::Instance(_) | Core::Apply(_) => {
            let curios_core::Free::Global(witness @ curios_core::Global::Witness(_)) =
                term.head_name()?
            else {
                return None;
            };
            let (concept, arguments) = table.answers(witness, &spine_arguments(term))?;

            Some(WitnessForm::Global(Answered {
                concept,
                // A concept takes a parameter, so no arguments are arguments unread.
                arguments: (!arguments.is_empty()).then_some(arguments),
            }))
        }
        _ => None,
    }
}

/// What a superclass edge is known to reach.
enum Edge {
    /// A concept application, its arguments unread where the witness's own are, or where the concept's record does not give them.
    Reached(Answered),
    /// A name the table does not know as a concept: an edge the elaborator has yet to record, written through a definition. What it answers is not known, so neither is what resolution does with it.
    Unread,
}

/// What the superclass edge at field `index` of `from`'s concept reaches, for `witness`, a witness of that application: `None` where the field is no edge.
fn edge(
    table: &curios_core::WitnessSpelling,
    from: &Answered,
    witness: &curios_core::Term,
    index: usize,
) -> Option<Edge> {
    let (_, reached) = table
        .supers(&from.concept)
        .find(|(field, _)| *field == index)?;
    if table.fields(reached).is_none() {
        return Some(Edge::Unread);
    }
    let arguments = from
        .arguments
        .as_ref()
        .and_then(|arguments| table.reached(&from.concept, arguments, witness, index))
        .filter(|(concept, _)| concept == reached)
        .map(|(_, arguments)| arguments);

    Some(Edge::Reached(Answered {
        concept: *reached,
        arguments,
    }))
}

/// Whether resolution, at the reader's position, would put `argument` back if it were left out — the only case a `use` argument may be: the witness resolution answers its application with ([`resolved`]) is the argument itself.
fn restorable(argument: &curios_core::Term, frame: Frame) -> bool {
    let Some(table) = frame.table() else {
        return false;
    };
    let (goal, taken) = match witness_form(argument, frame) {
        None => return false,
        Some(WitnessForm::Pending) => return true,
        Some(WitnessForm::Local {
            node,
            path,
            answered,
        }) => (answered, Some((node, path))),
        Some(WitnessForm::Global(answered)) => (answered, None),
    };

    match resolved(table, frame, &goal) {
        Resolved::Unknown => false,
        Resolved::Local(node, path) => {
            taken.is_some_and(|(taken, along)| std::ptr::eq(taken, node) && along == path)
        }
        // Coherence makes the table's answer the one witness it holds for the application.
        Resolved::Table => taken.is_none(),
    }
}

/// Who resolution answers a goal with, where the walk can tell.
enum Resolved<'a> {
    /// A binder in scope, or the superclass path off it the fields name.
    Local(&'a WitnessNode<'a>, Vec<usize>),
    /// The global table, nothing in scope answering first.
    Table,
    /// A candidate met on the way is neither the goal's application nor certainly another, or two are the goal's at one depth, which resolution refuses as ambiguous.
    Unknown,
}

/// Who resolution answers `goal` with at the reader's position, following `documentation/syntax.md`'s order over what each candidate is a witness of ([`meets`]): the binders in scope innermost first, the first whose application is the goal's; then their superclass projections a depth at a time, the one of a depth that is; then the global table. A binder whose concept the table does not know answers nothing and shadows nothing.
fn resolved<'a>(
    table: &curios_core::WitnessSpelling,
    frame: Frame<'a>,
    goal: &Answered,
) -> Resolved<'a> {
    // Each candidate: the binder it is a path off, the path, the witness as a term, and what it answers.
    let mut level = chain(frame.witnesses)
        .filter_map(|node| {
            Some((
                node,
                Vec::new(),
                curios_core::Term::free_var(&node.label),
                node.answered()?,
            ))
        })
        .collect::<Vec<_>>();
    let mut direct = true;
    // The superclass graph is acyclic (checked where the registries are seeded), so the walk ends once every path has run out.
    while !level.is_empty() {
        let mut found = Vec::new();
        for (node, path, _, answered) in &level {
            match meets(answered, goal, frame) {
                Meets::Same => found.push((*node, path.clone())),
                Meets::Apart => {}
                Meets::Unknown => return Resolved::Unknown,
            }
            // Among the binders themselves the first match wins, as a name shadows.
            if direct && !found.is_empty() {
                break;
            }
        }
        match (found.pop(), found.is_empty()) {
            (Some((node, path)), true) => return Resolved::Local(node, path),
            (Some(_), false) => return Resolved::Unknown,
            (None, _) => {}
        }

        direct = false;
        let mut next = Vec::new();
        for (node, path, witness, answered) in level {
            for (index, _) in table.supers(&answered.concept) {
                let reached = match edge(table, &answered, &witness, index) {
                    Some(Edge::Reached(reached)) => reached,
                    Some(Edge::Unread) => return Resolved::Unknown,
                    None => continue,
                };
                let mut along = path.clone();
                along.push(index);
                let projected =
                    curios_core::Term::from(curios_core::Subterm::Proj(curios_core::Proj {
                        head: witness.clone(),
                        field: curios_core::Field::Struct(answered.concept, index),
                    }));
                next.push((node, along, projected, reached));
            }
        }
        level = next;
    }

    Resolved::Table
}

/// What a candidate's application is to a goal's.
enum Meets {
    /// One application: the candidate answers the goal.
    Same,
    /// Certainly another: resolution passes over the candidate.
    Apart,
    /// Neither is known, so neither is what resolution does with the candidate.
    Unknown,
}

/// What `candidate` is to `goal`: another concept's, or one differing at an argument ([`differ`]), is passed over; one spelled as the goal is answers it; and anything else, an application whose arguments could not be read among them, is not known.
fn meets(candidate: &Answered, goal: &Answered, frame: Frame) -> Meets {
    if candidate.concept != goal.concept {
        return Meets::Apart;
    }
    let (Some(these), Some(those)) = (&candidate.arguments, &goal.arguments) else {
        return Meets::Unknown;
    };
    if these.len() != those.len() {
        return Meets::Unknown;
    }
    if these
        .iter()
        .zip(those)
        .any(|(this, that)| differ(this, that, frame))
    {
        return Meets::Apart;
    }

    match these == those {
        true => Meets::Same,
        false => Meets::Unknown,
    }
}

/// What a term is headed by where no reduction changes it: a binder, which converts with nothing but itself, or a type former, by the key the witness table files it under.
#[derive(PartialEq, Eq)]
enum Head {
    Binder(curios_core::Free),
    Former(curios_core::HeadKey),
}

/// Whether two terms are certainly not one: each has a rigid head and the heads differ.
fn differ(this: &curios_core::Term, that: &curios_core::Term, frame: Frame) -> bool {
    matches!(
        (rigid_head(this, frame), rigid_head(that, frame)),
        (Some(this), Some(that)) if this != that
    )
}

/// The rigid head of `term`, where it has one: the binder it is headed by, read through a function to what the function answers and down an application to what is applied, or the type former its spelling is keyed by ([`curios_core::Spelling::written_head`]), the reading a witness's own key is spelled by. Both read a name as the type it names, so two spellings of one type constructor — `M` and `(A) => M(A)`, `Io` and `(A) => Io(A)` — have one head and are never told apart. A local definition stands for its value, so it is no binder, and a term whose head only reduction decides has none.
fn rigid_head(term: &curios_core::Term, frame: Frame) -> Option<Head> {
    match local_head(term) {
        Some(local) => (!frame.defines(local)).then_some(Head::Binder(*local)),
        None => frame.spelling.written_head(term).map(Head::Former),
    }
}

/// The local name `term` is headed by, under its binders and at the head of its applications.
fn local_head(term: &curios_core::Term) -> Option<&curios_core::Free> {
    use curios_core::Subterm as Core;

    match &**term {
        Core::Func(function) => local_head(function.telescope.terminal()),
        Core::Apply(application) => local_head(&application.head),
        Core::Var(variable) => variable
            .as_free()
            .filter(|name| matches!(name, curios_core::Free::Local(_))),
        _ => None,
    }
}

/// The operator a call of the method a projection off `witness` stands for is written as: the one dispatching to that method, when the call passes it exactly two plain arguments — an open witness goal's the operator it was inserted for.
fn method_operator(
    witness: &curios_core::Term,
    index: usize,
    arguments: &[curios_core::Argument],
    frame: Frame,
) -> Option<InfixOp> {
    let [_, _] = arguments else {
        return None;
    };
    if arguments
        .iter()
        .any(|argument| argument.plicity != Plicity::Explicit)
    {
        return None;
    }
    let table = frame.table()?;
    let concept = match witness_form(witness, frame)? {
        WitnessForm::Pending => {
            let curios_core::Subterm::Metavar(curios_core::Metavar {
                origin: curios_core::MetavarOrigin::Witness(origin),
                ..
            }) = &**witness
            else {
                return None;
            };
            let curios_core::CalleeId::Operator(op) = origin.func else {
                return None;
            };

            return Some(op);
        }
        WitnessForm::Local { answered, .. } | WitnessForm::Global(answered) => answered.concept,
    };
    match table.fields(&concept)?.get(index)? {
        curios_core::FieldSpelling::Method { operator, .. } => *operator,
        curios_core::FieldSpelling::Super(_) => None,
    }
}

/// The method a projection off a witness stands for, written as a program reaches it: the operator dispatching to it between its two operands, whatever hidden arguments its elaboration filled after them, or its wrapper — called with the concept's parameters marked `@`, the witness only where resolution would not restore it, and, for a wrapper taking the method's own parameters in its group, those in the same call. `arguments` is the call the projection heads, when it heads one.
///
/// The outer `None` is a projection that is no method's, written as the projection it is; the inner one a method this walk cannot write.
fn method(
    witness: &curios_core::Term,
    index: usize,
    arguments: Option<&[curios_core::Argument]>,
    frame: Frame,
) -> Option<Option<Term>> {
    let table = frame.table()?;
    // What stands after an operator's two operands is what its own elaboration put there, a bound's proof, as an intrinsic operation's is: the reader wrote none, and the operator written again asks for it again.
    if let Some([left, right, filled @ ..]) = arguments
        && filled
            .iter()
            .all(|argument| argument.plicity == Plicity::Implicit)
        && let Some(op) = method_operator(witness, index, &[left.clone(), right.clone()], frame)
    {
        let written = sub(left.term.clone(), frame)
            .zip(sub(right.term.clone(), frame))
            .map(|(left, right)| infix(op, left, right));

        return Some(written);
    }

    let (concept, parameters) = match witness_form(witness, frame)? {
        // An open witness goal carries only the operator it was inserted for, which spells it or nothing does.
        WitnessForm::Pending => return None,
        // A path off a binder is written by its method alone, as it was before its application could be read.
        WitnessForm::Local { path, answered, .. } => {
            let parameters = match path.is_empty() {
                true => answered.arguments,
                false => None,
            };

            (answered.concept, parameters.unwrap_or_default())
        }
        WitnessForm::Global(answered) => (answered.concept, answered.arguments.unwrap_or_default()),
    };

    let curios_core::FieldSpelling::Method {
        wrapper, merged, ..
    } = table.fields(&concept)?.get(index)?.clone()
    else {
        return None;
    };

    let restored = restorable(witness, frame);
    // The witness is handed on with the parameters: whether it is left out or holds its place as `use _` is the rule every call's hidden run follows ([`call_arguments`]), and a method's own `@` arguments come after it.
    let reference =
        curios_core::Term::var(curios_core::Var::free(curios_core::Free::Global(wrapper)));
    let leading = parameters
        .into_iter()
        .map(|parameter| (Plicity::Implicit, parameter))
        .chain(std::iter::once((Plicity::Witness, witness.clone())))
        .collect::<Vec<_>>();
    let own = arguments.map(|arguments| {
        arguments
            .iter()
            .map(|argument| (argument.plicity, argument.term.clone()))
            .collect::<Vec<_>>()
    });

    let term = match (merged, own) {
        (true, Some(own)) => {
            curios_core::Term::apply_marked(reference, leading.into_iter().chain(own))
        }
        // The method as a value: its wrapper, which a checked position instantiates and eta-expands — where resolution restores the witness, since the one call that could name it would have to carry the method's own arguments too.
        (true, None) if restored => reference,
        (true, None) => return Some(None),
        (false, own) => {
            let field = curios_core::Term::apply_marked(reference, leading);
            match own {
                Some(own) => curios_core::Term::apply_marked(field, own),
                None => field,
            }
        }
    };

    Some(sub(term, frame))
}

/// A structure's literal: its head, then every field under the mark its declaration gives it — a plain field in its place, an `@` field under its mark and its name, a `use` field as a call's witness argument is written — except a certified string, which is the literal it stands for.
fn written_struct(literal: curios_core::Struct, frame: Frame) -> Option<Term> {
    let curios_core::Struct {
        name,
        params,
        fields,
        entries,
        ..
    } = literal;
    if let Some(text) = string_literal(&name, &fields, frame) {
        return text;
    }
    let Subterm::Name(head) = global(&name, frame)?.into_subterm() else {
        unreachable!("a declaration is written as a name");
    };

    // A literal elaboration has not rebuilt holds the entries it was written with, one a field, and is written back by them: its fields are the written ones, a spread's base among them, and not the declaration's slots. One whose head was applied holds the arguments with no mark, which is no form to write back.
    if !entries.is_empty() {
        if !params.is_empty() {
            return None;
        }
        let written = fields
            .into_iter()
            .zip(entries)
            .map(|(field, entry)| {
                let value = sub(field, frame)?;
                let labelled = |label| TupleField {
                    label,
                    func_params: None,
                    value: value.clone(),
                };

                Some(match entry {
                    curios_core::StructEntry::Field(label) => {
                        StructLitEntry::Field(labelled(label))
                    }
                    curios_core::StructEntry::Implicit(label) => {
                        StructLitEntry::Implicit(labelled(label))
                    }
                    curios_core::StructEntry::Use => StructLitEntry::Use(value),
                    curios_core::StructEntry::Spread => StructLitEntry::Spread(value),
                })
            })
            .collect::<Option<Vec<_>>>()?;

        return Some(
            Subterm::StructLit(StructLit {
                head,
                params: None,
                entries: written,
            })
            .into(),
        );
    }

    let declared = frame.spelling.structure_fields(&name);
    let slot = |slot: usize| declared.and_then(|declared| declared.get(slot));
    let marks = (0..fields.len())
        .map(|index| slot(index).map_or(Plicity::Explicit, |(mark, _)| *mark))
        .collect::<Vec<_>>();
    let restored = fields
        .iter()
        .zip(&marks)
        .map(|(field, mark)| *mark == Plicity::Witness && restorable(field, frame))
        .collect::<Vec<_>>();
    // Which hidden entries are written is the rule a call's arguments follow ([`call_arguments`]).
    let mut written_after = vec![false; fields.len()];
    let mut later = false;
    for index in (0..fields.len()).rev() {
        written_after[index] = later;
        later = marks[index] != Plicity::Explicit && (later || !restored[index]);
    }

    let mut entries = Vec::with_capacity(fields.len());
    for (index, field) in fields.into_iter().enumerate() {
        let entry = |label: Option<String>, value: Term| TupleField {
            label,
            func_params: None,
            value,
        };
        match (marks[index], restored[index], written_after[index]) {
            (Plicity::Witness, true, false) => {}
            (Plicity::Witness, true, true) => entries.push(StructLitEntry::Use(placeholder())),
            (Plicity::Witness, false, _) => {
                entries.push(StructLitEntry::Use(sub(field, frame)?));
            }
            (Plicity::Implicit, ..) => {
                let label = slot(index).and_then(|(_, label)| label.clone());

                entries.push(StructLitEntry::Implicit(entry(label, sub(field, frame)?)));
            }
            (Plicity::Explicit, ..) => {
                entries.push(StructLitEntry::Field(entry(None, sub(field, frame)?)));
            }
        }
    }

    // Where no expected type states the structure's parameters the head is applied to them, as a program pins them: `Gcd(a, b) { … }`.
    let params = match frame.inferred && !params.is_empty() {
        true => {
            let marks = frame.spelling.nominal_marks(&name, params.len());

            Some(nominal_arguments(params, marks, 0, frame)?)
        }
        false => None,
    };

    Some(
        Subterm::StructLit(StructLit {
            head,
            params,
            entries,
        })
        .into(),
    )
}

/// A certified string as the literal it stands for, `"body"` in place of the structure over its bytes and the proof certifying them. The outer `None` is a structure that is no such string: another declaration's, or a `Str` whose bytes are not one constant or are not text. The inner one is a string a report quotes a prefix of and counts the rest, which no literal spells.
fn string_literal(
    name: &curios_core::Global,
    fields: &[curios_core::Term],
    frame: Frame,
) -> Option<Option<Term>> {
    if frame.spelling.string_declaration() != Some(name) {
        return None;
    }
    let curios_core::Subterm::Intrinsic(curios_core::Intrinsic::Bin(Grain::X, packed)) =
        &**fields.first()?
    else {
        return None;
    };
    let text = String::from_utf8(packed.to_bytes()?).ok()?;
    if text.chars().count() > curios_core::ELIDED_LITERAL_CHARACTERS {
        return Some(None);
    }

    Some(Some(
        Subterm::ProofLiteral(ProofLiteral::Str(StrLit::line(text))).into(),
    ))
}

/// The word a binder a scope opened is written as: its name, or `_` where the compiler minted it with none and `body` never refers to it.
fn binder(label: &curios_core::Free, body: &curios_core::Term, frame: Frame) -> Label {
    match label.hint().is_some() || body.mentions_free(label) {
        true => Label::from(frame.word(label)),
        false => Label::from("_"),
    }
}

/// The `; ih` a fold's arm binds its hypothesis by, left out where the arm is a case split: the compiler minted the binder and `body` never reads it.
fn hypothesis(
    label: &curios_core::Free,
    body: &curios_core::Term,
    frame: Frame,
) -> Option<Pattern> {
    (label.hint().is_some() || body.mentions_free(label))
        .then(|| Pattern::Binder(Label::from(frame.word(label))))
}

/// An elimination as the `match` that lowers to it: each case as the pattern a program writes it by.
fn written_match(elimination: curios_core::Match, frame: Frame) -> Option<Term> {
    use curios_core::{Carrier, Cases, MatchResult};

    let curios_core::Match {
        head,
        result,
        cases,
    } = elimination;
    let open = |labels: &[curios_core::Free]| {
        labels
            .iter()
            .map(curios_core::Term::free_var)
            .collect::<Vec<_>>()
    };

    // A motive is the lambda a program writes after the scrutinee, binding its indices and then itself. It is written where the result depends on what it binds, and where the match stands with no expected type to take its result from: elsewhere one that names none of its binders states the type the match already has where it stands, as a goal taken from there does, and a program writes neither.
    let motive = match result {
        MatchResult::Family(motive) => {
            let labels = frame.mint.scope_labels(&motive);
            let terms = open(&labels);
            let body = motive.open(&terms.iter().collect::<Vec<_>>());
            let params = labels
                .iter()
                .map(|label| FuncParam {
                    plicity: Plicity::Explicit,
                    pattern: Pattern::Binder(binder(label, &body, frame)),
                    annotation: None,
                })
                .collect();

            match frame.inferred || labels.iter().any(|label| body.mentions_free(label)) {
                true => Some(
                    Subterm::Func(Func {
                        params,
                        body: sub(body, frame)?,
                    })
                    .into(),
                ),
                false => None,
            }
        }
        MatchResult::Ambient(_) => None,
    };
    let head = unchecked(head, frame)?;
    let arm = |pattern: MatchPattern, body: Term| MatrixArm {
        pattern,
        body,
        span: None,
    };

    let arms = match cases {
        Cases::Bool {
            false_case,
            true_case,
        } => vec![
            arm(MatchPattern::Bool(true), sub(true_case, frame)?),
            arm(MatchPattern::Bool(false), sub(false_case, frame)?),
        ],
        // A dispatch by value: a numeral an arm, and the fallthrough it is required to have.
        Cases::Switch { cases, default } => {
            let mut arms = Vec::with_capacity(cases.len() + 1);
            for (value, body) in cases {
                let pattern = match value.is_zero() {
                    true => NatPattern::Zero,
                    false => NatPattern::Lit(NatLiteral(value, Radix::Dec(0))),
                };
                arms.push(arm(MatchPattern::Nat(pattern), sub(body, frame)?));
            }
            arms.push(arm(
                MatchPattern::Binder(Label::from("_")),
                sub(default, frame)?,
            ));

            arms
        }
        Cases::Induct { cases, default, .. } => {
            let mut arms = Vec::with_capacity(cases.len());
            for (tag, case) in cases {
                let labels = frame.mint.arm_labels(&case);
                let terms = open(&labels);
                let body = case.open(&terms.iter().collect::<Vec<_>>());
                let args = labels
                    .iter()
                    .enumerate()
                    .map(|(slot, label)| {
                        let mark = case.plicities().get(slot).copied();

                        (
                            mark.unwrap_or(Plicity::Explicit),
                            MatchPattern::Binder(binder(label, &body, frame)),
                        )
                    })
                    .collect();
                arms.push(arm(
                    MatchPattern::Variant {
                        tag: tag.as_string(),
                        args,
                    },
                    sub(body, frame)?,
                ));
            }
            // The fallthrough a match may end in, standing for every constructor it does not name.
            if let Some(default) = default {
                arms.push(arm(
                    MatchPattern::Binder(Label::from("_")),
                    sub(default, frame)?,
                ));
            }

            arms
        }
        Cases::FreeMonoid { carrier } => match carrier {
            Carrier::Nat {
                empty_case,
                cons_case,
            } => {
                let labels = frame.mint.scope_labels(&cons_case);
                let terms = open(&labels);
                let ([predecessor, ih], [opened, hypothesized]) = (&labels[..], &terms[..]) else {
                    return None;
                };
                let body = cons_case.open(&[opened, hypothesized]);
                let succ = NatPattern::Succ {
                    pred_label: binder(predecessor, &body, frame),
                    ih: hypothesis(ih, &body, frame),
                };

                vec![
                    arm(MatchPattern::Nat(NatPattern::Zero), sub(empty_case, frame)?),
                    arm(MatchPattern::Nat(succ), sub(body, frame)?),
                ]
            }
            Carrier::Bin {
                grain,
                empty_case,
                cons_case,
            } => {
                let labels = frame.mint.scope_labels(&cons_case);
                let terms = open(&labels);
                let ([first, rest, ih], [peeled, remaining, hypothesized]) =
                    (&labels[..], &terms[..])
                else {
                    return None;
                };
                let body = cons_case.open(&[peeled, remaining, hypothesized]);
                let atom = BinPattern::Atom {
                    grain,
                    head_label: binder(first, &body, frame),
                    tail_label: binder(rest, &body, frame),
                    ih: hypothesis(ih, &body, frame),
                };

                vec![
                    arm(
                        MatchPattern::Bin(BinPattern::End(grain)),
                        sub(empty_case, frame)?,
                    ),
                    arm(MatchPattern::Bin(atom), sub(body, frame)?),
                ]
            }
            Carrier::List {
                empty_case,
                cons_case,
                ..
            } => {
                let labels = frame.mint.scope_labels(&cons_case);
                let terms = open(&labels);
                let ([first, rest, ih], [peeled, remaining, hypothesized]) =
                    (&labels[..], &terms[..])
                else {
                    return None;
                };
                let body = cons_case.open(&[peeled, remaining, hypothesized]);
                let cons = ListPattern::Cons {
                    head_label: binder(first, &body, frame),
                    tail_label: binder(rest, &body, frame),
                    ih: hypothesis(ih, &body, frame),
                };

                vec![
                    arm(
                        MatchPattern::List(ListPattern::Nil),
                        sub(empty_case, frame)?,
                    ),
                    arm(MatchPattern::List(cons), sub(body, frame)?),
                ]
            }
        },
    };

    Some(Subterm::Match(Match { head, motive, arms }).into())
}
