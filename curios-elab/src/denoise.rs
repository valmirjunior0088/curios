//! Display-only folding of elaboration internals back into source-shaped spelling.
//!
//! An infix operator elaborates to a projection of its operator concept witness applied to the operands (`a + b` ≙ `Add/add(a, b)` — see `elaborate_infix`), so a goal report would spell `0 + 0` as `(witness2).0(0, 0)`: an anonymous witness projection no reader should have to decode. The fold reverses exactly that rebuild, recognizing the three forms the call reaches a report in — a still-unsolved witness metavariable, whose `WitnessOrigin` carries the operator symbol; a solved-and-substituted witness global, resolved back to its operator through the witness table and the concept's field roster; and an *abstract* witness, a `use` binder standing for a witness the caller will supply, resolved through the concept its declared type names.
//!
//! The abstract case is the one reduction cannot reach. Where the operand type is concrete the projection reduces to its intrinsic and prints infix without any of this, so the fold only earns its keep under a `use Add(A)` parameter — exactly where the reader has least else to go on.
//!
//! A method no operator dispatches through folds the same way, to the wrapper call a program writes: a bound a divisor owes reads `Div/Ok(n)` where the report would otherwise spell `(witness5).0(n)`. An unsolved witness carries only an operator's symbol as provenance, so it folds for operators alone.
//!
//! The folded term reintroduces the elaboration-transient `Infix` node (under [`Subterm::Transient`]) purely for observation: it only ever meets the printer, never checking, reduction, or erasure.
//!
//! The second fold here is over recursion. A mismatch report normalizes both sides so the disagreement is visible, and normalizing `double(p + 1)` unfolds the global through its structural group, which carries no name: the report then spelled `rec #0: (n: Nat) -> Nat = n => …; #0(p) + 2` where the author wrote `double`. [`refold_recs`] recognizes a `Rec` node whose group is one the context defined — compared projected, so an instance of a universe-polymorphic group is the group it instantiates — and spells its tail with each member as the definition it is: `double(p) + 2`. This lives here rather than in the printer because the group to recognize is the *elaborated* one, which only the context holds; the lowered module the formatter has is a different structure, and on an error path there is no elaborated module at all.

#[cfg(test)]
mod tests;

use {
    super::{Context, TermBuilders},
    curios_core::{
        Apply, Bound, Field, Free, Global, Instance, InstanceHead, Metavar, MetavarOrigin, Proj,
        Rec, RecGroup, StructType, Subterm, Term, Var, Visit,
    },
    curios_utilities::InfixOp,
    std::{collections::BTreeMap, rc::Rc},
};

/// The concept method a witness projection stands for.
#[derive(Debug, Clone)]
pub(crate) struct Method {
    /// The generated wrapper a program calls the method through, `Concept/field` — what a report spells the projection as. Absent for an unsolved witness, whose provenance names an operator and nothing else.
    wrapper: Option<Global>,
    /// The operator dispatching through this method, which a binary call folds back to instead.
    operator: Option<InfixOp>,
}

/// The method behind a witness projection, keyed both ways a report can arrive at one. Built once and shared across a batch's display calls; the metavariable-origin case needs no entry because the insertion provenance itself names the operator.
#[derive(Debug, Default)]
pub(crate) struct MethodTable {
    /// (witness name, projected field index) → method, for every registered witness.
    by_witness: BTreeMap<(Global, usize), Method>,
    /// (concept name, field index) → method, for every concept. Keyed on the concept rather than on any witness of it, because an abstract witness has none: the binder's type names the concept and the projection's index picks the method out of it.
    by_concept: BTreeMap<(Global, usize), Method>,
}

pub(crate) type Methods = Rc<MethodTable>;

/// Declared types of the binders visible at the display site, so an abstract witness resolves to its concept. The goal path supplies the goal's birth telescope, the mismatch path the live local context; a binder missing from the map simply declines to fold.
pub(crate) type BinderTypes = Rc<BTreeMap<Free, Term>>;

pub(crate) fn method_table(context: &Context) -> Methods {
    let mut table = MethodTable::default();

    // The concept rows come first and carry the whole method lookup: a witness of a concept answers for the same method at the same index, so the witness rows are a re-keying of these rather than a second scan of the fields.
    for (concept_name, concept) in context.concepts() {
        let Global::Authored(concept_path) = concept_name else {
            continue;
        };
        for (index, field) in concept.fields.iter().enumerate() {
            // A superclass edge is an anonymous field no wrapper is generated for: projecting it is resolution's own step, not a call anyone wrote.
            if concept
                .supers
                .iter()
                .any(|(position, _)| *position == index)
            {
                continue;
            }
            let method = Method {
                wrapper: Some(Global::Authored(concept_path.with(field))),
                operator: context.syntax().operator.operator_for(concept_path, field),
            };
            table
                .by_concept
                .insert((concept_name.clone(), index), method);
        }
    }

    for (concept_name, witness) in context.witness_entries() {
        let Some(concept) = context.concept(concept_name) else {
            continue;
        };
        for index in 0..concept.fields.len() {
            if let Some(method) = table.by_concept.get(&(concept_name.clone(), index)) {
                table
                    .by_witness
                    .insert((witness.name.clone(), index), method.clone());
            }
        }
    }

    Rc::new(table)
}

/// Fold every concept-method witness projection in `term` back to its source spelling: an operator's to infix, any other method's to its wrapper call. Display-only — see the module documentation. Runs after tolerant materialization, so solved witnesses arrive as globals, unsolved ones as origin-marked metavariables, and abstract ones as binders; all three fold.
pub(crate) fn denoise_for_display(table: &Methods, binders: &BinderTypes, term: &Term) -> Term {
    let captured = Rc::clone(table);
    let scope = Rc::clone(binders);
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term| fold(&captured, &scope, term)),
    );
    term.traverse(&mut visit)
}

/// Spell every unfolded top-level `rec` in `term` by its definitions' names. Display-only — see the module documentation. The table of groups is gathered only when the term holds a `Rec` node at all, which a mismatch rarely does.
pub(crate) fn refold_recs(context: &Context, term: &Term) -> Term {
    if !term.mentions_rec() {
        return term.clone();
    }
    let table: Rc<Vec<(RecGroup, Vec<Global>)>> = Rc::new(
        context
            .rec_definitions()
            .into_iter()
            .map(|(group, names)| (group.projected(), names))
            .collect(),
    );
    refold_with(&table, term)
}

fn refold_with(table: &Rc<Vec<(RecGroup, Vec<Global>)>>, term: &Term) -> Term {
    let captured = Rc::clone(table);
    let mut visit = Visit::rewriting(
        |_, _| None,
        Box::new(move |_, term| refold_node(&captured, term)),
    );
    term.traverse(&mut visit)
}

/// The node-level refold: a `Rec` whose group the table knows opens its tail over the members' names, and the opened tail is refolded in turn, since a substituted node is not descended into.
fn refold_node(table: &Rc<Vec<(RecGroup, Vec<Global>)>>, term: &Term) -> Option<Term> {
    let Subterm::Rec(Rec { group, tail }) = &**term else {
        return None;
    };
    let projected = group.projected();
    let (_, names) = table.iter().find(|(known, _)| *known == projected)?;
    if names.len() != group.length() {
        return None;
    }
    let members = names
        .iter()
        .map(|name| Term::var(Var::free(Free::Global(name.clone()))))
        .collect::<Vec<_>>();
    let refs = members.iter().collect::<Vec<_>>();
    Some(refold_with(table, &tail.open(&refs)))
}

/// The node-level fold. A substituted node is not descended into, so a folded call denoises its own arguments before wrapping them.
///
/// A binary call to an operator's method reads back as the operator; any other call, or a bare projection, reads as the method's wrapper. `!=` needs no special case: `Neq` has its own concept slot, so [`OperatorSyntax::operator_for`](curios_utilities::OperatorSyntax::operator_for) keys the `neq` projection exactly.
fn fold(table: &Methods, binders: &BinderTypes, term: &Term) -> Option<Term> {
    let (head, arguments) = match &**term {
        Subterm::Apply(Apply { head, arguments }) => (head, arguments.as_slice()),
        _ => (term, &[][..]),
    };
    let method = projected_method(table, binders, head)?;
    let denoise = |argument: &Term| denoise_for_display(table, binders, argument);

    if let (Some(op), [left, right]) = (method.operator, arguments) {
        return Some(Term::infix(op, denoise(&left.term), denoise(&right.term)));
    }

    let wrapper = Term::var(Var::free(Free::Global(method.wrapper?)));
    Some(match &**term {
        Subterm::Apply(_) => Term::apply_marked(
            wrapper,
            arguments
                .iter()
                .map(|argument| (argument.plicity, denoise(&argument.term))),
        ),
        _ => wrapper,
    })
}

/// The method a `Proj(witness, index)` stands for, whichever of the three forms the witness arrives in.
fn projected_method(table: &Methods, binders: &BinderTypes, head: &Term) -> Option<Method> {
    let Subterm::Proj(Proj {
        head: witness,
        field: Field::Index(index),
    }) = &**head
    else {
        return None;
    };

    // A solved witness reference may carry its occurrence's universe instance; the identity lives in the head, which the or-pattern reads through.
    match &**witness {
        // Unsolved: the insertion provenance rides the metavariable, and it names an operator or nothing.
        Subterm::Metavar(Metavar {
            origin: MetavarOrigin::Witness(origin),
            ..
        }) => Some(Method {
            wrapper: None,
            operator: Some(InfixOp::from_symbol(&origin.func)?),
        }),
        Subterm::Var(var)
        | Subterm::Instance(Instance {
            head: InstanceHead::Var(var),
            ..
        }) => match var.as_free()? {
            // Solved and substituted: the witness global names its method through the precomputed table.
            Free::Global(global) => table.by_witness.get(&(global.clone(), *index)).cloned(),
            // Abstract: a `use` binder standing for a witness the caller supplies. It has no registered name to key on, so its declared type — a concept application — names the concept instead.
            name @ Free::Local(_) => table
                .by_concept
                .get(&(concept_of(binders.get(name)?)?, *index))
                .cloned(),
        },
        _ => None,
    }
}

/// The concept a witness binder's declared type names, in either spelling a report can reach it in: unreduced — the declaration's global applied to the concept parameters, carrying the occurrence's universe instance — or reduced to the record the concept denotes. A binder's declared type is whatever elaboration assumed it as, and a `use` parameter is never forced on the way to a diagnostic, so the unreduced form is the common one. Anything else is not a witness binder and declines.
fn concept_of(type_: &Term) -> Option<Global> {
    let head = match &**type_ {
        Subterm::Apply(Apply { head, .. }) => head,
        _ => type_,
    };
    match &**head {
        Subterm::StructType(StructType { name, .. }) => Some(name.clone()),
        Subterm::Var(var)
        | Subterm::Instance(Instance {
            head: InstanceHead::Var(var),
            ..
        }) => match var.as_free()? {
            Free::Global(global) => Some(global.clone()),
            Free::Local(_) => None,
        },
        _ => None,
    }
}
