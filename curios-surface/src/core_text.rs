//! Core text: Core written in the surface grammar with nothing left to an elaborator, and one decorator.
//!
//! **A form builds a node, its template, and a decorator sets what differs.** Each surface form a node is written in builds one node with every field it does not spell at a default stated once, for that kind of node, in its module here; `#{field = value, …}` before the form sets each field the node holds otherwise. A label is the field's Core name, a struct-valued field is written nested as a struct literal whose omitted fields keep the template's, and an enum-valued field takes its variant's Core name. One label is no field: `node` names the node a form fills where it is other than the one the form builds, as a constructor's value is written as the call of the constructor's definition under `#{node = Variant}`. So the shortest text is the canonical one, and a node prints with a decorator exactly where it is not what its form builds.
//!
//! **A decorator attaches to the next whole operand.** A primary — a name, a literal, a call — takes it as it stands; a larger form is parenthesized under it, and so is a decorated call's head, so `(#{universes = {levels = [0]}} /f)(@Nat, 2)` sets the instance of the name and not of the call.
//!
//! **Every name is written as the spelling handed in spells it.** Core text proper — a dump, a kernel's refusal, the round-trip — is written under a spelling that says so ([`Spelling::core_text`](crate::Spelling::core_text)), every global by its absolute path or by the shortening that spelling sets.
//!
//! **A reader's spelling writes the same forms as a reader writes them.** Under any other spelling — a diagnostic's, and every term a reader is shown — no decorator is written, an intrinsic is its surface name or operator (`Nat`, `2 + 3`), a sort's level and a name's instance follow the name as `.{…}`, a level metavariable is `?u1`, a type former's η-form is the former, a constructor's value and a record literal leave their family's parameters out, and a metavariable is `?7`. That spelling is for a reader, who cannot write most of what a decorator states, and reads back as no term.
//!
//! **What Core's equality does not read is not written.** A term's position and a binder's place among the written ones are left out, and a binder is written by its hint, renamed where it would capture or has none: two terms printed alike are one term to every judgment.
//!
//! **Each node kind's form and template live in a module of their own, its reader beside its writer.** A sort and the levels it is at, an intrinsic, a function, a tuple, a nominal family or record, a match, a binding, a name, the forms only elaboration builds, and a nominal declaration's form. A reader builds what its form builds and sets what the decorator states, refusing a field its node does not hold; [`to_core`] reads a file's items, making the items a form implies where a reader puts them. A writer builds the shown tree its form is ([`Tree`]), which the program's printer lays out as it lays out a program.

#[cfg(test)]
mod tests;

mod frame;
pub use frame::*;

mod read;
pub use read::*;

mod item;
pub use item::*;

mod binding;
use binding::*;

mod declaration;
pub use declaration::*;

mod family;
use family::*;

mod function;
use function::*;

mod intrinsic;
use intrinsic::*;

mod matching;
use matching::*;

mod name;
use name::*;

mod sort;
use sort::*;
pub use sort::{level_spelled, universe_context_written};

mod transient;
use transient::*;

mod tuple;
use tuple::*;

use curios_core::{Subterm, Term};

/// `term` as the shown tree a print lays out under `frame`, with no bound on its descent: [`sub`] is the guarded door every child goes through.
pub fn core_written(term: Term, frame: Frame) -> Tree {
    match Term::unwrap_or_clone(term) {
        Subterm::Sort(sort) => sort_written(sort, frame),
        Subterm::Intrinsic(intrinsic) => intrinsic_written(intrinsic, frame),
        Subterm::Foreign(foreign) => foreign_written(foreign, frame),
        Subterm::FuncType(function) => function_type_written(function, frame),
        Subterm::Func(function) => function_written(function, frame),
        Subterm::Apply(apply) => apply_written(apply, frame),
        Subterm::TupleType(tuple) => tuple_type_written(tuple, frame),
        Subterm::Tuple(tuple) => tuple_written(tuple, frame),
        Subterm::Proj(projection) => projection_written(projection, frame),
        Subterm::InductType(family) => family_written(family, frame),
        Subterm::Variant(variant) => variant_written(variant, frame),
        Subterm::StructType(record) => record_written(record, frame),
        Subterm::Struct(literal) => literal_written(literal, frame),
        Subterm::Match(elimination) => match_written(elimination, frame),
        Subterm::Let(binding) => let_written(binding, frame),
        Subterm::Rec(group) => rec_written(group, frame),
        Subterm::Instance(instance) => instance_written(instance, frame),
        Subterm::Var(var) => var_written(var, frame),
        Subterm::Metavar(metavar) => metavar_written(metavar, frame),
        Subterm::Transient(transient) => transient_written(transient, frame),
    }
}
