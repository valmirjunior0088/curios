//! The written form of the `.crs` language: the tree a file is read into, the parser that reads it, the printer that writes it back as written, the formatter behind `curios format`, the translation of a Core term back to the tree, which a report prints through, and the writer of Core text with the naming policy every term is spelled by.
//!
//! [`Term`]s keep sugar as written, which `README.md` decides; `curios-module`'s lowering undoes it.

mod names;
pub use names::{ArchivedLabel, ArchivedName, Label, Name};

mod nat;
pub use nat::{NatLiteral, Radix};

mod intrinsic;
pub use intrinsic::{BytesSegment, Intrinsic, ListEntry, Literal};

mod dialect;
use dialect::*;
pub use dialect::{
    CoreEntry, CoreFile, CoreForm, CoreHead, CoreIntrinsic, CoreItem, CoreMember, CoreText,
    CoreValue, Decorated, DecoratorField, Dialect, Program, Unwritable,
};

mod term;
use term::*;
pub use term::{
    Alternative, Apply, Argument, BytesPattern, Choose, ChooseArm, ChooseTest, Field, Func,
    FuncParam, FuncSugarParam, FuncType, FuncTypeParam, Infix, Let, LetBinding, LetGroup,
    LetSignature, ListPattern, Match, MatchPattern, MatchPatternField, MatrixArm, NatPattern,
    NumLit, Pattern, PatternField, Proj, ProofLiteral, StrLit, StructLit, StructLitEntry, Subterm,
    Term, Tuple, TupleField, TupleType, TupleTypeParam, func_sugar_params, func_sugar_type_params,
};

mod spell;
pub use spell::spell;

mod from_core;
pub use from_core::written_pattern;
use from_core::*;

mod spelling;
use spelling::*;
pub use spelling::{
    DisplayNames, FormScope, IntrinsicForm, IntrinsicOperand, Islands, LiteralEntry, ReaderNames,
    ReaderPosition, Rename, Shorten, Spellable, Spelled, Spelling, build_rename, display_names,
    intrinsic_form, intrinsic_former, intrinsic_global,
};

mod core_text;
use core_text::*;
pub use core_text::{ReadError, Refusal, to_core};

mod format;
pub use format::{Formatted, NARROWEST, Width};

mod print;
use print::*;
pub use print::{
    print_case_head, print_case_result_head, print_concept_field_head, print_concept_head,
    print_foreign_head, print_induct_head, print_let_head, print_struct_field_head,
    print_struct_head, print_test_head, print_witness_head,
};

mod parse;
use parse::*;

mod module;
use module::*;
pub use module::{
    ArchivedGroupItem, ArchivedTopUse, ArchivedUseGroup, CasePayloadParam, ConceptField, Doc,
    Entrypoint, Form, GroupItem, Module, StructField, TopBroken, TopCase, TopConcept, TopForeign,
    TopInduct, TopItem, TopLet, TopMod, TopStruct, TopTest, TopUse, TopWitness, UseGroup,
    WitnessField,
};

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_surface::test_support` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;
