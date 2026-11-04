//! The written form of the `.crs` language: the tree a file is read into, the parser that reads it, the printer that writes it back as written, the formatter behind `curios format`, the translation of a Core term back to the tree, which a report prints through, and the printer of Core text with the naming policy every term is spelled by.
//!
//! [`Term`]s keep sugar as written, which `README.md` decides; `curios-module`'s lowering undoes it.

mod names;
pub use names::*;

mod nat;
pub use nat::*;

mod intrinsic;
pub use intrinsic::*;

mod term;
pub use term::*;

mod spell;
pub use spell::*;

mod from_core;
pub use from_core::*;

mod spelling;
pub use spelling::*;

mod core_text;
use core_text::*;

mod format;
pub use format::*;

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
pub use module::*;
