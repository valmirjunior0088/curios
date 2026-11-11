//! The Curios core language: the term representation both checkers build on.
//!
//! This crate holds **what a term is** and nothing that judges one. It defines [`Term`]/[`Subterm`] and its locally-nameless binder discipline ([`Scope`], [`Telescope`], [`Bound`]), the intrinsic roster ([`Intrinsic`]) with its free-monoid spine algebra and its folds ([`reduce_intrinsic`]), algebraic universe [`Level`]s and the [`UniverseContext`] declarations generalize into, the nominal registry entries ([`InductDecl`], [`StructDecl`], [`ConceptDecl`]) and the [`Polarity`] lattice they carry, the [`Size`] lattice and [`CallMatrix`] a certifier's record grades a definition's applications by, the finished-program shape both checkers walk ([`Definitions`], [`Program`], [`Item`], [`Definition`]), compiler names, and what a unit's names are spelled by ([`Lexicon`]).
//!
//! The judgments live on either side of it, as `README.md` states. `curios-analysis` holds the rules both checkers run and `curios-cert` the certifier that only one of them does — together, every rule that can admit a program — and `curios-elab` holds everything that makes the *surface language* work: elaboration, unification, zonking, witness resolution, erasure, and the universe *solver*. Both depend on this crate and never the reverse. A [`UniverseContext`] is data the elaborator's solver infers over and each checker decides the satisfiability of for itself, and [`Reducer`] is the seam between a fold's arithmetic and a checker's reduction strategy; both lines are `README.md`'s decisions.
//!
//! One module is not trusted, `builders` — the constructors the surface lowering and the elaborator both build terms with, such as [`str_literal`] and [`Term::num_lit`] — and why it lives here is `README.md`'s decision. It cannot admit a bad program: no judgment calls a builder. A term is written by `curios-surface`, and a crate below it writes one through the [`Notation`] its reporter hands it.
//!
//! The crate is a flat module space: every module re-exports at the root, so consumers use `curios_core::Term`, not paths into the modules.

mod scope;
use scope::*;
pub use scope::{
    Advance, ArchivedBinding, ArchivedLabel, ArchivedMany, ArchivedOne, ArchivedScope,
    ArchivedTelescope, ArchivedThree, ArchivedTwo, ArchivedVar, ArchivedWidth, Arity, Binding,
    Bound, Cursor, Label, Lockstep, Many, NodeMemo, One, ProofBinders, Scope, Sharing, Step,
    Telescope, Three, Two, Var, Visit, Width, instantiate_qualities, instantiate_universes,
    open_quality, project_erased_universes, rewrite_universe_levels, rewrite_universe_qualities,
    stamp_declaration_instance, substitute_universe_metas, universe_metas, universe_params,
};

mod concept;
pub use concept::{ArchivedConceptDecl, ConceptDecl, method_spelling};

mod items;
use items::*;
pub use items::{
    ArchivedItemlessModule, ArchivedOutlines, Arranged, Changes, ItemPlaces, ItemVector, Itemless,
    ItemlessModule, Items, Iter, Outlines, WithoutItems,
};

mod item_run;
pub use item_run::{ArchivedItemRun, ItemRun};

mod read_table;
pub use read_table::{ArchivedReadTable, ReadEntry, ReadTable};

mod declarations;
pub use declarations::Declarations;

mod definitions;
pub use definitions::{
    Abroad, ArchivedAbroad, ArchivedCertification, ArchivedCertified, ArchivedDeclared,
    ArchivedDefinition, ArchivedDefinitionKind, ArchivedDefinitions, ArchivedItem, ArchivedNominal,
    ArchivedParameterField, ArchivedRead, ArchivedReads, ArchivedRecDefinition, ArchivedRecItem,
    ArchivedTotality, Certification, Certified, Concluded, Declared, Definition, DefinitionKind,
    Definitions, Entrypoint, HeldReads, Item, Nominal, ParameterField, Positional, Program, Read,
    ReadBy, ReadRecord, Reads, RecDefinition, RecItem, Revision, Totality, Zonkable, Zonked,
    ZonkedRefusal, former_spelling, free_locals_of, open_positions_of, validate_stored_identities,
};

mod places;
pub use places::{Place, Places};

mod order;
pub use order::{
    ArchivedOrder, ArchivedOrderTable, ArchivedTabledReach, Order, OrderTable, OrderTabling, Reach,
    Revised, TabledReach,
};

mod atoms;
pub use atoms::same_atom;
use atoms::*;

mod linear;
pub use linear::{Aligned, LinearView, LinearViews};

mod nat;
pub use nat::{ArchivedNat, Nat};

mod int;
use int::*;
pub use int::{int_has_stuck_product, int_normalize, int_same, int_stuck_product};

mod lattice;
pub use lattice::Extremum;

mod universe;
pub use universe::{
    ArchivedLevel, ArchivedLevelHead, ArchivedQuality, ArchivedQualityMetaId, ArchivedQualityParam,
    ArchivedUniverseConstraint, ArchivedUniverseConstraintKind, ArchivedUniverseContext,
    ArchivedUniverseMetaId, ArchivedUniverseParam, ArchivedUniverseRole, ArchivedUniverseSeed,
    ArchivedUniverses, ArchivedWrittenSort, Level, LevelHead, Quality, QualityMetaId, QualityParam,
    UniverseConstraint, UniverseConstraintKind, UniverseConstraintOrigin, UniverseContext,
    UniverseError, UniverseMetaId, UniverseParam, UniverseRole, UniverseSeed, Universes,
    WrittenSort, inconsistent_sort, instantiated_on, names_a_proposition,
};

mod intrinsic;
pub use intrinsic::{
    ArchivedIntrinsic, Declaration, Intrinsic, IntrinsicHead, Operand, Operands, Produced,
    Signature, foreign_operands, foreign_produced, foreign_signature, wire_term,
};

mod spine;
use spine::*;
pub use spine::{
    peel_commutative, peel_comparison, peel_intrinsic, peel_lattice_leaf, peel_monomial, peel_spine,
};

mod words;
use words::*;

mod free_monoid;
use free_monoid::*;
pub use free_monoid::{FreeMonoid, Layer};

mod cost;
pub use cost::{Category, Cost};

mod consumption;
pub use consumption::Consumption;

mod reduce;
use reduce::*;
pub use reduce::{
    Exhaustion, Probe, ProbeSpellings, ReduceError, Reducer, align_comparisons, bool_value,
    decide_bool, eta_head, is_bool_connective, normalize_bool, probe_spellings, project_literal,
    reduce_intrinsic,
};

mod machine;
pub use machine::{
    AsideAnswers, ClosedHost, ClosedMemo, Demand, accelerable, lasting, reduce_closed,
    weak_head_value,
};

mod names;
pub use names::{
    ArchivedAtom, ArchivedCalleeId, ArchivedFree, ArchivedGlobal, ArchivedImport, ArchivedImports,
    ArchivedImportsEntry, ArchivedInScope, ArchivedMention, ArchivedMint, ArchivedMinted,
    ArchivedMints, ArchivedStoredMints, Atom, ByDeclarationProxy, CalleeId, Faithful, Free, Global,
    Import, Imported, Imports, ImportsEntry, InScope, Mention, Mint, Minted, Mints, ModuleExport,
    Paths, Spellings, StoredMints, UnitPaths, WritablePath,
};

mod unresolved;
pub use unresolved::{
    ArchivedMissing, ArchivedSought, ArchivedUnresolved, Missing, Sought, Unresolved,
};

mod extent;
pub use extent::{
    Anchor, Anchored, Anchors, ArchivedAnchor, ArchivedAnchored, ArchivedAnchors, ArchivedExtent,
    ArchivedSourceTexts, Extent, InterningAnchor, SourceTexts, Within,
};

mod witness;
pub use witness::{
    ArchivedHeadKey, ArchivedWitnessId, ArchivedWitnessKey, HeadKey, InterningHeads, Keying,
    WitnessId, WitnessKey, Written,
};

mod trace;
pub use trace::{ArchivedTrace, Trace};

mod term;
#[cfg(feature = "profile")]
pub use term::take_looks;
use term::*;
pub use term::{
    Apply, ArchivedApply, ArchivedArgument, ArchivedArm, ArchivedAscribed, ArchivedBang,
    ArchivedBind, ArchivedCarrier, ArchivedCases, ArchivedField, ArchivedForeign, ArchivedFunc,
    ArchivedFuncType, ArchivedImplicitOrigin, ArchivedInductArm, ArchivedInductType, ArchivedInfix,
    ArchivedInstance, ArchivedLet, ArchivedLetBinding, ArchivedMatch, ArchivedMatchResult,
    ArchivedMatrix, ArchivedMetavar, ArchivedMetavarId, ArchivedMetavarOrigin, ArchivedMethod,
    ArchivedNumLit, ArchivedPattern, ArchivedProj, ArchivedRec, ArchivedRecGroup,
    ArchivedRecMemberScopes, ArchivedRow, ArchivedSort, ArchivedStruct, ArchivedStructEntry,
    ArchivedStructType, ArchivedSubterm, ArchivedTerm, ArchivedTransient, ArchivedTuple,
    ArchivedTupleType, ArchivedVariant, ArchivedWitnessOrigin, Argument, Arm, Ascribed, Bang, Bind,
    Carrier, Cases, Field, Foreign, FormerEta, Func, FuncType, HeadTag, ImplicitOrigin, InductArm,
    InductType, Infix, Instance, Let, LetBinding, LevelDifferences, Match, MatchResult, Matrix,
    MentionCache, Mentions, MentionsIter, MetaRenaming, Metavar, MetavarId, MetavarOrigin,
    MetavarSpine, Method, Names, NumLit, Pattern, Proj, Ranked, Rec, RecGroup, RecMemberScopes,
    Row, ScalarCache, Scalars, Sort, SpineEntries, Struct, StructEntry, StructType, Subterm, Term,
    Transient, Tuple, TupleType, Variant, WitnessOrigin, former_eta,
};

mod walk;
pub use walk::{ChildResults, Enter};

mod inductive;
pub use inductive::{
    ArchivedConstructors, ArchivedInductDecl, ArchivedInductParam, Constructors, InductDecl,
    InductInstance, InductParam,
};

mod structure;
pub use structure::{ArchivedStructDecl, StructDecl, StructInstance};

mod polarity;
pub use polarity::{ArchivedPolarity, Polarity};

mod call_matrix;
pub use call_matrix::{
    Applications, ArchivedApplications, ArchivedCallMatrix, ArchivedSize, CallMatrix, Size,
};

mod variance;
pub use variance::{ArchivedVariance, Families, Variance};

mod lexicon;
pub use lexicon::{Lexicon, Marks, Members, Symbols, suffixes_of};

mod notation;
pub use notation::Notation;

mod builders;
pub use builders::{str_literal, syn_call};

#[cfg(any(test, feature = "test-support"))]
pub mod test_support;
