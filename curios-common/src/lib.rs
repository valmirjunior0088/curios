//! Foundational utilities shared across every Curios pipeline stage: source spans, the fresh-name `Entropy`/`Mint` supply, the `name!` and `id!` newtype macros, the typed identity-addressed [`Arena`], the [`Persistent`], [`Ordered`] and [`Sorted`] maps a state extended a step at a time is kept in, the SHA-256 content [`digest()`] and the [`Fingerprint`] every cabinet key, record and tree hash is spelled with, the resolved-module-path `Qualifier` identity, the language's words ([`Keyword`]) and the classes code is painted in ([`Class`]), the value types the surface (`curios-module`) and core (`curios-core`) `Term` representations share verbatim (`Plicity`, `InfixOp`), and the [`SyntaxRegistry`] shape those two stages read their emitted vocabulary from. Compiler-known names themselves belong to `curios-module`, alongside the source declarations they name: this crate states the slots, never the spellings.
//!
//! Why names are never ordered, why every identity space has one source, why qualifiers and symbols are interned once per process, why a segment's legality is decided here, why a mount is a prefix, why the syntax registry states slots and never spellings, and why depth is bought with stack are `README.md`'s decisions.
//!
//! Two neighbours hold the rest of the shared foundations. The numeric half of the shared vocabulary — `Natural`, `Integer`, `Flt`, and the erased carriers' scalar semantics — is `curios-num`, the one crate that names `num-bigint`. The two combinator DSLs are `curios-parse` and `curios-print`, two crates as `curios-print/README.md` decides.

mod macros;

mod arena;
pub use arena::{ArchivedArena, Arena, ArenaId};

mod persistent;
pub use persistent::{
    InKeyOrder, InOrder, InSharedKeyOrder, KeyOrder, Ordered, Persistent, SharedKeyOrder, Sorted,
    Summary,
};

mod entropy;
pub use entropy::{Entropy, Mint};

mod span;
pub use span::{
    About, ArchivedNote, ArchivedReport, ArchivedSource, ArchivedSpan, Code, MAX_LINE_WIDTH, Note,
    OsError, Pane, Report, Said, Source, Span,
};

mod source_name;
pub use source_name::{ArchivedSourceName, STDIN_NAME, SourceName};

mod style;
pub use style::{Kind, Role, Style, runs};

mod interner;
pub use interner::{Table, intern};

mod qualifier;
pub use qualifier::{
    ArchivedQualifier, Interning, Qualifier, is_identifier, is_identifier_char, is_identifier_start,
};

mod lineage;
pub use lineage::{ArchivedLineage, Lineage};

mod keyword;
pub use keyword::{Keyword, Standing, is_reserved};

mod class;
pub use class::{ArchivedClass, Class, ClassResolver};

mod symbol;
pub use symbol::{ArchivedSymbol, InterningText, Symbol};

mod plicity;
pub use plicity::{ArchivedPlicity, Plicity};

mod sign;
pub use sign::{ArchivedSign, Sign};

mod staged;
pub use staged::{write_whole, write_whole_prepared};

mod mount;
pub use mount::{ArchivedMount, ArchivedRootKind, Mount, RootKind};

mod infix_op;
pub use infix_op::{ArchivedInfixOp, InfixOp, InfixOpResolver};

mod syntax;
pub use syntax::{
    ChannelSyntax, CharacterSyntax, ConceptField, Derivation, DerivationSyntax, EntailmentSyntax,
    EqlDerivation, HashDerivation, IntegerSyntax, LiftSyntax, MonadSyntax, NaturalSyntax,
    OperatorSyntax, OptionSyntax, OrdDerivation, OrderSyntax, ProgramSyntax, ProofSyntax,
    ReportSyntax, ResultSyntax, SpellDerivation, StringSyntax, SyntaxName, SyntaxRegistry,
    TestSyntax,
};

mod recurse;
pub use recurse::{grown, recurse};

mod digest;
pub use digest::{Digest, Fingerprint, Member, Object, digest};

mod recorded;
pub use recorded::{Filed, Recorded, Records};

mod candidates;
pub use candidates::{Candidates, Facet, candidate_name, candidate_prefix};

mod nearest;
pub use nearest::nearest;

mod keyed;
pub use keyed::Keyed;

mod variants;
pub use variants::Variants;

// A namespace rather than a root export, for `curios-native`'s `test_support` reason: `curios_common::test_support::Temporary` says at its use site that the caller reached for scaffolding rather than product API.
#[cfg(feature = "test-support")]
pub mod test_support;
