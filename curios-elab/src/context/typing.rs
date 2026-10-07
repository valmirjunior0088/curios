//! What any judgment reads, and the machine it runs on.
//!
//! A judgment — the type of a term, its reduct, whether two terms convert — reads the declarations in scope and the binders it stands under, mints the binders it opens, and spends a budget. That is everything a judgment over finished terms needs, and it is what [`Typing`] holds: the binder supply, the budget, the lexical frames, the program's registries, the memo tables over what was derived from them, the protocol by which reduction asks conversion, and the names the type-directed features emit. Nothing here says that a unit is being elaborated; what does is `Underway`'s, and a context that only reads finished terms holds that half empty.

use {
    super::{Asking, Caches, Frames, Program, Spend},
    curios_utilities::{Entropy, SyntaxRegistry},
};

/// The half of a context every judgment reads.
#[derive(Debug)]
pub(crate) struct Typing {
    // The binder supply: a binder a judgment opens is minted here, above what the declaration's lowering minted. Exchanged with the attempt it mints for (`Context::exchange`), so the identities a declaration mints are the ones it would mint elaborated alone.
    pub(super) fresh_names: Entropy,
    // What a judgment may spend, restored at every declaration boundary, and what has been drawn from it; see [`Spend`].
    pub(super) spend: Spend,
    // The reduction and elaboration memo tables with their write stamps and the named invalidation protocol every mutation site routes through; see [`Caches`].
    pub(super) caches: Caches,
    // The frame-scoped lexical stores — assumptions, local definitions, refinements, the witness scope; see [`Frames`].
    pub(super) frames: Frames,
    // The program-wide declaration registries, witness table, and totality verdicts — flat stores of monotonic facts about the program, not lexically-scoped bindings; `enter_frame`/`leave_frame` never touch them. See [`Program`].
    pub(super) program: Program,
    // The protocol by which a judgment's reduction asks conversion; see [`Asking`].
    pub(super) asking: Asking,
    // The names the type-directed features synthesize — infix dispatch and row subsumption. Supplied rather than spelled: the elaborator knows *which* declaration it needs, and `curios-prelude` knows what that declaration is called. See `Context::syntax`.
    pub(super) syntax: SyntaxRegistry,
}

impl Typing {
    /// A machine with nothing in scope, in which each declaration may spend `budget` units, synthesizing the names `syntax` registers.
    pub(super) fn new(budget: u64, syntax: SyntaxRegistry) -> Self {
        Self {
            fresh_names: Entropy::<usize>::new(),
            spend: Spend::new(budget),
            caches: Caches::new(),
            frames: Frames::new(),
            program: Program::new(),
            asking: Asking::default(),
            syntax,
        }
    }
}
