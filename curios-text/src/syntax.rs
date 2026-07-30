//! Explicit lowering targets supplied by the prelude owner.

use curios_base::Qualifier;

/// One compiler-known `/syn` name, stated as its module segments.
///
/// Segments rather than a path string, because lowering needs the *identity*, and building one from `"/syn/Monad/bind"` would mean splitting a spelling — the coupling `curios-elab`'s name vocabulary exists to remove. The registry is the site that knows the structure, so the registry states it.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxName {
    segments: &'static [&'static str],
}

impl SyntaxName {
    pub const fn new(segments: &'static [&'static str]) -> Self {
        Self { segments }
    }

    /// The resolved identity this name denotes — what a lowered `Var` carries.
    pub fn qualifier(self) -> Qualifier {
        Qualifier::from(self.segments.iter().copied())
    }

    /// The flattened spelling, for the nominal registries `curios-elab` still keys by `String`, and for diagnostics. Rendering, not parsing: it goes out and never back in. Retired with those keys.
    pub fn symbol(self) -> String {
        self.qualifier().join()
    }

    /// The final segment — the declaration's own name.
    pub fn last(self) -> &'static str {
        self.segments.last().copied().unwrap_or_default()
    }
}

/// The compiler-known `/syn` names used by surface desugaring.
///
/// Fields are private so `curios-text` owns the shape of the lowering contract, while the crate that owns the corresponding source declarations chooses the canonical value.
#[derive(Debug, Clone, Copy)]
pub struct SyntaxRegistry {
    monad: MonadSyntax,
    character: CharacterSyntax,
    string: StringSyntax,
    proof: ProofSyntax,
}

impl SyntaxRegistry {
    pub const fn new(
        monad: MonadSyntax,
        character: CharacterSyntax,
        string: StringSyntax,
        proof: ProofSyntax,
    ) -> Self {
        Self {
            monad,
            character,
            string,
            proof,
        }
    }

    pub const fn monad(self) -> MonadSyntax {
        self.monad
    }

    pub const fn character(self) -> CharacterSyntax {
        self.character
    }

    pub const fn string(self) -> StringSyntax {
        self.string
    }

    pub const fn proof(self) -> ProofSyntax {
        self.proof
    }

    pub fn targets(self) -> impl Iterator<Item = SyntaxName> {
        [
            self.monad.bind,
            self.character.character,
            self.character.scalar_below,
            self.character.scalar_above,
            self.string.string,
            self.string.of_scan_eq,
            self.string.refl_scan,
            self.proof.true_qed,
            self.proof.false_absurd,
        ]
        .into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MonadSyntax {
    bind: SyntaxName,
}

impl MonadSyntax {
    pub const fn new(bind: SyntaxName) -> Self {
        Self { bind }
    }

    pub const fn bind(self) -> SyntaxName {
        self.bind
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CharacterSyntax {
    character: SyntaxName,
    scalar_below: SyntaxName,
    scalar_above: SyntaxName,
}

impl CharacterSyntax {
    pub const fn new(
        character: SyntaxName,
        scalar_below: SyntaxName,
        scalar_above: SyntaxName,
    ) -> Self {
        Self {
            character,
            scalar_below,
            scalar_above,
        }
    }

    pub const fn character(self) -> SyntaxName {
        self.character
    }

    pub const fn scalar_below(self) -> SyntaxName {
        self.scalar_below
    }

    pub const fn scalar_above(self) -> SyntaxName {
        self.scalar_above
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringSyntax {
    string: SyntaxName,
    of_scan_eq: SyntaxName,
    refl_scan: SyntaxName,
}

impl StringSyntax {
    pub const fn new(string: SyntaxName, of_scan_eq: SyntaxName, refl_scan: SyntaxName) -> Self {
        Self {
            string,
            of_scan_eq,
            refl_scan,
        }
    }

    pub const fn of_scan_eq(self) -> SyntaxName {
        self.of_scan_eq
    }

    pub const fn refl_scan(self) -> SyntaxName {
        self.refl_scan
    }

    pub const fn string(self) -> SyntaxName {
        self.string
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ProofSyntax {
    true_qed: SyntaxName,
    false_absurd: SyntaxName,
}

impl ProofSyntax {
    pub const fn new(true_qed: SyntaxName, false_absurd: SyntaxName) -> Self {
        Self {
            true_qed,
            false_absurd,
        }
    }

    pub const fn true_qed(self) -> SyntaxName {
        self.true_qed
    }

    pub const fn false_absurd(self) -> SyntaxName {
        self.false_absurd
    }
}
