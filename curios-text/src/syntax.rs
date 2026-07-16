//! Explicit lowering targets supplied by the prelude owner.

/// The compiler-known `/syn` names used by surface desugaring.
///
/// Fields are private so `curios-text` owns the shape of the lowering contract,
/// while the crate that owns the corresponding source declarations chooses the
/// canonical value.
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

    pub fn targets(self) -> impl Iterator<Item = &'static str> {
        [
            self.monad.bind,
            self.character.character,
            self.character.scalar_below,
            self.character.scalar_above,
            self.string.string,
            self.string.scan_lead,
            self.string.utf8_stop,
            self.string.utf8_more,
            self.string.step,
            self.proof.true_qed,
            self.proof.false_absurd,
        ]
        .into_iter()
    }
}

#[derive(Debug, Clone, Copy)]
pub struct MonadSyntax {
    bind: &'static str,
}

impl MonadSyntax {
    pub const fn new(bind: &'static str) -> Self {
        Self { bind }
    }

    pub const fn bind(self) -> &'static str {
        self.bind
    }
}

#[derive(Debug, Clone, Copy)]
pub struct CharacterSyntax {
    character: &'static str,
    scalar_below: &'static str,
    scalar_above: &'static str,
}

impl CharacterSyntax {
    pub const fn new(
        character: &'static str,
        scalar_below: &'static str,
        scalar_above: &'static str,
    ) -> Self {
        Self {
            character,
            scalar_below,
            scalar_above,
        }
    }

    pub const fn character(self) -> &'static str {
        self.character
    }

    pub const fn scalar_below(self) -> &'static str {
        self.scalar_below
    }

    pub const fn scalar_above(self) -> &'static str {
        self.scalar_above
    }
}

#[derive(Debug, Clone, Copy)]
pub struct StringSyntax {
    string: &'static str,
    scan_lead: &'static str,
    utf8_stop: &'static str,
    utf8_more: &'static str,
    step: &'static str,
}

impl StringSyntax {
    pub const fn new(
        string: &'static str,
        scan_lead: &'static str,
        utf8_stop: &'static str,
        utf8_more: &'static str,
        step: &'static str,
    ) -> Self {
        Self {
            string,
            scan_lead,
            utf8_stop,
            utf8_more,
            step,
        }
    }

    pub const fn string(self) -> &'static str {
        self.string
    }

    pub const fn scan_lead(self) -> &'static str {
        self.scan_lead
    }

    pub const fn utf8_stop(self) -> &'static str {
        self.utf8_stop
    }

    pub const fn utf8_more(self) -> &'static str {
        self.utf8_more
    }

    pub const fn step(self) -> &'static str {
        self.step
    }
}

#[derive(Debug, Clone, Copy)]
pub struct ProofSyntax {
    true_qed: &'static str,
    false_absurd: &'static str,
}

impl ProofSyntax {
    pub const fn new(true_qed: &'static str, false_absurd: &'static str) -> Self {
        Self {
            true_qed,
            false_absurd,
        }
    }

    pub const fn true_qed(self) -> &'static str {
        self.true_qed
    }

    pub const fn false_absurd(self) -> &'static str {
        self.false_absurd
    }
}
