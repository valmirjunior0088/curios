//! The refusal a witness expects once its flaw is closed, and whether what the checkers said meets it.

use {
    crate::{Answer, Checker, Refusal, Witness, raised},
    std::fmt,
};

/// One checker's refusal, as a witness expects it. Written by [`refused!`](crate::refused), which keeps the pattern and its spelling one.
#[derive(Debug, Clone, Copy)]
pub enum Expected {
    /// The elaborator, by an error `is` recognises: every one where `pattern` is absent, which is how an open ticket expects a refusal no error names yet.
    Elaborator {
        pattern: Option<&'static str>,
        is: fn(&curios_elab::Error) -> bool,
    },
    /// The kernel, by an error `is` recognises, over whichever item or entry it refused.
    Kernel {
        pattern: Option<&'static str>,
        is: fn(&curios_cert::Error) -> bool,
    },
}

impl Expected {
    /// The checker expected to refuse.
    fn checker(&self) -> Checker {
        match self {
            Expected::Elaborator { .. } => Checker::Elaborator,
            Expected::Kernel { .. } => Checker::Kernel,
        }
    }

    /// Whether `refusal` is the one expected.
    fn meets(&self, refusal: &Refusal) -> bool {
        match (self, refusal) {
            (Expected::Elaborator { is, .. }, Refusal::Elaborator { error, .. }) => {
                error.each().any(|member| is(raised(member)))
            }
            (Expected::Kernel { is, .. }, Refusal::Kernel(verdict)) => is(&verdict.error),
            _ => false,
        }
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        // A pattern is spelled as it was written, line breaks included, and a report gives it one line.
        let on_one_line = |pattern: &str| pattern.split_whitespace().collect::<Vec<_>>().join(" ");

        let pattern = match self {
            Expected::Elaborator { pattern, .. } | Expected::Kernel { pattern, .. } => pattern,
        };

        match pattern {
            None => write!(formatter, "the {}", self.checker()),
            Some(pattern) => write!(
                formatter,
                "the {}'s `{}`",
                self.checker(),
                on_one_line(pattern)
            ),
        }
    }
}

/// What refuses a witness once its flaw is closed, one entry per checker expected to refuse it.
///
/// `refused![kernel: Error::NotASort(_)]` expects the kernel's refusal by that error, and `refused![elaborator: Error::TypeMismatch { .. }, kernel: Error::Mismatch { .. }]` expects both checkers'. `Error` is the checker's own under each label, `curios_elab::Error` and `curios_cert::Error`. After either, `if` and a condition over what the pattern binds narrows an error whose variant says too little. A label alone, `refused![kernel]`, expects a refusal by an error not named yet, which is how an open ticket states a refusal its fix has still to introduce.
#[macro_export]
macro_rules! refused {
    ($($checker:ident $(: $pattern:pat $(if $guard:expr)?)?),+ $(,)?) => {
        &[
            $($crate::expected!($checker $(: $pattern $(if $guard)?)?)),+
        ]
    };
}

/// One entry of [`refused!`].
#[doc(hidden)]
#[macro_export]
macro_rules! expected {
    (elaborator) => {
        $crate::Expected::Elaborator {
            pattern: None,
            is: |_| true,
        }
    };
    (elaborator: $pattern:pat $(if $guard:expr)?) => {
        $crate::Expected::Elaborator {
            pattern: Some(stringify!($pattern $(if $guard)?)),
            is: |error| {
                use ::curios_elab::Error;

                matches!(error, $pattern $(if $guard)?)
            },
        }
    };
    (kernel) => {
        $crate::Expected::Kernel {
            pattern: None,
            is: |_| true,
        }
    };
    (kernel: $pattern:pat $(if $guard:expr)?) => {
        $crate::Expected::Kernel {
            pattern: Some(stringify!($pattern $(if $guard)?)),
            is: |error| {
                use ::curios_cert::Error;

                matches!(error, $pattern $(if $guard)?)
            },
        }
    };
}

/// What a refusal says, on one line, with the checker and error it is.
fn said(refusal: &Refusal) -> String {
    let reported = refusal.to_string();

    format!(
        "{} says {}",
        refusal.named(),
        reported.lines().next().unwrap_or_default()
    )
}

impl Witness {
    /// What keeps `answer` from being the refusal the witness expects, or nothing.
    pub fn unmet(&self, answer: &Answer) -> Option<String> {
        let refusals = answer.refusals();
        let [first, ..] = refusals else {
            return Some("admitted".to_string());
        };

        self.expect
            .iter()
            .find(|expected| !refusals.iter().any(|refusal| expected.meets(refusal)))
            .map(|expected| {
                let asked = expected.checker() == Checker::Elaborator || answer.kernel_asked();
                let nearest = refusals
                    .iter()
                    .find(|refusal| refusal.by() == expected.checker())
                    .unwrap_or(first);

                match asked {
                    true => format!("refused, but not by {expected}: the {}", said(nearest)),
                    false => format!(
                        "refused, but not by {expected}, which was not asked: the {}",
                        said(nearest)
                    ),
                }
            })
    }
}
