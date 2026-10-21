//! The refusal a witness expects once its flaw is closed, and whether what the checkers said meets it.

use {
    crate::{Answer, Checker, Refusal, Witness, declared_in, raised, standing_at},
    curios_core::Global,
    curios_utilities::Qualifier,
    std::fmt,
};

/// One checker's refusal, as a witness expects it. Written by [`refused!`](crate::refused), which keeps the pattern and its spelling one.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Expected {
    /// The elaborator, by an error `is` recognises: every one where `pattern` is absent, which is how an open ticket expects a refusal no error names yet.
    Elaborator {
        pattern: Option<&'static str>,
        within: Option<Within>,
        is: fn(&curios_elab::Error) -> bool,
    },
    /// The kernel, by an error `is` recognises, at the item or entry `within` names, or whichever it refused where it names none.
    Kernel {
        pattern: Option<&'static str>,
        within: Option<Within>,
        is: fn(&curios_cert::Error) -> bool,
    },
}

/// Where an expected refusal stands: the declaration it is raised in, by its path, or the term a program closes with. A mismatch is every conversion's refusal, so one named where the flaw's construction stands is not met by one raised anywhere else.
#[derive(Debug, Clone, Copy)]
pub(crate) enum Within {
    Declaration(&'static str),
    Entry,
}

impl Within {
    /// Whether a refusal standing at `name` stands here.
    fn holds(&self, name: Option<&Global>) -> bool {
        match (self, name) {
            (Within::Entry, None) => true,
            (Within::Declaration(path), Some(Global::Authored(qualifier))) => {
                *qualifier == Qualifier::from(path.trim_start_matches('/').split('/'))
            }
            _ => false,
        }
    }
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
            (Expected::Elaborator { is, within, .. }, Refusal::Elaborator { error, .. }) => {
                error.each().any(|member| {
                    is(raised(member))
                        && within.is_none_or(|within| within.holds(declared_in(member)))
                })
            }
            (Expected::Kernel { is, within, .. }, Refusal::Kernel(verdict)) => {
                is(&verdict.error)
                    && within.is_none_or(|within| within.holds(verdict.name.as_ref()))
            }
            _ => false,
        }
    }
}

impl fmt::Display for Expected {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        // A pattern is spelled as it was written, line breaks included, and a report gives it one line.
        let on_one_line = |pattern: &str| pattern.split_whitespace().collect::<Vec<_>>().join(" ");

        let (pattern, within) = match self {
            Expected::Elaborator {
                pattern, within, ..
            }
            | Expected::Kernel {
                pattern, within, ..
            } => (pattern, within),
        };

        match pattern {
            None => write!(formatter, "the {}", self.checker())?,
            Some(pattern) => write!(
                formatter,
                "the {}'s `{}`",
                self.checker(),
                on_one_line(pattern)
            )?,
        }
        match within {
            None => Ok(()),
            Some(Within::Entry) => write!(formatter, " {}", standing_at(None)),
            Some(Within::Declaration(path)) => write!(formatter, " in `{path}`"),
        }
    }
}

/// What refuses a witness once its flaw is closed, one entry per checker expected to refuse it.
///
/// `refused![kernel: curios_cert::Error::NotASort(_)]` expects the kernel's refusal by that error, and `refused![elaborator: curios_elab::Error::TypeMismatch { .. }, kernel: curios_cert::Error::Mismatch { .. }]` expects both checkers'. Each pattern names its checker's own error with its crate, `curios_elab::Error` under `elaborator` and `curios_cert::Error` under `kernel`. After either, `in` and a declaration's path, `in "/f"`, or `in entry` for the term a program closes with, names where the refusal stands ([`Within`]), and `if` and a condition over what the pattern binds narrows an error whose variant says too little. A label alone, `refused![kernel]`, expects a refusal by an error not named yet, which is how an open ticket states a refusal its fix has still to introduce.
#[macro_export]
macro_rules! refused {
    ($($checker:ident $(: $pattern:pat $(in $within:tt)? $(if $guard:expr)?)?),+ $(,)?) => {
        &[
            $($crate::expected!($checker $(: $pattern $(in $within)? $(if $guard)?)?)),+
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
            within: None,
            is: |_| true,
        }
    };
    (elaborator: $pattern:pat $(in $within:tt)? $(if $guard:expr)?) => {
        $crate::Expected::Elaborator {
            pattern: Some(stringify!($pattern $(if $guard)?)),
            within: $crate::within!($($within)?),
            is: |error| matches!(error, $pattern $(if $guard)?),
        }
    };
    (kernel) => {
        $crate::Expected::Kernel {
            pattern: None,
            within: None,
            is: |_| true,
        }
    };
    (kernel: $pattern:pat $(in $within:tt)? $(if $guard:expr)?) => {
        $crate::Expected::Kernel {
            pattern: Some(stringify!($pattern $(if $guard)?)),
            within: $crate::within!($($within)?),
            is: |error| matches!(error, $pattern $(if $guard)?),
        }
    };
}

/// Where an entry of [`refused!`] stands: nowhere named, the closing term, or a declaration by its path.
#[doc(hidden)]
#[macro_export]
macro_rules! within {
    () => {
        None
    };
    (entry) => {
        Some($crate::Within::Entry)
    };
    ($declaration:literal) => {
        Some($crate::Within::Declaration($declaration))
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
    pub(crate) fn unmet(&self, answer: &Answer) -> Option<String> {
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
