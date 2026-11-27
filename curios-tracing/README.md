# curios-tracing

The macros a site instruments with, `profile!`, `sample!` and `note!`, over `tracing`'s callsites and nothing else, and the one crate that names `tracing`: the recorder, [`curios-profile`](../curios-profile/README.md), implements its subscriber interface through this crate, and what it makes of the macros is its own.

## Design

### A span is this crate's macro, not `tracing::instrument`

**Decision.** A site instruments with `profile!`, `sample!` and `note!`, which expand to `$crate::tracing::…` and ask nothing of the caller: `profile!`, written as a function's first statement, times the function, and its `=>` form times one expression; `sample!` records a magnitude as the one reading a recorder holds it by, an event carrying `value`; `note!` is `tracing`'s own event syntax. The macros are a crate of their own, which takes `tracing` alone.

**Rationale.** What instruments reaches from the foundation of the compiler to the runtime, and the runtime is the slim launcher's, which takes nothing of the compiler's: a crate holding the macros and `tracing` alone is one every instrumenting crate can take, so the trusted base, the prelude's build and the launcher reach `tracing` and no recorder, and the launcher no more than that. `tracing::instrument`'s expansion requires a crate literally named `tracing` in the invoking crate's extern prelude, so it cannot be re-exported, and every instrumenting crate would name `tracing` itself; and its procedural macro would write code inside the checkers. A guard the macro binds is one no site writes, and a magnitude's field is spelled in `sample!` alone, where a site writing `tracing` itself would spell it at each of its sites and a misspelt one would be read as no magnitude.

**Rejected.** `tracing` written at every site: a guard bound by hand at every span, and the field a magnitude is read by restated at every sample. The macros in `curios-common`, the foundation every stage takes: the runtime instruments too, and taking them there would put the whole foundation into the slim launcher.
