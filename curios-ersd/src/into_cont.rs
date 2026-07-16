mod frame;
use frame::*;

mod builder;
use builder::*;

mod conts;
use conts::*;

mod lowerer;
use lowerer::*;

mod lower_prim;

mod rec;
use rec::*;

#[cfg(test)]
mod tests;

use curios_cont::{Func, FuncName, Module};

/// A `into_cont` lowering failure: a Curios language restriction earlier stages don't
/// reject syntactically — only closures may be mutually recursive, and a call/match-valued
/// term cannot be bound where a synchronous value is required — reported as an ordinary
/// compile error instead of panicking.
#[derive(Debug)]
pub enum Error {
    /// A term reaching `Apply`/`Match`/`NatMatch` on its construction path was bound where
    /// a synchronous value is required.
    UnsupportedSyncRecItem { term: String },
    /// Two or more non-closure `rec` bindings depend on each other's value.
    CyclicRecComputed { cycle: Vec<String> },
}

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Error::UnsupportedSyncRecItem { term } => write!(
                f,
                "`into_cont` does not support a call-valued `rec` item in value position: \
                 the following term reaches `Apply`/`Match`/`NatMatch` on its construction path \
                 but is bound where a synchronous value is required: {term}",
            ),
            Error::CyclicRecComputed { cycle } => write!(
                f,
                "`into_cont` does not support value-level mutual recursion: \
                 {} would require runtime fixpoint cells; only closures may be \
                 mutually recursive (a cyclic tuple/array reaches this path too)",
                cycle.join(" -> "),
            ),
        }
    }
}

/// Shorthand for a lowering result: every fallible step in this module fails with the
/// same [`Error`].
type LowerResult<T> = Result<T, Error>;

/// Lower an (optimized) erased module into the continuation IR. The flat `items` list and the entrypoint `body` become the region of a single parameterless `main` — synchronous items are lowered inline into that top-level region, effectful ones through the CPS machinery — with every closure encountered split out as its own `Clsr` in the output module, and `main` set as its entry.
#[cfg_attr(feature = "profile", tracing::instrument(level = "trace", skip_all))]
pub fn into_cont(erased: &crate::Module) -> Result<Module, Error> {
    let mut cont_module = Module::new();

    let (resume, region) = Lowerer::new(&mut cont_module).lower_module(erased, &Frame::new())?;

    let entry = FuncName::from("main");

    cont_module.add_func(
        entry.clone(),
        Func {
            params: vec![],
            resume,
            region,
        },
    );
    cont_module.set_entry(entry);

    Ok(cont_module)
}
