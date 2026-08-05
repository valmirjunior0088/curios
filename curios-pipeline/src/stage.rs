//! The driver's introspection surface: one borrowed view per intermediate representation, and the stage names the CLI's `--print` flag is derived from.

use {curios_cont::CpsModule, curios_text::Entrypoint, std::fmt};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — the CLI's `--print` stage dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
pub enum Stage<'a> {
    Text(&'a Entrypoint),
    /// Core as `curios_text::into_core` produced it: syntax that nothing has checked. It carries term metavariables, lowering-time universe seeds, and unresolved `Infix` and `NumLit` nodes, and its registries are unelaborated. Useful for debugging the lowering; not a typed program.
    Core(&'a curios_core::Module),
    /// Core after elaboration and zonking, which is the module every later stage consumes. Metavariable-free by construction — `zonk_module` errors on an unsolved hole — with universes validated, positivity checked, totality recorded, and both erasure obligations gated. The prelude prefix is spliced back in from the archive.
    ///
    /// The difference from [`Stage::Core`] is the absence of `Metavar`, `Infix`, and `NumLit`, which is exactly what the independent kernel requires of an input: this is the stage `curios_cert::recheck_module` takes, and [`Stage::Core`] is not.
    CoreElab(&'a curios_core::Module),
    Ersd(&'a curios_ersd::Module),
    ErsdOptm(&'a curios_ersd::Module),
    Cont(&'a CpsModule),
    ContOptm(&'a CpsModule),
    Wasm(&'a curios_wasm::Module),
}

impl<'a> Stage<'a> {
    /// Every stage name, in pipeline order — the single source the CLI's `--print` default/help text is derived from, so it cannot drift from [`Stage::name`].
    pub const NAMES: [&'static str; 8] = [
        "text",
        "core",
        "core-elab",
        "ersd",
        "ersd-optm",
        "cont",
        "cont-optm",
        "wasm",
    ];

    /// This stage's name, matching its entry in [`Stage::NAMES`].
    pub fn name(&self) -> &'static str {
        match self {
            Stage::Text(_) => "text",
            Stage::Core(_) => "core",
            Stage::CoreElab(_) => "core-elab",
            Stage::Ersd(_) => "ersd",
            Stage::ErsdOptm(_) => "ersd-optm",
            Stage::Cont(_) => "cont",
            Stage::ContOptm(_) => "cont-optm",
            Stage::Wasm(_) => "wasm",
        }
    }
}

impl fmt::Display for Stage<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stage::Text(entrypoint) => write!(f, "{entrypoint}"),
            Stage::Core(module) => write!(f, "{module}"),
            Stage::CoreElab(module) => write!(f, "{module}"),
            Stage::Ersd(module) => write!(f, "{module}"),
            Stage::ErsdOptm(module) => write!(f, "{module}"),
            Stage::Cont(module) => write!(f, "{module}"),
            Stage::ContOptm(module) => write!(f, "{module}"),
            // The one stage dump rendered within a width: `--print wasm` is a manual-inspection surface, and wide signature lines break one binding per line there. The other document-based dumps keep the unbounded layout until their printers grow break points worth fitting.
            Stage::Wasm(module) => write!(f, "{}", module.display_within(100)),
        }
    }
}
