//! The driver's introspection surface: one borrowed view per intermediate representation, and the stage names `wonder stage`'s rungs are derived from.

use {curios_cont::CpsModule, curios_text::Entrypoint, std::fmt};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — `wonder stage`'s dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
///
/// The enum is the vocabulary of observation points, not a promise that the pure pipeline emits each: [`Stage::WasmOptm`] observes what Binaryen did to the emitted module, and this crate must not depend on Binaryen, so that one variant is constructed downstream by the native product and its payload is rendered text rather than a borrowed IR.
pub enum Stage<'a> {
    Text(&'a Entrypoint),
    /// Core as `curios_text::into_core` produced it: syntax that nothing has checked. It carries term metavariables, lowering-time universe seeds, and unresolved `Transient` nodes (`Infix`, `NumLit`), and its registries are unelaborated. Useful for debugging the lowering; not a typed program.
    Core(&'a curios_core::Module),
    /// Core after elaboration and zonking, which is the module every later stage consumes. Metavariable-free by construction — `zonk_module` errors on an unsolved hole — with universes validated, positivity checked, totality recorded, and both erasure obligations gated. It carries the entry's own items only; the prelude is scope every later stage is seeded from, not a run of items in front of them.
    ///
    /// The difference from [`Stage::Core`] is the absence of `Metavar` and every `Transient`, which is exactly what the independent kernel requires of an input: this is the stage `curios_cert::recheck_module` takes, and [`Stage::Core`] is not.
    CoreElab(&'a curios_core::Module),
    Ersd(&'a curios_ersd::Module),
    ErsdOptm(&'a curios_ersd::Module),
    Cont(&'a CpsModule),
    ContOptm(&'a CpsModule),
    Wasm(&'a curios_wasm::Module),
    /// The Binaryen-optimized module, rendered by Binaryen's own text writer — ground truth from the session that optimized it, not a reader's reconstruction. The native product's `wasm_optm` emits it, mirroring the driver's own observe-at-production idiom; `compile_entrypoint` never does, and `every_stage_is_observed_once_in_names_order` pins that deliberate absence.
    WasmOptm(&'a str),
}

impl<'a> Stage<'a> {
    /// Every stage name, in pipeline order — the single source `wonder stage`'s rung names are derived from. Nothing structural ties this list to [`Stage::name`]; the `every_stage_is_observed_once_in_names_order` test is what pins the two to each other and to the driver's emission order — including the last entry's absence from that order, per [`Stage::WasmOptm`]'s own note.
    pub const NAMES: [&'static str; 9] = [
        "text",
        "core",
        "core-elab",
        "ersd",
        "ersd-optm",
        "cont",
        "cont-optm",
        "wasm",
        "wasm-optm",
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
            Stage::WasmOptm(_) => "wasm-optm",
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
            // The one stage dump rendered within a width: `wonder stage wasm` is a manual-inspection surface, and wide signature lines break one binding per line there. The other document-based dumps keep the unbounded layout until their printers grow break points worth fitting.
            Stage::Wasm(module) => write!(f, "{}", module.display_within(100)),
            // Already laid out by Binaryen's writer; only its trailing newline is trimmed, so this dump ends like every house-rendered one.
            Stage::WasmOptm(text) => write!(f, "{}", text.trim_end()),
        }
    }
}
