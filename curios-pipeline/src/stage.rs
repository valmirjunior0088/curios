//! The driver's introspection surface: one borrowed view per intermediate representation, and the stage names `wonder --stage`'s rungs are derived from.

use {
    curios_common::SyntaxRegistry,
    curios_module::SYNTAX,
    curios_surface::{Entrypoint, FormScope, Spellable, Spelling},
    curios_unit::Predecessors,
    std::{fmt, rc::Rc},
};

/// A borrowed view of one intermediate representation, handed to the caller's `observe` callback the moment that stage is produced. This is the pipeline's only introspection surface — `wonder --stage`'s dumps and the test suites' IR assertions both hang off it — and borrowing keeps the driver from retaining any stage it has already lowered past.
///
/// The rungs end at the emitted module, the last representation the compiler controls: what Binaryen makes of it is the browser target's `.wasm`, inspected there by hand.
pub enum Stage<'a> {
    Text(&'a Entrypoint),
    /// Core as `curios_module::into_core` produced it: syntax that nothing has checked. It carries term and universe metavariables — the universes' seeds travel beside it, in what the lowering hands elaboration — and unresolved `Transient` nodes (`Infix`, `NumLit`), and the entries its items hold are unelaborated. Useful for debugging the lowering; not a typed program.
    Core(&'a curios_core::Program, DumpScope<'a>),
    /// Core after elaboration and zonking, which is the program every later stage consumes. Metavariable-free by construction — each item is zonked strictly where it is published, which errors on an unsolved hole — with universes validated, positivity checked, totality recorded, and both erasure obligations gated. It carries the entry's own items only; the prelude is scope every later stage is seeded from, not a run of items in front of them.
    ///
    /// The difference from [`Stage::Core`] is the absence of `Metavar` and every `Transient`, which is exactly what the independent certifier requires of an input: this is the stage `curios_cert::certify_definitions` takes, and [`Stage::Core`] is not.
    CoreElab(&'a curios_core::Program, DumpScope<'a>),
    Anf(&'a curios_anf::Module),
    AnfOptm(&'a curios_anf::Module),
    Cps(&'a curios_cps::Module),
    CpsOptm(&'a curios_cps::Module),
    Wasm(&'a curios_wasm::Module),
}

/// What a dump of Core is written over: the registry the program was compiled with and the units before it, whose entries a test's and a foreign row's forms are made against ([`FormScope`]).
#[derive(Clone, Copy)]
pub struct DumpScope<'a> {
    pub syntax: &'a SyntaxRegistry,
    pub predecessors: Predecessors<'a>,
}

impl DumpScope<'_> {
    /// What a dump of Core is written under: Curios/Core proper, as a text of its own, a test's and a foreign row's forms made against the units before it.
    pub fn spelling(&self) -> Spelling {
        let units = self.predecessors.placed();
        let scope = FormScope::over(
            *self.syntax,
            units.iter().map(|(definitions, ..)| *definitions),
        );

        Spelling::core_text(self.syntax)
            .with_form_scope(Rc::new(scope))
            .written_as_text()
    }
}

impl<'a> Stage<'a> {
    /// Every stage name, in pipeline order — the single source `wonder --stage`'s rung names are derived from. Nothing structural ties this list to [`Stage::name`]; the `every_stage_is_observed_once_in_names_order` test is what pins the two to each other and to the driver's emission order.
    pub const NAMES: [&'static str; 8] = [
        "text",
        "core",
        "core-elab",
        "anf",
        "anf-optm",
        "cps",
        "cps-optm",
        "wasm",
    ];

    /// Whether `name` is a rung a unit has: its text, its lowered module and its elaborated one, the three names [`Stage::NAMES`] opens with. From `anf` on a program's entry roots what is rendered, and a unit has none.
    pub fn of_a_unit(name: &str) -> bool {
        Self::NAMES[..3].contains(&name)
    }

    /// Whether `name` is a rung a program read from Curios/Core has: every rung from its elaborated module on, the two before it being a lowering's, which Curios/Core has none of.
    pub fn of_core_text(name: &str) -> bool {
        Self::NAMES[2..].contains(&name)
    }

    /// This stage's name, matching its entry in [`Stage::NAMES`].
    pub fn name(&self) -> &'static str {
        match self {
            Stage::Text(_) => "text",
            Stage::Core(..) => "core",
            Stage::CoreElab(..) => "core-elab",
            Stage::Anf(_) => "anf",
            Stage::AnfOptm(_) => "anf-optm",
            Stage::Cps(_) => "cps",
            Stage::CpsOptm(_) => "cps-optm",
            Stage::Wasm(_) => "wasm",
        }
    }
}

impl fmt::Display for Stage<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Stage::Text(entrypoint) => write!(f, "{}", entrypoint.printed(&SYNTAX)),
            Stage::Core(program, scope) | Stage::CoreElab(program, scope) => {
                write!(f, "{}", program.spelled(&Rc::new(scope.spelling())))
            }
            Stage::Anf(module) => write!(f, "{module}"),
            Stage::AnfOptm(module) => write!(f, "{module}"),
            Stage::Cps(module) => write!(f, "{module}"),
            Stage::CpsOptm(module) => write!(f, "{module}"),
            // The one stage dump rendered within a width: `wonder --stage wasm` is a manual-inspection surface, and wide signature lines break one binding per line there. The other document-based dumps use the unbounded layout, their printers having no break points worth fitting.
            Stage::Wasm(module) => write!(f, "{}", module.display_within(100)),
        }
    }
}
