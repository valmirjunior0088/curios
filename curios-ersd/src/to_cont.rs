mod frame;
use frame::*;

mod builder;
use builder::*;

mod conts;
use conts::*;

mod lowerer;
use lowerer::*;

mod lower_prim;
use lower_prim::*;

mod rec;
use rec::*;

use curios_cont::{Func, FuncName, Module};

/// Lower an (optimized) erased module into the continuation IR. The flat `items` list and the entrypoint `body` become the region of a single parameterless `main` — synchronous items are lowered inline into that top-level region, effectful ones through the CPS machinery — with every closure encountered split out as its own `Clsr` in the output module, and `main` set as its entry.
pub fn to_cont(erased: &crate::Module) -> Module {
    let mut cont_module = Module::new();

    let (resume, region) = Lowerer::new(&mut cont_module).lower_module(erased, &Frame::new());

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

    cont_module
}

#[cfg(test)]
mod tests;
