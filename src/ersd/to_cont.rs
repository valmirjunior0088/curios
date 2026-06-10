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

use crate::{cont, ersd};

pub fn to_cont(erased: &ersd::Module) -> cont::Module {
    let mut cont_module = cont::Module::new();

    let (resume, region) = Lowerer::new(&mut cont_module).lower_module(erased, &Frame::new());

    let entry = cont::FuncName::from("main");

    cont_module.add_func(
        entry.clone(),
        cont::Func {
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
