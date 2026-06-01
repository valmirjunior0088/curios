mod entropy;
use entropy::*;

mod frame;
use frame::*;

mod builder;
use builder::*;

mod conts;
use conts::*;

mod lowerer;
use lowerer::*;

mod rec;
use rec::*;

use crate::{cont, ersd};

pub fn to_cont(erased_term: &ersd::Term) -> cont::Module {
    let mut cont_module = cont::Module::new();

    let (resume, region) = Lowerer::new(&mut cont_module).lower_entry(erased_term, &Frame::new());

    cont_module.add_func(
        cont::FuncName::from("main"),
        cont::Func {
            params: vec![],
            resume,
            region,
        },
    );

    cont_module
}

#[cfg(test)]
mod tests;
