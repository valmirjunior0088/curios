mod entropy;
use entropy::*;

mod frame;
use frame::*;

mod lowerer;
use lowerer::*;

use crate::{cont, core};

pub fn to_cont(core_term: &core::ErasedTerm) -> cont::Module {
    let mut cont_module = cont::Module::new();

    let (resume, region) = Lowerer::new(&mut cont_module).lower_entry(core_term, &Frame::new());

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
mod tests {
    use super::*;

    #[test]
    fn lowers_recursive_pairs_into_main_region_values() {
        let term = core::ErasedTerm::LetRec(core::ErasedLetRec {
            names: vec!["x".into(), "y".into()],
            items: vec![
                core::ErasedTerm::from(core::ErasedPair {
                    fst: core::ErasedTerm::Name(core::ErasedName::from("y")).into(),
                    snd: core::ErasedTerm::Prim(core::ErasedPrim::Int(1)).into(),
                })
                .into(),
                core::ErasedTerm::from(core::ErasedPair {
                    fst: core::ErasedTerm::Prim(core::ErasedPrim::Int(2)).into(),
                    snd: core::ErasedTerm::Name(core::ErasedName::from("x")).into(),
                })
                .into(),
            ],
            tail: core::ErasedTerm::Name(core::ErasedName::from("x")).into(),
        });

        let module = to_cont(&term);

        assert!(module.consts().is_empty());
        assert!(module.clsrs().is_empty());
        assert_eq!(module.funcs().len(), 1);
        assert_eq!(module.funcs()[0].0.string, "main");

        let func = &module.funcs()[0].1;
        assert!(func.region.blocks.is_empty());
        assert_eq!(func.region.values.len(), 4);

        let recursive_pairs = func
            .region
            .values
            .iter()
            .filter_map(|(name, value)| match value {
                cont::Value::Tpl2(left, right) => Some((name.clone(), left.clone(), right.clone())),
                _ => None,
            })
            .collect::<Vec<_>>();

        assert_eq!(recursive_pairs.len(), 2);
        assert!(
            recursive_pairs
                .iter()
                .any(|(name, left, _)| name.string == "v0" && left.string == "v1")
        );
        assert!(
            recursive_pairs
                .iter()
                .any(|(name, _, right)| name.string == "v1" && right.string == "v0")
        );

        let cont::Tail::Jump(target) = &func.region.tail else {
            panic!("expected main tail jump");
        };

        assert_eq!(target.target.string, func.resume.string);
        assert_eq!(target.params.len(), 1);
        assert_eq!(target.params[0].string, "v0");
    }

    #[test]
    fn lowers_tail_apply_as_indirect_call_to_resume() {
        let term = core::ErasedTerm::Apply(core::ErasedApply {
            head: core::ErasedTerm::Func(core::ErasedFunc {
                captures: vec![],
                param: "x".into(),
                body: core::ErasedTerm::Name(core::ErasedName::from("x")).into(),
            })
            .into(),
            param: core::ErasedTerm::Prim(core::ErasedPrim::Int(7)).into(),
        });

        let module = to_cont(&term);

        assert_eq!(module.clsrs().len(), 1);

        let func = &module.funcs()[0].1;
        assert!(func.region.blocks.is_empty());

        let cont::Tail::Call(cont::CallTarget::Indirect { resume, .. }) = &func.region.tail else {
            panic!("expected indirect call in main tail");
        };

        assert_eq!(resume.string, func.resume.string);
    }

    #[test]
    fn lowers_apply_in_value_position_through_join_block() {
        let term = core::ErasedTerm::Pair(core::ErasedPair {
            fst: core::ErasedTerm::Apply(core::ErasedApply {
                head: core::ErasedTerm::Func(core::ErasedFunc {
                    captures: vec![],
                    param: "x".into(),
                    body: core::ErasedTerm::Name(core::ErasedName::from("x")).into(),
                })
                .into(),
                param: core::ErasedTerm::Prim(core::ErasedPrim::Int(7)).into(),
            })
            .into(),
            snd: core::ErasedTerm::Prim(core::ErasedPrim::Int(1)).into(),
        });

        let module = to_cont(&term);
        let func = &module.funcs()[0].1;

        assert_eq!(func.region.blocks.len(), 1);

        let cont::Tail::Call(cont::CallTarget::Indirect { resume, .. }) = &func.region.tail else {
            panic!("expected root indirect call");
        };

        let (block_name, block) = &func.region.blocks[0];
        assert_eq!(block_name.string, resume.string);
        assert_eq!(block.params.len(), 1);
        assert!(
            block
                .region
                .values
                .iter()
                .any(|(_, value)| matches!(value, cont::Value::Tpl2(_, _)))
        );
    }
}
