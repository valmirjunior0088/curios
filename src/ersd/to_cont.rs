mod entropy;
use entropy::*;

mod frame;
use frame::*;

mod lowerer;
use lowerer::*;

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
mod tests {
    use {
        super::to_cont,
        crate::{
            cont,
            ersd::{Apply, Func, Let, LetRec, Name, Pair, Prim, Term},
        },
    };

    #[test]
    fn lowers_recursive_pairs_into_main_region_values() {
        let term = Term::LetRec(LetRec {
            names: vec!["x".into(), "y".into()],
            items: vec![
                Term::from(Pair {
                    fst: Term::Name(Name::from("y")).into(),
                    snd: Term::Prim(Prim::Int(1)).into(),
                })
                .into(),
                Term::from(Pair {
                    fst: Term::Prim(Prim::Int(2)).into(),
                    snd: Term::Name(Name::from("x")).into(),
                })
                .into(),
            ],
            tail: Term::Name(Name::from("x")).into(),
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
                cont::Value::Pure(cont::Data::Tpl(elems)) if elems.len() == 2 => {
                    Some((name.clone(), elems[0].clone(), elems[1].clone()))
                }
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
        let term = Term::Apply(Apply {
            head: Term::Func(Func {
                captures: vec![],
                param: "x".into(),
                body: Term::Name(Name::from("x")).into(),
            })
            .into(),
            param: Term::Prim(Prim::Int(7)).into(),
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
    fn lowers_lst_into_main_region_value() {
        let term = Term::Let(Let {
            name: "a".into(),
            body: Term::Prim(Prim::Nat(1)).into(),
            tail: Term::Let(Let {
                name: "b".into(),
                body: Term::Prim(Prim::Nat(2)).into(),
                tail: Term::Prim(Prim::Lst(vec![
                    Term::Name(Name::from("a")).into(),
                    Term::Name(Name::from("b")).into(),
                ]))
                .into(),
            })
            .into(),
        });

        let module = to_cont(&term);
        let func = &module.funcs()[0].1;

        assert!(func.region.values.iter().any(|(_, value)| matches!(
            value,
            cont::Value::Pure(cont::Data::Lst(elems)) if elems.len() == 2
        )));
    }

    #[test]
    fn lowers_lst_with_apply_element_through_join_block() {
        let term = Term::Prim(Prim::Lst(vec![
            Term::Apply(Apply {
                head: Term::Func(Func {
                    captures: vec![],
                    param: "x".into(),
                    body: Term::Name(Name::from("x")).into(),
                })
                .into(),
                param: Term::Prim(Prim::Nat(1)).into(),
            })
            .into(),
            Term::Prim(Prim::Nat(2)).into(),
        ]));

        let module = to_cont(&term);
        let func = &module.funcs()[0].1;

        assert_eq!(func.region.blocks.len(), 1);

        let (_, block) = &func.region.blocks[0];

        assert!(block.region.values.iter().any(|(_, value)| matches!(
            value,
            cont::Value::Pure(cont::Data::Lst(elems)) if elems.len() == 2
        )));
    }

    #[test]
    fn lowers_apply_in_value_position_through_join_block() {
        let term = Term::Pair(Pair {
            fst: Term::Apply(Apply {
                head: Term::Func(Func {
                    captures: vec![],
                    param: "x".into(),
                    body: Term::Name(Name::from("x")).into(),
                })
                .into(),
                param: Term::Prim(Prim::Int(7)).into(),
            })
            .into(),
            snd: Term::Prim(Prim::Int(1)).into(),
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
                .any(|(_, value)| matches!(value, cont::Value::Pure(cont::Data::Tpl(_))))
        );
    }

    #[test]
    fn lowers_bin_literal() {
        let term = Term::Prim(Prim::Bin(vec![1, 2, 3]));
        let module = to_cont(&term);

        let func = &module.funcs()[0].1;
        let has_bin = func.region.values.iter().any(|(_, value)| {
            matches!(value, cont::Value::Pure(cont::Data::Bin(bytes)) if bytes == &[1, 2, 3])
        });

        assert!(has_bin);
    }

    #[test]
    fn lowers_nat_to_bin() {
        let term = Term::Prim(Prim::NatToBin(Term::Prim(Prim::Nat(0x42)).into()));
        let module = to_cont(&term);

        let func = &module.funcs()[0].1;
        let has_nat_to_bin = func.region.values.iter().any(|(_, value)| {
            matches!(value, cont::Value::Eval(cont::Code::NatToBin(_)))
        });

        assert!(has_nat_to_bin);
    }
}
