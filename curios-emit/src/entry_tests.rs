//! Residual entry capability controls turn exports without changing nominal rows or dispatch needed by ordinary calls.

use crate::test_support::{
    assert_absent, assert_contains, indirect_apply, indirect_apply_discarding_result, nat, wat,
};

fn registered_entry(suspended: bool, construct: bool) -> curios_cont::Module {
    let mut module = curios_cont::Module::new();
    let row = module.suspension_row();
    let main = module.reserve_function();
    let ret = module.reserve_continuation();
    let result = construct.then(|| module.add_value(None));
    let body = module.add_node(curios_cont::Node::ApplyCont(curios_cont::Edge {
        target: ret,
        args: vec![if suspended {
            curios_cont::Atom::Value(result.unwrap())
        } else {
            nat(7)
        }],
    }));
    let body = if construct {
        module.add_node(curios_cont::Node::LetValue {
            result: result.unwrap(),
            value: curios_cont::ValueExpr::Row(
                row,
                vec![curios_cont::Atom::Filler; module.row(row).width()],
            ),
            next: body,
        })
    } else {
        body
    };
    module.define_function(
        main,
        curios_cont::Function {
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: ret,
            body,
            droppable: false,
        },
    );
    module.set_entry(main);
    module
}

#[test]
fn a_registered_row_without_an_escaping_suspension_exports_the_direct_entry() {
    for construct in [false, true] {
        let module = registered_entry(false, construct);
        assert!(module.suspension().is_some());
        let text = wat(&module);
        assert_contains(&text, "(export \"func/main\" (func $func/main))");
        assert_contains(&text, "row/0$suspension");
        assert_absent(&text, "(export \"func/advance\"");
        assert_absent(&text, "(func $io/main");
        assert_absent(&text, "(func $io/advance");
        assert_absent(&text, "(table $clsr/0");
    }
}

#[test]
fn an_escaping_suspension_exports_turns_and_their_dispatch() {
    let text = wat(&registered_entry(true, true));
    assert_contains(&text, "(export \"func/main\" (func $io/main))");
    assert_contains(&text, "(export \"func/advance\" (func $io/advance))");
    assert_contains(&text, "(table $clsr/0");
}

#[test]
fn direct_entry_selection_preserves_dispatch_required_by_surviving_calls() {
    let mut module = indirect_apply_discarding_result();
    module.suspension_row();
    let text = wat(&module);
    assert_absent(&text, "(export \"func/advance\"");
    assert_contains(&text, "call_indirect $clsr/0 ");
    assert_contains(&text, "(table $clsr/0");
}

#[test]
fn an_opaque_callback_return_retains_the_turn_abi() {
    let mut module = indirect_apply();
    module.suspension_row();
    let text = wat(&module);
    assert_contains(&text, "(export \"func/advance\"");
    assert_contains(&text, "(export \"func/main\" (func $io/main))");
}
