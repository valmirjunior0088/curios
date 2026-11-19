//! Residual entry capability controls turn exports without changing nominal rows or dispatch needed by ordinary calls.

use crate::test_support::*;

fn registered_entry(suspended: bool, construct: bool) -> curios_cps::Module {
    let mut module = curios_cps::Module::new();
    let row = module.suspension_row();
    let main = module.reserve_function();
    let ret = module.reserve_continuation();
    let result = construct.then(|| module.add_value(None));
    let body = module.add_node(curios_cps::Node::ApplyCont(curios_cps::Edge {
        target: ret,
        args: vec![if suspended {
            curios_cps::Atom::Value(result.unwrap())
        } else {
            nat(7)
        }],
    }));
    let body = if construct {
        module.add_node(curios_cps::Node::LetValue {
            result: result.unwrap(),
            value: curios_cps::ValueExpr::Row(
                row,
                vec![curios_cps::Atom::Filler; module.row(row).width()],
            ),
            next: body,
        })
    } else {
        body
    };
    module.define_function(
        main,
        curios_cps::Function {
            lineage: None,
            debug_name: Some("main".into()),
            params: vec![],
            return_cont: ret,
            body,
            droppable: curios_cps::Droppable::Never,
            terminates: false,
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

/// A closure call answers what its callees return, so a callback answering a suspension carries it to the entry's return and the turns stay, and one answering a number leaves the entry direct.
#[test]
fn a_callbacks_suspension_reaches_the_entry_through_its_closure_call() {
    for suspends in [false, true] {
        let mut module = match suspends {
            false => indirect_apply(),
            true => indirect_apply_suspending(),
        };
        module.suspension_row();
        let text = wat(&module);
        match suspends {
            true => {
                assert_contains(&text, "(export \"func/advance\"");
                assert_contains(&text, "(export \"func/main\" (func $io/main))");
            }
            false => {
                assert_absent(&text, "(export \"func/advance\"");
                assert_contains(&text, "(export \"func/main\" (func $func/main))");
            }
        }
    }
}
