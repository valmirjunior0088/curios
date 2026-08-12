use {
    super::{Demand, demand_of, demands},
    crate::{CpsAtom, CpsFunction, CpsIntrinsicOp, CpsModule, CpsNode, CpsValueExpr, CpsValueId},
    std::collections::BTreeSet,
};

/// Three parameters, one read only through a projection, one consumed whole, one never mentioned.
fn module() -> (CpsModule, CpsValueId, CpsValueId, CpsValueId) {
    let mut module = CpsModule::default();
    let projected = module.add_value(Some("projected".into()));
    let whole = module.add_value(Some("whole".into()));
    let unused = module.add_value(Some("unused".into()));
    let field = module.add_value(Some("field".into()));
    let built = module.add_value(Some("built".into()));

    let exit = module.add_node(CpsNode::Exit { value: None });
    let construct = module.add_node(CpsNode::LetValue {
        result: built,
        value: CpsValueExpr::Tuple(vec![CpsAtom::Value(whole)]),
        next: exit,
    });
    let project = module.add_node(CpsNode::LetIntrinsic {
        result: field,
        op: CpsIntrinsicOp::TplGet(0),
        args: vec![CpsAtom::Value(projected)],
        next: construct,
    });

    let function = module.reserve_function();
    let return_cont = module.reserve_continuation();
    module.define_function(
        function,
        CpsFunction {
            debug_name: Some("main".into()),
            params: vec![projected, whole, unused],
            return_cont,
            body: project,
        },
    );
    module.set_entry(function);

    (module, projected, whole, unused)
}

#[test]
fn a_projection_is_not_a_whole_use() {
    let (module, projected, whole, unused) = module();
    let demands = demands(&module);

    // The refinement the return protocol needs: this value is read, but never in one piece.
    assert_eq!(
        demand_of(&demands, projected),
        Demand::Projected(BTreeSet::from([0]))
    );
    assert_eq!(demand_of(&demands, whole), Demand::Opaque);
    assert_eq!(demand_of(&demands, unused), Demand::Unused);
}

#[test]
fn an_unseeded_value_is_opaque_and_never_unused() {
    let (module, ..) = module();
    let demands = demands(&module);
    let absent = CpsValueId(9999);

    // Absence must read as the top rather than the bottom. Reading it as `Unused` is dead-parameter elimination deleting a parameter the walk simply never reached.
    assert!(!demands.contains_key(&absent));
    assert_eq!(demand_of(&demands, absent), Demand::Opaque);
}
