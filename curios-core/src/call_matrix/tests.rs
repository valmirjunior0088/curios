use crate::*;

/// A parameter every use of which is seen and that is applied nowhere is stated, as applying nothing, so a summary holding one is filed; only a part that uses every parameter otherwise states nothing.
#[test]
fn a_parameter_applied_nowhere_is_stated() {
    let nowhere = PartApplications::new(Vec::new(), vec![Some(Vec::new())], Vec::new());
    let otherwise = PartApplications::new(Vec::new(), vec![None], Vec::new());

    assert!(!nowhere.states_nothing());
    assert!(otherwise.states_nothing());
    assert!(!Applications::new(vec![nowhere]).states_nothing());
    assert!(Applications::new(vec![otherwise]).states_nothing());
}
