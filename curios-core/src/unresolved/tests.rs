use super::*;

fn failed(
    segment: &str,
    sought: Sought,
    within: Option<Qualifier>,
    missing: Missing,
) -> Unresolved {
    Unresolved {
        segment: segment.to_string(),
        sought,
        within,
        missing,
        nearest: None,
        elsewhere: Vec::new(),
    }
}

#[test]
fn a_name_the_scope_does_not_hold_is_not_in_scope() {
    let unresolved = failed("coutn", Sought::Binding, None, Missing::Absent);

    assert_eq!(unresolved.to_string(), "`coutn` is not in scope");
}

#[test]
fn a_step_a_module_does_not_hold_names_the_module_it_stopped_in() {
    let binding = failed(
        "to_srt",
        Sought::Binding,
        Some(Qualifier::from(["std", "Nat"])),
        Missing::Absent,
    );
    let module = failed(
        "el",
        Sought::Module,
        Some(Qualifier::from(["std", "Nat"])),
        Missing::Absent,
    );

    assert_eq!(binding.to_string(), "`/std/Nat` has no `to_srt`");
    assert_eq!(module.to_string(), "`/std/Nat` has no module `el`");
}

#[test]
fn a_step_from_the_root_names_the_path_it_would_have_been() {
    let unresolved = failed(
        "Nope",
        Sought::Module,
        Some(Qualifier::empty()),
        Missing::Absent,
    );

    assert_eq!(unresolved.to_string(), "`/Nope` names no module");
}

#[test]
fn a_private_step_names_the_module_it_is_private_to() {
    let unresolved = failed(
        "Sealed",
        Sought::Module,
        Some(Qualifier::from(["A"])),
        Missing::Private,
    );

    assert_eq!(unresolved.to_string(), "`Sealed` is private to `/A`");
}

// A root the unit may not name is the step that failed, and what it is offered are the paths that carry the name it reached for under that root: each is shown by its own last segment, not by the root's.
#[test]
fn a_root_out_of_reach_is_refused_with_what_carries_the_name_reached_for() {
    let unresolved = Unresolved {
        elsewhere: vec![Qualifier::from(["std", "Nat"])],
        ..failed(
            "sys",
            Sought::Module,
            Some(Qualifier::empty()),
            Missing::Internal,
        )
    };

    assert_eq!(
        unresolved.to_string(),
        "`sys` is internal to the standard library\n  `Nat` is `/std/Nat`: write it absolute, or `use /std/{Nat};`"
    );
}

#[test]
fn a_root_the_unit_did_not_declare_is_told_to_declare_it() {
    let unresolved = failed(
        "geometry",
        Sought::Module,
        Some(Qualifier::empty()),
        Missing::Undeclared,
    );

    assert_eq!(
        unresolved.to_string(),
        "`geometry` is mounted by this compilation but is not a declared dependency of this unit; declare it to reach its names"
    );
}

#[test]
fn a_literals_head_is_said_to_name_no_structure() {
    let unresolved = failed("Piar", Sought::Structure, None, Missing::Absent);

    assert_eq!(unresolved.to_string(), "`Piar` names no structure in scope");
}

#[test]
fn the_nearest_name_and_each_way_to_reach_one_elsewhere_follow_the_refusal() {
    let unresolved = Unresolved {
        nearest: Some("Bol".to_string()),
        elsewhere: vec![Qualifier::from(["std", "Bool"])],
        ..failed("Bool", Sought::Binding, None, Missing::Absent)
    };

    assert_eq!(
        unresolved.to_string(),
        "`Bool` is not in scope\n  did you mean `Bol`?\n  `Bool` is `/std/Bool`: write it absolute, or `use /std/{Bool};`"
    );
}
