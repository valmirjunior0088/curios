//! A binder whose producer names no carrier — a parameter, a call's result, a payload read — states the register carrier its type names, read at the type the binder is checked at, and a type naming none states nothing.

use {
    curios_ersd::{Module, RegisterShape},
    curios_pipeline::{Stage, compile_with_prelude},
    curios_text::{Entrypoint, RootSource},
};

const STATING: &str = r#"
    use /std/{Flt, Bool, Nat, Option, List, Process, Str};
    let scale(stated_flt : Flt, stated_flag : Bool) -> Flt =
        match stated_flag | true => stated_flt * +2.0 | false => stated_flt end;
    let poly(A : Type, poly_value : A) -> A = poly_value;
    let Held(b : Bool) -> Type = match b | true => Flt | false => Str end;
    let held(b : Bool, held_value : Held(b)) -> Held(b) = held_value;
    let pick(o : Option(Flt)) -> Flt = match o | some(payload_flt) => payload_flt | none() => +0.0 end;
    let entry() -> /std/Io({}) =
        let counted_nat = List/len(Process/args()!);
        let called_flt = scale(Nat/to_flt(counted_nat), counted_nat % 2 == 0);
        let instantiated_flt = poly(Flt, called_flt);
        let reduced_flt = held(true, instantiated_flt);
        /std/print(Flt/to_str(pick(Option/some(reduced_flt))));
    entry()
    "#;

fn erased(source: &str) -> Module {
    let entrypoint = source.parse::<Entrypoint>().expect("source parses");
    let mut ersd = None;
    compile_with_prelude(
        curios_pipeline::DEFAULT_BUDGET,
        &entrypoint,
        &RootSource::none(),
        |stage| {
            if let Stage::Ersd(module) = stage {
                ersd = Some(module.clone());
            }
        },
    )
    .expect("source compiles");
    ersd.expect("ersd stage observed")
}

/// What each binder named `name` states.
fn stated(module: &Module, name: &str) -> Vec<Option<RegisterShape>> {
    module
        .values()
        .iter()
        .flatten()
        .filter(|definition| definition.debug_name.as_deref() == Some(name))
        .map(|definition| definition.register)
        .collect()
}

#[test]
fn a_binder_states_a_register_carrier_only_where_its_type_names_one() {
    let module = erased(STATING);
    for (name, register) in [
        ("stated_flt", Some(RegisterShape::Flt)),
        ("stated_flag", Some(RegisterShape::Immediate)),
        ("payload_flt", Some(RegisterShape::Flt)),
        ("called_flt", Some(RegisterShape::Flt)),
        // A call's result is read at the caller's instance, and a computed type at what it reduces to.
        ("instantiated_flt", Some(RegisterShape::Flt)),
        ("reduced_flt", Some(RegisterShape::Flt)),
        ("poly_value", None),
        ("held_value", None),
        ("counted_nat", None),
    ] {
        assert_eq!(
            stated(&module, name),
            [register],
            "{name} states {register:?}"
        );
    }
}
