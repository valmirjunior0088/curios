use curios_wasm::{CompType, Export, SubType, TypeName};

fn func_arity(sub_type: &SubType) -> (usize, usize) {
    match &sub_type.comp_type {
        CompType::Func(func_type) => (func_type.inputs().len(), func_type.outputs().len()),
        comp_type => panic!("expected a func type, got {comp_type:?}"),
    }
}

/// The bridge's `bin` declaration is the compiler's, verbatim — the premise
/// of the whole canonicalization scheme.
#[test]
fn bin_type_is_the_compilers() {
    assert_eq!(
        super::bridge_module().get_type(&TypeName::from("bin")),
        Some(&curios_cont::bin_sub_type())
    );
}

#[test]
fn accessors_are_exported_with_their_shapes() {
    let module = super::bridge_module();

    for (name, inputs, outputs) in [
        ("bin_len", 1, 1),
        ("bin_get", 2, 1),
        ("bin_new", 1, 1),
        ("bin_set", 3, 0),
    ] {
        let export = module
            .exports()
            .iter()
            .find(|(export_name, _)| export_name == name);

        assert!(
            matches!(export, Some((_, Export::Func(_)))),
            "missing func export {name}"
        );

        let sub_type = module
            .get_type(&TypeName::from(name))
            .unwrap_or_else(|| panic!("missing type {name}"));

        assert_eq!(func_arity(sub_type), (inputs, outputs), "{name}");
    }
}

#[test]
fn bridge_bytes_encode_a_wasm_module() {
    assert_eq!(&super::bridge_bytes()[..4], b"\0asm");
}
