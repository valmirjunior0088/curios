//! The encoder: what it refuses at index-space construction, before any byte is written, and what each function's entry of the code section measures.

use crate::*;

/// The local index space is the type's inputs followed by the locals; a param list of another length would number every local into the wrong slot, in a module that still validates.
#[test]
#[should_panic(expected = "func `f` names 1 params for a type of 2 inputs")]
fn a_param_list_of_another_length_than_its_type_is_refused() {
    let module = r#"
        (module $m
            (type $t (func (param i32 i32) (result i32)))
            (func $f (type $t) (param $x i32)
                (local $l i32)
                i32.const 7
                local.set $l
                local.get $l))
"#
    .parse::<Module>()
    .expect("expected a module");

    to_bytes(&module);
}

/// A body's size is its entry of the code section without the size before it: the locals, run-length encoded, then the expression and its `end`, the very bytes the module holds after that size.
#[test]
fn a_functions_code_size_is_its_entry_after_the_size() {
    let module = r#"
        (module $m
            (type $t (func (result i32)))
            (func $seven (type $t)
                i32.const 7)
            (func $local (type $t)
                (local $l i32)
                local.get $l))
"#
    .parse::<Module>()
    .expect("expected a module");

    let sizes = code_sizes(&module);
    // No locals, `i32.const 7`, `end`; then one run of one `i32`, `local.get 0`, `end`.
    let seven = [0x00, 0x41, 0x07, 0x0B];
    let local = [0x01, 0x01, 0x7F, 0x20, 0x00, 0x0B];
    assert_eq!(sizes[&FuncName::from("seven")], seven.len());
    assert_eq!(sizes[&FuncName::from("local")], local.len());

    let bytes = to_bytes(&module);
    for body in [&seven[..], &local[..]] {
        let entry = [&[body.len() as u8][..], body].concat();
        assert!(
            bytes.windows(entry.len()).any(|window| window == entry),
            "the module holds {entry:02X?}"
        );
    }
}
