//! What the encoder refuses at index-space construction, before any byte is written.

use crate::{Module, to_bytes};

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
