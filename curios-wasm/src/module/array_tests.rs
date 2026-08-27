//! Array data and element segments, and the copy instruction over them.

use super::test_support::*;

#[test]
fn the_array_data_segment_instructions_round_trip() {
    round_trips(
        r#"
        (module $array_datas
            (type $f (func))
            (type $bytes (array (mut i8)))
            (func $a (type $f)
                i32.const 0
                i32.const 1
                array.new_data $bytes $d
                i32.const 0
                i32.const 0
                i32.const 1
                array.init_data $bytes $d)
            (data $d passive "\00"))
"#,
    );
}

/// `array.copy` names its destination type before its source, in the printed order and the encoded one alike.
#[test]
fn the_array_copy_instruction_round_trips() {
    round_trips(
        r#"
        (module $array_copy
            (type $f (func))
            (type $dst (array (mut i8)))
            (type $src (array i8))
            (func $a (type $f)
                i32.const 1
                array.new_default $dst
                i32.const 0
                i32.const 0
                i32.const 1
                array.new_data $src $d
                i32.const 0
                i32.const 1
                array.copy $dst $src)
            (data $d passive "\00"))
"#,
    );
}

#[test]
fn the_array_element_segment_instructions_round_trip() {
    round_trips(
        r#"
        (module $array_elems
            (type $f (func))
            (type $funcs (array (mut (ref null func))))
            (func $a (type $f)
                i32.const 0
                i32.const 1
                array.new_elem $funcs $e
                i32.const 0
                i32.const 0
                i32.const 1
                array.init_elem $funcs $e)
            (elem $e passive func $a))
"#,
    );
}
