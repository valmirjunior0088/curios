use super::*;

#[test]
fn round_trip() {
    let source = r#"
        (module $round_trip
            (type $id (func (param i32) (result i32)))
            (type $point (struct (field $x i32) (field $y (mut i32))))
            (type $bytes (array (mut i8)))
            (import "env" "ext_add" (func $ext_add (type $id)))
            (func $demo (type $id) (param $x i32) (result i32)
                (local $tmp i32)
                i32.const 41
                local.set $tmp
                local.get $x
                i32.const 1
                i32.add
                i32.const 2
                struct.new $point
                drop
                i32.const 3
                array.new_fixed $bytes 1
                drop
                i32.const 0
                i32.const 5
                array.new_data $bytes $greeting
                drop
                local.get $tmp)
            (global $answer (mut i32)
                i32.const 41)
            (data $greeting "\68\65\6c\6c\6f")
            (export "demo" (func $demo))
            (export "answer" (global $answer)))
"#;

    let first = source
        .parse::<Module>()
        .expect("expected first module")
        .to_string();

    let second = first
        .parse::<Module>()
        .expect("expected second module")
        .to_string();

    assert_eq!(first, second);
}
