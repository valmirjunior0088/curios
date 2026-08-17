use crate::*;

/// Printing a parsed module, parsing that text, and printing it again must reach the same text. The fixed point is what pins a construct's text form against both halves at once: a printer and a parser that disagree cannot both survive it.
fn round_trips(source: &str) {
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

#[test]
fn round_trip() {
    round_trips(
        r#"
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
                array.new_data $bytes $greeting$0
                drop
                i32.const 0
                i32.load8_u $mem
                i32.const 7
                i32.store8 $mem
                memory.size $mem
                drop
                i32.const 1
                memory.grow $mem
                drop
                local.get $tmp)
            (memory $mem i32 0)
            (global $answer (mut i32)
                i32.const 41)
            (data $greeting$0 passive "\68\65\6c\6c\6f")
            (export "demo" (func $demo))
            (export "answer" (global $answer))
            (export "memory" (memory $mem))
            (elem $declared declare func $demo))
"#,
    );
}

#[test]
fn table_declared_imported_and_exported() {
    round_trips(
        r#"
        (module $tables
            (import "env" "given" (table $given i32 1 8 (ref null func)))
            (table $own i32 2 (ref null func))
            (export "own" (table $own)))
"#,
    );
}

#[test]
fn table_with_an_initializer() {
    round_trips(
        r#"
        (module $table_init
            (type $f (func))
            (func $a (type $f))
            (table $t i32 1 1 (ref func)
                ref.func $a)
            (elem $declared declare func $a))
"#,
    );
}

/// A 64-bit table, which the memory64 proposal admits beside a 64-bit memory.
#[test]
fn table_with_sixty_four_bit_addresses() {
    round_trips(
        r#"
        (module $table64
            (table $t i64 1 (ref null func)))
"#,
    );
}

/// Element-segment flag `0x00`: active at the first table, function indices.
#[test]
fn elem_active_at_the_first_table_with_funcs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (table $t i32 1 (ref null func))
            (elem $e (table $t) (offset i32.const 0) func $a))
"#,
    );
}

/// Element-segment flag `0x01`: passive, function indices — what `table.init` copies from.
#[test]
fn elem_passive_with_funcs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e passive func $a))
"#,
    );
}

/// Element-segment flag `0x02`: active at a table that is not the first, so the table index is spelled out.
#[test]
fn elem_active_at_a_later_table_with_funcs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (table $first i32 1 (ref null func))
            (table $second i32 1 (ref null func))
            (elem $e (table $second) (offset i32.const 0) func $a))
"#,
    );
}

/// Element-segment flag `0x03`: declarative, function indices — `ref.func` eligibility and nothing else.
#[test]
fn elem_declarative_with_funcs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e declare func $a))
"#,
    );
}

/// Element-segment flag `0x04`: active at the first table, expressions of nullable `funcref` — the one expression list the format lets go untyped.
#[test]
fn elem_active_at_the_first_table_with_exprs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (table $t i32 2 (ref null func))
            (elem $e (table $t) (offset i32.const 0) (ref null func) (item ref.func $a) (item ref.null func)))
"#,
    );
}

/// Element-segment flag `0x05`: passive, expressions under an explicit element type.
#[test]
fn elem_passive_with_exprs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e passive (ref null $f) (item ref.func $a)))
"#,
    );
}

/// Element-segment flag `0x06`: active with an element type other than `funcref`, which the untyped `0x04` form cannot express even at the first table.
#[test]
fn elem_active_with_typed_exprs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (table $t i32 1 (ref null $f))
            (elem $e (table $t) (offset i32.const 0) (ref null $f) (item ref.func $a)))
"#,
    );
}

/// Element-segment flag `0x07`: declarative, expressions under an explicit element type.
#[test]
fn elem_declarative_with_exprs() {
    round_trips(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e declare (ref null $f) (item ref.func $a)))
"#,
    );
}

#[test]
fn table_instructions() {
    round_trips(
        r#"
        (module $table_instrs
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                table.get $first
                table.set $first
                table.size $first
                drop
                ref.null func
                i32.const 1
                table.grow $first
                drop
                i32.const 0
                ref.null func
                i32.const 1
                table.fill $first
                i32.const 0
                i32.const 0
                i32.const 1
                table.copy $first $second
                i32.const 0
                i32.const 0
                i32.const 1
                table.init $first $e
                elem.drop $e)
            (table $first i32 1 (ref null func))
            (table $second i32 1 (ref null func))
            (elem $e passive func $a))
"#,
    );
}

#[test]
fn indirect_call_instructions() {
    round_trips(
        r#"
        (module $indirect
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                call_indirect $t $f
                i32.const 0
                return_call_indirect $t $f)
            (table $t i32 1 (ref null func)))
"#,
    );
}

#[test]
fn memory_declared_imported_and_exported() {
    round_trips(
        r#"
        (module $memories
            (import "env" "given" (memory $given i32 1 8))
            (memory $own i32 2)
            (export "own" (memory $own)))
"#,
    );
}

/// A 64-bit memory, whose address operands are `i64` rather than `i32`.
#[test]
fn memory_with_sixty_four_bit_addresses() {
    round_trips(
        r#"
        (module $memory64
            (type $f (func))
            (func $a (type $f)
                i64.const 0
                i32.load8_u $m
                drop
                memory.size $m
                drop)
            (memory $m i64 1))
"#,
    );
}

/// Data-segment flag `0x01`: passive, what `memory.init` and `array.new_data` read from.
#[test]
fn data_passive() {
    round_trips(
        r#"
        (module $data
            (data $d passive "\00\01"))
"#,
    );
}

/// Data-segment flag `0x00`: active at the first memory, whose index the encoding leaves implicit.
#[test]
fn data_active_at_the_first_memory() {
    round_trips(
        r#"
        (module $data
            (memory $m i32 1)
            (data $d (memory $m) (offset i32.const 0) "\00\01"))
"#,
    );
}

/// Data-segment flag `0x02`: active at a memory that is not the first, so the memory index is spelled out.
#[test]
fn data_active_at_a_later_memory() {
    round_trips(
        r#"
        (module $data
            (memory $first i32 1)
            (memory $second i32 1)
            (data $d (memory $second) (offset i32.const 0) "\00\01"))
"#,
    );
}

/// Every load and store, each at its natural alignment and zero offset — the immediate the printer writes as nothing but the memory.
#[test]
fn memory_access_instructions() {
    round_trips(
        r#"
        (module $accesses
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                i32.load $m
                drop
                i32.const 0
                i64.load $m
                drop
                i32.const 0
                f32.load $m
                drop
                i32.const 0
                f64.load $m
                drop
                i32.const 0
                i32.load8_s $m
                drop
                i32.const 0
                i32.load8_u $m
                drop
                i32.const 0
                i32.load16_s $m
                drop
                i32.const 0
                i32.load16_u $m
                drop
                i32.const 0
                i64.load8_s $m
                drop
                i32.const 0
                i64.load8_u $m
                drop
                i32.const 0
                i64.load16_s $m
                drop
                i32.const 0
                i64.load16_u $m
                drop
                i32.const 0
                i64.load32_s $m
                drop
                i32.const 0
                i64.load32_u $m
                drop
                i32.const 0
                i32.const 0
                i32.store $m
                i32.const 0
                i64.const 0
                i64.store $m
                i32.const 0
                f32.const 0
                f32.store $m
                i32.const 0
                f64.const 0
                f64.store $m
                i32.const 0
                i32.const 0
                i32.store8 $m
                i32.const 0
                i32.const 0
                i32.store16 $m
                i32.const 0
                i64.const 0
                i64.store8 $m
                i32.const 0
                i64.const 0
                i64.store16 $m
                i32.const 0
                i64.const 0
                i64.store32 $m)
            (memory $m i32 1))
"#,
    );
}

/// A memarg's non-default parts: an offset, an alignment below the natural one, and a memory that is not the first.
#[test]
fn memory_access_immediates() {
    round_trips(
        r#"
        (module $immediates
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                i32.load $first offset=4
                drop
                i32.const 0
                i32.load $first align=1
                drop
                i32.const 0
                i64.load $second offset=8 align=2
                drop)
            (memory $first i32 1)
            (memory $second i32 1))
"#,
    );
}

#[test]
fn bulk_memory_instructions() {
    round_trips(
        r#"
        (module $bulk
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                i32.const 0
                i32.const 1
                memory.copy $first $second
                i32.const 0
                i32.const 0
                i32.const 1
                memory.fill $first
                i32.const 0
                i32.const 0
                i32.const 1
                memory.init $first $d
                data.drop $d)
            (memory $first i32 1)
            (memory $second i32 1)
            (data $d passive "\00"))
"#,
    );
}

#[test]
fn array_data_segment_instructions() {
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
fn array_copy_instruction() {
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
fn array_element_segment_instructions() {
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
