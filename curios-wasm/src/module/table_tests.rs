//! A module round-trips, and every table and element-segment form survives it.

use super::test_support::*;

#[test]
fn a_module_of_every_section_round_trips() {
    round_trips(
        r#"
        (module $a_module_of_every_section_round_trips
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
fn a_table_is_declared_imported_and_exported() {
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
fn a_table_with_an_initializer_round_trips() {
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
fn a_table_with_sixty_four_bit_addresses_round_trips() {
    round_trips(
        r#"
        (module $table64
            (table $t i64 1 (ref null func)))
"#,
    );
}

/// Element-segment flag `0x00`: active at the first table, function indices.
#[test]
fn an_element_segment_active_at_the_first_table_round_trips_with_funcs() {
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
fn a_passive_element_segment_round_trips_with_funcs() {
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
fn an_element_segment_active_at_a_later_table_round_trips_with_funcs() {
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
fn a_declarative_element_segment_round_trips_with_funcs() {
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
fn an_element_segment_active_at_the_first_table_round_trips_with_exprs() {
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
fn a_passive_element_segment_round_trips_with_exprs() {
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
fn an_active_element_segment_round_trips_with_typed_exprs() {
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
fn a_declarative_element_segment_round_trips_with_exprs() {
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
fn the_table_instructions_round_trip() {
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
fn the_indirect_call_instructions_round_trip() {
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
