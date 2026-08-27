//! Memories, data segments, and the access instructions and immediates over them.

use super::test_support::*;

#[test]
fn a_memory_is_declared_imported_and_exported() {
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
fn a_memory_with_sixty_four_bit_addresses_round_trips() {
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
fn a_passive_data_segment_round_trips() {
    round_trips(
        r#"
        (module $data
            (data $d passive "\00\01"))
"#,
    );
}

/// Data-segment flag `0x00`: active at the first memory, whose index the encoding leaves implicit.
#[test]
fn a_data_segment_active_at_the_first_memory_round_trips() {
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
fn a_data_segment_active_at_a_later_memory_round_trips() {
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
fn the_memory_access_instructions_round_trip() {
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
fn the_memory_access_immediates_round_trip() {
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
fn the_bulk_memory_instructions_round_trip() {
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
