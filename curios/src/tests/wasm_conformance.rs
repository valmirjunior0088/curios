//! `curios-wasm`'s memory, table, and segment surface, checked against the engine that has to accept it.
//!
//! These probes live here rather than in `curios-wasm` because that crate must not name wasmtime — the pin is `curios-runtime`'s single row — and `curios_runtime::validate` is the only thing that reads an encoded module back. Until a binary reader exists (`Stage::WasmOptm`'s specification owns it), acceptance by the engine *is* the binary-side check: a flag byte, a limits header, or a memarg written wrong is a validation failure here rather than a silent miscompile later.
//!
//! Each module is written in the crate's own WAT dialect and parsed, so one source string exercises the parser, the printer's inverse, and the encoder at once.

use {
    curios_runtime::validate,
    curios_wasm::{Module, to_bytes},
};

fn validates(source: &str) {
    let module = source.parse::<Module>().expect("expected a module");

    if let Err(error) = validate(&to_bytes(&module)) {
        panic!("expected a valid module: {error}");
    }
}

#[test]
fn tables_are_declared_imported_and_exported() {
    validates(
        r#"
        (module $tables
            (import "env" "given" (table $given i32 1 8 (ref null func)))
            (table $own i32 2 (ref null func))
            (export "own" (table $own)))
"#,
    );
}

/// A non-defaultable element type has no default slot value, so the table must carry an initializer — the function-references form of the table section.
#[test]
fn a_table_carries_an_initializer() {
    validates(
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

/// A 64-bit table, which memory64 admits beside a 64-bit memory: `table.size` answers `i64` there.
#[test]
fn a_table_is_addressed_by_sixty_four_bits() {
    validates(
        r#"
        (module $table64
            (type $f (func (result i64)))
            (func $a (type $f)
                table.size $t)
            (table $t i64 1 (ref null func)))
"#,
    );
}

/// Element-segment flag `0x00`: active at the first table, function indices, no element type spelled.
#[test]
fn elem_flag_00_is_accepted() {
    validates(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (table $t i32 1 (ref null func))
            (elem $e (table $t) (offset i32.const 0) func $a))
"#,
    );
}

/// Element-segment flag `0x01`: passive, function indices under an element kind.
#[test]
fn elem_flag_01_is_accepted() {
    validates(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e passive func $a))
"#,
    );
}

/// Element-segment flag `0x02`: active at a table that is not the first, so the table index is written out.
#[test]
fn elem_flag_02_is_accepted() {
    validates(
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

/// Element-segment flag `0x03`: declarative, function indices — what makes `ref.func` in a function body legal.
#[test]
fn elem_flag_03_is_accepted() {
    validates(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f)
                ref.func $a
                drop)
            (elem $e declare func $a))
"#,
    );
}

/// Element-segment flag `0x04`: active at the first table, expressions of nullable `funcref` — the untyped expression form.
#[test]
fn elem_flag_04_is_accepted() {
    validates(
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
fn elem_flag_05_is_accepted() {
    validates(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f))
            (elem $e passive (ref null $f) (item ref.func $a)))
"#,
    );
}

/// Element-segment flag `0x06`: active with an element type other than `funcref`, which flag `0x04` cannot express even at the first table.
#[test]
fn elem_flag_06_is_accepted() {
    validates(
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
fn elem_flag_07_is_accepted() {
    validates(
        r#"
        (module $elem
            (type $f (func))
            (func $a (type $f)
                ref.func $a
                drop)
            (elem $e declare (ref null $f) (item ref.func $a)))
"#,
    );
}

#[test]
fn the_table_instructions_are_accepted() {
    validates(
        r#"
        (module $table_instrs
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                table.get $first
                drop
                i32.const 0
                ref.null func
                table.set $first
                table.size $first
                drop
                ref.null func
                i32.const 1
                table.grow $first
                drop
                i32.const 0
                ref.null func
                i32.const 0
                table.fill $first
                i32.const 0
                i32.const 0
                i32.const 0
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
fn the_indirect_calls_are_accepted() {
    validates(
        r#"
        (module $indirect
            (type $f (func))
            (func $a (type $f)
                i32.const 0
                call_indirect $t $f)
            (func $b (type $f)
                i32.const 0
                return_call_indirect $t $f)
            (table $t i32 1 (ref null func)))
"#,
    );
}

#[test]
fn the_element_segment_array_operations_are_accepted() {
    validates(
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

#[test]
fn memories_are_declared_imported_and_exported() {
    validates(
        r#"
        (module $memories
            (import "env" "given" (memory $given i32 1 8))
            (memory $own i32 2)
            (export "own" (memory $own)))
"#,
    );
}

/// A 64-bit memory: its address operands are `i64`, and so is what `memory.size` answers.
#[test]
fn a_memory_is_addressed_by_sixty_four_bits() {
    validates(
        r#"
        (module $memory64
            (type $f (func (result i64)))
            (func $a (type $f)
                i64.const 0
                i32.load8_u $m
                drop
                memory.size $m)
            (memory $m i64 1))
"#,
    );
}

#[test]
fn the_memory_access_family_is_accepted() {
    validates(
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

/// A memarg's three parts: an offset, an alignment below the access width's natural one, and a memory that is not the first — the case that sets the alignment field's memory-index bit.
#[test]
fn a_memarg_reaches_a_later_memory_at_an_offset() {
    validates(
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
fn the_bulk_memory_instructions_are_accepted() {
    validates(
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

/// Data-segment flag `0x01`: passive, and needing no memory at all.
#[test]
fn data_flag_01_is_accepted() {
    validates(
        r#"
        (module $data
            (data $d passive "\00\01"))
"#,
    );
}

/// Data-segment flag `0x00`: active at the first memory, whose index the encoding leaves implicit.
#[test]
fn data_flag_00_is_accepted() {
    validates(
        r#"
        (module $data
            (memory $m i32 1)
            (data $d (memory $m) (offset i32.const 0) "\00\01"))
"#,
    );
}

/// Data-segment flag `0x02`: active at a memory that is not the first, so the memory index is written out.
#[test]
fn data_flag_02_is_accepted() {
    validates(
        r#"
        (module $data
            (memory $first i32 1)
            (memory $second i32 1)
            (data $d (memory $second) (offset i32.const 0) "\00\01"))
"#,
    );
}

#[test]
fn the_data_segment_array_operations_are_accepted() {
    validates(
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
