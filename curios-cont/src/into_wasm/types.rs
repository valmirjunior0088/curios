//! The fixed runtime heap-type shapes every emitted module declares: the data
//! representations (`Flt`, packed binary sequences, `Lst`, `Cell`) whose structure is
//! program-independent, unlike the per-program families (`tpl/N`, closures,
//! environments, `func/N`) the emitter derives from the module. Kept in one
//! file so the emitter has one spelling for each shape. curios-web's bridge
//! builder declares its own structurally identical `$bytes` type: wasm-GC
//! canonicalizes structural types, so any module declaring the exact shape
//! can exchange byte-payload refs with a compiled program. curios-runtime's
//! `host_func_type` mirrors `$bytes` and `$elems` in wasmtime's type
//! universe — keep the two ends in sync.
//!
//! # The rope representation
//!
//! `Bits`, `Bytes`, and `Lst` are *ropes*: the two packed grains share one
//! three-shape tagged union behind the non-final `$rope/bin` struct base, while
//! lists use `$rope/lst` (both have fields `tag` + `len`). A `leaf` holds a flat
//! payload array (`$bytes` / `$elems`); a `node` holds two children plus a
//! memoization `cache`; a `view` is a window — a `base` rope plus an `offset`.
//! The cost model this buys:
//!
//! - `concat`/`append` are O(1): one `node` allocation, no copying.
//! - `len` is O(1): every shape carries it.
//! - `slice` is O(1): one `view` allocation over the source (collapsing a
//!   view-of-view, so windows never stack). A `view`'s base is always
//!   *flat-available* — a leaf or an already-cached node (slicing an uncached
//!   node forces it first, which memoizes) — so `get` reads straight through
//!   a window without forcing or copying.
//! - The first *whole-value read* (`eql` on equal lengths, `map`, a host
//!   call) forces the rope — one O(n) fill into a fresh flat payload, memoized
//!   in the entry node's `cache` (its children are then nulled, releasing the
//!   tree). Later reads are O(1) to reach the payload. (A `view` is not
//!   memoized: forcing one is a single window copy of exactly its own size.)
//! - The documented hazard: *alternating* append and whole-value reads
//!   re-forces per read (the new node above a cached one is uncached), which
//!   is quadratic. Build fully, then read.
//!
//! Naive accumulation loops are therefore O(n) by construction, and so are
//! head/tail peel loops (`get` head + `slice` tail): the first peel forces
//! once, every later peel is an O(1) window over the settled payload. There
//! is no compile-time recognition anywhere.
//!
//! The host ABI is untouched by the rope: wire `Bin` payloads cross the boundary as the
//! flat `$bytes`/`$elems` arrays (params are forced before the call, results
//! are embedded into fresh leaves after it), so curios-runtime and the curios-web
//! bridge only ever see flat arrays.

use curios_wasm::{
    ArrayType, CompType, FieldName, FieldType, HeapType, Mutability, NumType, PackedType, RefType,
    StorageType, StructType, SubType, TypeName, ValType,
};

/// `Flt` — a boxed `f32`: `struct (field $special (f32))`.
pub(crate) fn flt_sub_type(special_field: FieldName) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Struct(StructType::from([(
            special_field,
            FieldType {
                storage_type: StorageType::Val(ValType::Num(NumType::F32)),
                mutability: Mutability::Const,
            },
        )])),
    }
}

/// `$bytes` — a `Bits`/`Bytes` rope's flat packed payload, and the wire-`Bin` host-boundary shape:
/// `array (mut i8)`.
pub(crate) fn bytes_sub_type() -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Array(ArrayType {
            field_type: FieldType {
                storage_type: StorageType::Packed(PackedType::I8),
                mutability: Mutability::Var,
            },
        }),
    }
}

/// `$elems` — an `Lst` rope's flat element payload, and the host-boundary
/// shape: `array (mut <top>)`. The element field stays mutable regardless of
/// cyclicity: payloads are built with `array.new_default` + per-element
/// `array.set`, so it must be writable.
pub(crate) fn elems_sub_type(top_type: ValType) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Array(ArrayType {
            field_type: FieldType {
                storage_type: StorageType::Val(top_type),
                mutability: Mutability::Var,
            },
        }),
    }
}

fn i32_const_field() -> FieldType {
    FieldType {
        storage_type: StorageType::Val(ValType::Num(NumType::I32)),
        mutability: Mutability::Const,
    }
}

fn ref_field(type_name: TypeName, is_nullable: bool, mutability: Mutability) -> FieldType {
    FieldType {
        storage_type: StorageType::Val(ValType::Ref(RefType {
            is_nullable,
            heap_type: HeapType::Concrete(type_name),
        })),
        mutability,
    }
}

/// A rope base (`$rope/bin` / `$rope/lst`) — the non-final struct every carrier ref is
/// cast to: `struct (field $tag (i32)) (field $len (i32))`. `tag` is 0 for a
/// leaf, 1 for a node, 2 for a view; `len` is the carrier's element count, so
/// `len` and the tag dispatch never force.
pub(crate) fn rope_base_sub_type(tag_field: FieldName, len_field: FieldName) -> SubType {
    SubType {
        is_final: false,
        super_types: vec![],
        comp_type: CompType::Struct(StructType::from([
            (tag_field, i32_const_field()),
            (len_field, i32_const_field()),
        ])),
    }
}

/// A rope leaf (`$rope/bin/leaf` / `$rope/lst/leaf`) — final, subtype of the base: adds
/// the flat payload (`$bytes` / `$elems`).
pub(crate) fn rope_leaf_sub_type(
    base_type: TypeName,
    tag_field: FieldName,
    len_field: FieldName,
    payload_field: FieldName,
    payload_type: TypeName,
) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![base_type],
        comp_type: CompType::Struct(StructType::from([
            (tag_field, i32_const_field()),
            (len_field, i32_const_field()),
            (
                payload_field,
                ref_field(payload_type, false, Mutability::Const),
            ),
        ])),
    }
}

/// A rope node (`$rope/bin/node` / `$rope/lst/node`) — final, subtype of the base: adds
/// two children and the memoization `cache`. All three are mutable and
/// nullable: forcing writes the flat payload into `cache` and nulls the
/// children, releasing the tree while the memo stays live.
pub(crate) fn rope_node_sub_type(
    base_type: TypeName,
    tag_field: FieldName,
    len_field: FieldName,
    left_field: FieldName,
    right_field: FieldName,
    cache_field: FieldName,
    payload_type: TypeName,
) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![base_type.clone()],
        comp_type: CompType::Struct(StructType::from([
            (tag_field, i32_const_field()),
            (len_field, i32_const_field()),
            (
                left_field,
                ref_field(base_type.clone(), true, Mutability::Var),
            ),
            (right_field, ref_field(base_type, true, Mutability::Var)),
            (cache_field, ref_field(payload_type, true, Mutability::Var)),
        ])),
    }
}

/// A rope view (`$rope/bin/view` / `$rope/lst/view`) — final, subtype of the base: a
/// window into a `base` rope starting at `offset`. All fields are immutable;
/// the invariant that makes windows read-through is *representational*: a
/// `view`'s base is always flat-available (a leaf, or a node whose `cache` is
/// already set), enforced by the only constructor, the emitted `slice`
/// helper.
pub(crate) fn rope_view_sub_type(
    base_type: TypeName,
    tag_field: FieldName,
    len_field: FieldName,
    base_field: FieldName,
    offset_field: FieldName,
) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![base_type.clone()],
        comp_type: CompType::Struct(StructType::from([
            (tag_field, i32_const_field()),
            (len_field, i32_const_field()),
            (base_field, ref_field(base_type, false, Mutability::Const)),
            (offset_field, i32_const_field()),
        ])),
    }
}

/// `Cell` — a mutable reference cell: `struct (field $special (mut <top>))`.
pub(crate) fn cell_sub_type(special_field: FieldName, top_type: ValType) -> SubType {
    SubType {
        is_final: true,
        super_types: vec![],
        comp_type: CompType::Struct(StructType::from([(
            special_field,
            FieldType {
                storage_type: StorageType::Val(top_type),
                mutability: Mutability::Var,
            },
        )])),
    }
}
