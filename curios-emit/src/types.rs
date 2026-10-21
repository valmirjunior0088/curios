//! The fixed runtime heap-type shapes every emitted module declares: the data representations (`Flt`, the boxed form of a `Nat` or `Int`, packed binary sequences, `List`, `Cell`) whose structure is program-independent, unlike the per-program rows (`tuple/N`, closures, environments, `func/N`) the emitter derives from the module. Kept in one file so the emitter has one spelling for each shape. curios-js's bridge builder imports [`bytes_sub_type`] and declares the same `$bytes` shape: wasm-GC canonicalizes structural types, so any module declaring the exact shape can exchange byte-payload refs with a compiled program. curios-native's `host_func_type` mirrors the flat payloads — `$bytes`, `$longs`, `$words` and `$elems` — in wasmtime's type universe; keep that end in sync.
//!
//! # The rope representation
//!
//! `Bytes` and `List` are *ropes*: a three-shape tagged union behind the non-final `$rope/bytes` struct base for `Bytes`, and `$rope/list` for lists (both have fields `tag` + `len`). A `leaf` holds a flat payload array (`$bytes` / `$elems`) exactly its run; a `node` holds two children plus a memoization `cache`, which may hold room past the run, and the `$rope/room` record of that room's written extent; a `view` is a window — a `base` rope plus an `offset`. The cost model this buys:
//!
//! - `concat`/`append` are O(1): one `node` allocation, no copying.
//! - `len` is O(1): every shape carries it.
//! - `slice` is O(1) over a flat-available base: one `view` allocation over the source (collapsing a view-of-view, so windows never stack), and none for the empty or the whole window. A `view`'s base is always *flat-available* — a leaf or an already-cached node — so `get` reads straight through a window without forcing or copying. Any other window of an uncached node forces the node first, which memoizes: that slice is the whole-value read below, and pays O(n) in the node however short the window.
//! - `get` on an uncached node reads through one level: the child that holds the element answers where it is flat-available — a leaf, a view, a cached node — and nothing is forced, so an append then a read of the part just appended copies nothing.
//! - The first *whole-value read* (`eql` on equal lengths, `map`, a host call, a nonempty proper `slice` of an uncached node, and a `get` that falls in a child of one that is itself an uncached node) forces the rope — one O(n) fill into a fresh flat payload, memoized in the entry node's `cache` (its children are then nulled, releasing the tree). Later reads are O(1) to reach the payload. (A `view` is not memoized: forcing one is a single window copy of exactly its own size.)
//! - A run read, grown and read again copies only what it grew by. A forcing whose first piece is a view or a flattened node fills a payload with room twice its run, and one whose first piece is a flattened node ending at its payload's written extent, the payload holding the whole run, claims that payload: it writes only the pieces after the first, past the extent, and raises the extent to its own length. So alternating append with a whole-value read, or with a `get` of the older part — which re-forces at every second append, the node above a cached one reading through and the node above that meeting an uncached child — copies each element a bounded number of times over the run's life.
//! - What is left of the hazard: a run handed to a host at every step crosses exact, a copy where its payload holds room; and a run extended two ways at every step copies whole at every step, since a payload's room is one run's to write — as a list that grows and is read through an eager flat build of itself and two more elements does, the build copying what the next append's flattening claims.
//!
//! Naive accumulation loops are therefore O(n) by construction, and so are loops that read what they accumulate and head/tail peel loops (`get` head + `slice` tail): the first peel forces once, every later peel is an O(1) window over the settled payload. There is no compile-time recognition anywhere.
//!
//! The host ABI is untouched by the rope: wire `Bytes` payloads cross the boundary as the flat `$bytes`/`$elems` arrays (params are taken exact before the call, a payload exactly its run, and results are embedded into fresh leaves after it), so curios-native and the curios-js bridge only ever see flat arrays, each read by its own length.

use super::{ChannelData, RopeData, concrete_val};

/// `Flt` — a boxed `f64`: `struct (field $special (f64))`.
pub fn flt_sub_type(special_field: curios_wasm::FieldName) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([(
            special_field,
            curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(
                    curios_wasm::NumType::F64,
                )),
                mutability: curios_wasm::Mutability::Const,
            },
        )])),
    }
}

/// `$bytes` — a `Bytes` rope's flat packed payload, and the wire-`Bytes` host-boundary shape: `array (mut i8)`. Exported so curios-js's bridge declares the identical shape rather than a copy that could drift.
pub fn bytes_sub_type() -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Array(curios_wasm::ArrayType {
            field_type: curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Packed(curios_wasm::PackedType::I8),
                mutability: curios_wasm::Mutability::Var,
            },
        }),
    }
}

/// `$elems` — a `List` rope's flat element payload, and the host-boundary shape: `array (mut <top>)`. The element field stays mutable regardless of cyclicity: payloads are built with `array.new_default` + per-element `array.set`, so it must be writable.
pub fn elems_sub_type(top_type: curios_wasm::ValType) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Array(curios_wasm::ArrayType {
            field_type: curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Val(top_type),
                mutability: curios_wasm::Mutability::Var,
            },
        }),
    }
}

fn i32_const_field() -> curios_wasm::FieldType {
    curios_wasm::FieldType {
        storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(
            curios_wasm::NumType::I32,
        )),
        mutability: curios_wasm::Mutability::Const,
    }
}

fn ref_field(
    type_name: curios_wasm::TypeName,
    is_nullable: bool,
    mutability: curios_wasm::Mutability,
) -> curios_wasm::FieldType {
    curios_wasm::FieldType {
        storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Ref(
            curios_wasm::RefType {
                is_nullable,
                heap_type: curios_wasm::HeapType::Concrete(type_name),
            },
        )),
        mutability,
    }
}

/// `$rope/room` — final: the written extent of a flattened payload that holds room past its run, `struct (field $extent (mut i32))`, in the run's own units. Its payload's slots below the extent are written and never again, and those past it are unwritten; the extent only grows, raised by the one run whose length it is when that run's flattening writes its other pieces into the room. Every flattened node over the payload references the record, so whether a run may write the room is a comparison any of them can make, of its length with the extent.
pub(crate) fn rope_room_sub_type(rope: &RopeData) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([(
            rope.extent_field.clone(),
            curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(
                    curios_wasm::NumType::I32,
                )),
                mutability: curios_wasm::Mutability::Var,
            },
        )])),
    }
}

/// A rope base (`$rope/bytes` / `$rope/list`) — the non-final struct every carrier ref is cast to: `struct (field $tag (i32)) (field $len (i32))`. `tag` is 0 for a leaf, 1 for a node, 2 for a view; `len` is the carrier's element count, so `len` and the tag dispatch never force.
pub(crate) fn rope_base_sub_type(rope: &RopeData) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: false,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (rope.tag_field.clone(), i32_const_field()),
            (rope.len_field.clone(), i32_const_field()),
        ])),
    }
}

/// A rope leaf (`$rope/bytes/leaf` / `$rope/list/leaf`) — final, subtype of the base: adds the flat payload (`$bytes` / `$elems`), exactly the run's length, so a leaf's read can leave its bounds to the engine's trap on the array.
pub(crate) fn rope_leaf_sub_type(rope: &RopeData) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![rope.base.clone()],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (rope.tag_field.clone(), i32_const_field()),
            (rope.len_field.clone(), i32_const_field()),
            (
                rope.payload_field.clone(),
                ref_field(rope.payload.clone(), false, curios_wasm::Mutability::Const),
            ),
        ])),
    }
}

/// A rope node (`$rope/bytes/node` / `$rope/list/node`) — final, subtype of the base: adds two children, the memoization `cache`, and the `room` record of a cache that holds room past the run. All four are mutable and nullable: forcing writes the flat payload into `cache`, its room record into `room` where it has one, and nulls the children, releasing the tree while the memo stays live. A cache is written once, and it holds the run in its first `len` slots.
pub(crate) fn rope_node_sub_type(rope: &RopeData) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![rope.base.clone()],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (rope.tag_field.clone(), i32_const_field()),
            (rope.len_field.clone(), i32_const_field()),
            (
                rope.left_field.clone(),
                ref_field(rope.base.clone(), true, curios_wasm::Mutability::Var),
            ),
            (
                rope.right_field.clone(),
                ref_field(rope.base.clone(), true, curios_wasm::Mutability::Var),
            ),
            (
                rope.cache_field.clone(),
                ref_field(rope.payload.clone(), true, curios_wasm::Mutability::Var),
            ),
            (
                rope.room_field.clone(),
                ref_field(rope.room.clone(), true, curios_wasm::Mutability::Var),
            ),
        ])),
    }
}

/// A rope view (`$rope/bytes/view` / `$rope/list/view`) — final, subtype of the base: a window into a `base` rope starting at `offset`. All fields are immutable; the invariant that makes windows read-through is *representational*: a `view`'s base is always flat-available (a leaf, or a node whose `cache` is already set), enforced by the only constructor, the emitted `slice` helper.
pub(crate) fn rope_view_sub_type(rope: &RopeData) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![rope.base.clone()],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (rope.tag_field.clone(), i32_const_field()),
            (rope.len_field.clone(), i32_const_field()),
            (
                rope.base_field.clone(),
                ref_field(rope.base.clone(), false, curios_wasm::Mutability::Const),
            ),
            (rope.offset_field.clone(), i32_const_field()),
        ])),
    }
}

/// `$words` — `array (mut i32)`: a boxed `Nat` or `Int`'s magnitude in 32-bit limbs, least significant first, and the flat payload a list of `Bool` crosses the host boundary as, one word per element. Mutable because a helper fills the array it allocates in place; no helper ever writes one it was handed.
pub fn words_sub_type() -> curios_wasm::SubType {
    number_array_sub_type(curios_wasm::NumType::I32)
}

/// `$longs` — `array (mut i64)`: the flat payload a list of `Nat` or `Int` crosses the host boundary as, one element per value, each as wide as a lone one crosses. Mutable for the reason `$words` is.
pub fn longs_sub_type() -> curios_wasm::SubType {
    number_array_sub_type(curios_wasm::NumType::I64)
}

fn number_array_sub_type(num_type: curios_wasm::NumType) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Array(curios_wasm::ArrayType {
            field_type: curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(num_type)),
                mutability: curios_wasm::Mutability::Var,
            },
        }),
    }
}

/// `$big` — a `Nat` or `Int` the i31 does not hold, the two carriers sharing one form: `struct (field $sign (i32)) (field $limbs (ref $words))`, the sign `1` for a negative value and `0` otherwise. Final, so telling it apart from every other reference is one exact cast, and canonical: its magnitude is trimmed, never zero, and never inside the i31's range, so a value has one spelling and an i31 and a boxed magnitude are never equal.
pub fn big_sub_type(
    sign_field: curios_wasm::FieldName,
    limbs_field: curios_wasm::FieldName,
    words_type: curios_wasm::TypeName,
) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (sign_field, i32_const_field()),
            (
                limbs_field,
                ref_field(words_type, false, curios_wasm::Mutability::Const),
            ),
        ])),
    }
}

/// A write-once cell: the nullable field starts empty, and `CellOp::Fill` permits its one transition to a non-null payload.
pub fn cell_sub_type(
    special_field: curios_wasm::FieldName,
    top_type: curios_wasm::ValType,
) -> curios_wasm::SubType {
    curios_wasm::SubType {
        is_final: true,
        super_types: vec![],
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([(
            special_field,
            curios_wasm::FieldType {
                storage_type: curios_wasm::StorageType::Val(top_type),
                mutability: curios_wasm::Mutability::Var,
            },
        )])),
    }
}

/// A channel owns its preallocated nullable slots; taking an item clears its slot before returning it.
pub(crate) fn channel_sub_type(data: &ChannelData) -> curios_wasm::SubType {
    let word = curios_wasm::FieldType {
        storage_type: curios_wasm::StorageType::Val(curios_wasm::ValType::Num(
            curios_wasm::NumType::I32,
        )),
        mutability: curios_wasm::Mutability::Var,
    };
    curios_wasm::SubType {
        is_final: true,
        super_types: Vec::new(),
        comp_type: curios_wasm::CompType::Struct(curios_wasm::StructType::from([
            (
                data.payload_field.clone(),
                curios_wasm::FieldType {
                    storage_type: curios_wasm::StorageType::Val(concrete_val(
                        data.payload.clone(),
                        false,
                    )),
                    mutability: curios_wasm::Mutability::Const,
                },
            ),
            (data.head_field.clone(), word.clone()),
            (data.count_field.clone(), word.clone()),
            (data.closed_field.clone(), word),
        ])),
    }
}
