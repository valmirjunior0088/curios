use super::{FieldName, TypeName};

/// The four wasm numeric value types.
#[derive(Debug, PartialEq, Clone)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

/// The abstract heap types of wasm-GC: the bottom types (`NoFunc`/`NoExtern`/`None`), the top types (`Func`/`Extern`/`Any`), and the internal hierarchy under `Any` (`Eq`, `I31`, `Struct`, `Array`).
#[derive(Debug, PartialEq, Clone)]
pub enum AbsHeapType {
    NoFunc,
    NoExtern,
    None,
    Func,
    Extern,
    Any,
    Eq,
    I31,
    Struct,
    Array,
}

/// What a reference points into: one of the spec's abstract heap types, or a concrete declared type referenced by name — the encoder resolves the name to a type index.
#[derive(Debug, PartialEq, Clone)]
pub enum HeapType {
    Abstract(AbsHeapType),
    Concrete(TypeName),
}

/// A reference type: a heap type plus whether `null` inhabits it.
#[derive(Debug, PartialEq, Clone)]
pub struct RefType {
    pub is_nullable: bool,
    pub heap_type: HeapType,
}

/// A wasm value type: numeric or reference. There is no `v128` variant — the backend never emits SIMD.
#[derive(Debug, PartialEq, Clone)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

/// The sub-word storage widths for struct fields and array elements; values widen to `i32` on access through the signed/unsigned `get` instruction variants.
#[derive(Debug, PartialEq, Clone)]
pub enum PackedType {
    I8,
    I16,
}

/// What a field slot holds: a full value type, or a packed integer width that exists only inside structs and arrays.
#[derive(Debug, PartialEq, Clone)]
pub enum StorageType {
    Val(ValType),
    Packed(PackedType),
}

/// Whether a field or global admits mutation (`Var`) or is fixed after initialization (`Const`).
#[derive(Debug, PartialEq, Clone)]
pub enum Mutability {
    Const,
    Var,
}

/// The type of a struct field or array element: a storage type plus its mutability.
#[derive(Debug, PartialEq, Clone)]
pub struct FieldType {
    pub storage_type: StorageType,
    pub mutability: Mutability,
}

/// A sequence of value types forming one side of a function signature.
#[derive(Debug, PartialEq, Clone)]
pub struct ResultType {
    pub val_types: Vec<ValType>,
}

impl<I> From<I> for ResultType
where
    I: IntoIterator<Item = ValType>,
{
    fn from(val_types: I) -> Self {
        Self {
            val_types: val_types.into_iter().collect(),
        }
    }
}

/// A function signature: parameter and result type vectors. Declared as a [`CompType`] like any other composite, so calls can reference it by type name.
#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
    pub inputs: ResultType,
    pub outputs: ResultType,
}

impl FuncType {
    /// The parameter types, as a slice.
    pub fn inputs(&self) -> &[ValType] {
        &self.inputs.val_types
    }

    /// The result types, as a slice.
    pub fn outputs(&self) -> &[ValType] {
        &self.outputs.val_types
    }
}

/// An array composite type: one field type shared by every element.
#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub field_type: FieldType,
}

/// A struct composite type with named fields. Field names are this crate's own layer: `struct.get`/`struct.set` instructions refer to fields by name, the encoder resolves them to positional indices, and the names survive into the emitted name section for debug tooling.
#[derive(Debug, PartialEq, Clone)]
pub struct StructType {
    pub fields: Vec<(FieldName, FieldType)>,
}

impl<I> From<I> for StructType
where
    I: IntoIterator<Item = (FieldName, FieldType)>,
{
    fn from(fields: I) -> Self {
        Self {
            fields: fields.into_iter().collect(),
        }
    }
}

/// The composite shape of a declared type — function signature, array, or struct: the three kinds of type definition wasm-GC admits.
#[derive(Debug, PartialEq, Clone)]
pub enum CompType {
    Func(FuncType),
    Array(ArrayType),
    Struct(StructType),
}

/// One type declaration within a recursion group: its composite shape plus the subtyping header — the named supertypes it extends and whether further subtyping is closed off (`is_final`). The emitter relies on non-final struct subtyping to give its value representations a common upcastable shape.
#[derive(Debug, PartialEq, Clone)]
pub struct SubType {
    pub is_final: bool,
    pub super_types: Vec<TypeName>,
    pub comp_type: CompType,
}

impl SubType {
    pub(crate) fn func_type(&self) -> Option<&FuncType> {
        match &self.comp_type {
            CompType::Func(func_type) => Some(func_type),
            CompType::Array(_) | CompType::Struct(_) => None,
        }
    }

    pub(crate) fn struct_type(&self) -> Option<&StructType> {
        match &self.comp_type {
            CompType::Struct(struct_type) => Some(struct_type),
            CompType::Array(_) | CompType::Func(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct RecType {
    pub sub_types: Vec<(TypeName, SubType)>,
}

impl<I> From<I> for RecType
where
    I: IntoIterator<Item = (TypeName, SubType)>,
{
    fn from(sub_types: I) -> Self {
        Self {
            sub_types: sub_types.into_iter().collect(),
        }
    }
}

/// The type of a global: its value type plus whether `global.set` is permitted after initialization.
#[derive(Debug, Clone)]
pub struct GlobalType {
    pub val_type: ValType,
    pub mutability: Mutability,
}

/// The signature of a `block`/`loop`/`if`: `Empty` and `Inline` cover the parameterless cases the binary format expresses directly, while `Concrete` names a declared func type for blocks that take parameters or produce multiple results.
#[derive(Debug, Clone)]
pub enum BlockType {
    Empty,
    Inline(ValType),
    Concrete(TypeName),
}
