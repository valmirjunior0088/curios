use super::{FieldName, TypeName};

#[derive(Debug, PartialEq, Clone)]
pub enum NumType {
    I32,
    I64,
    F32,
    F64,
}

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

#[derive(Debug, PartialEq, Clone)]
pub enum HeapType {
    Abstract(AbsHeapType),
    Concrete(TypeName),
}

#[derive(Debug, PartialEq, Clone)]
pub struct RefType {
    pub is_nullable: bool,
    pub heap_type: HeapType,
}

#[derive(Debug, PartialEq, Clone)]
pub enum ValType {
    Num(NumType),
    Ref(RefType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum PackedType {
    I8,
    I16,
}

#[derive(Debug, PartialEq, Clone)]
pub enum StorageType {
    Val(ValType),
    Packed(PackedType),
}

#[derive(Debug, PartialEq, Clone)]
pub enum Mutability {
    Const,
    Var,
}

#[derive(Debug, PartialEq, Clone)]
pub struct FieldType {
    pub storage_type: StorageType,
    pub mutability: Mutability,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ResultType {
    pub val_types: Vec<ValType>,
}

impl ResultType {
    pub fn is_empty(&self) -> bool {
        self.val_types.is_empty()
    }
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

#[derive(Debug, PartialEq, Clone)]
pub struct FuncType {
    pub inputs: ResultType,
    pub outputs: ResultType,
}

impl FuncType {
    pub fn inputs(&self) -> &[ValType] {
        &self.inputs.val_types
    }

    pub fn outputs(&self) -> &[ValType] {
        &self.outputs.val_types
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct ArrayType {
    pub field_type: FieldType,
}

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

impl StructType {
    pub fn field_names(&self) -> impl ExactSizeIterator<Item = &FieldName> {
        self.fields.iter().map(|(field_name, _)| field_name)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum CompType {
    Func(FuncType),
    Array(ArrayType),
    Struct(StructType),
}

#[derive(Debug, PartialEq, Clone)]
pub struct SubType {
    pub is_final: bool,
    pub super_types: Vec<TypeName>,
    pub comp_type: CompType,
}

impl SubType {
    pub fn func_type(&self) -> Option<&FuncType> {
        match &self.comp_type {
            CompType::Func(func_type) => Some(func_type),
            CompType::Array(_) | CompType::Struct(_) => None,
        }
    }

    pub fn struct_type(&self) -> Option<&StructType> {
        match &self.comp_type {
            CompType::Struct(struct_type) => Some(struct_type),
            CompType::Array(_) | CompType::Func(_) => None,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RecType {
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

#[derive(Debug, Clone)]
pub struct GlobalType {
    pub val_type: ValType,
    pub mutability: Mutability,
}

#[derive(Debug, Clone)]
pub enum BlockType {
    Empty,
    Inline(ValType),
    Concrete(TypeName),
}
