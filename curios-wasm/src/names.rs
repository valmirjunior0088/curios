//! Wasm symbol names. Spelled `kind/uniquifier[$hint]`, part of the naming scheme shared with `curios-ersd` and `curios-cont` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`.

use curios_utilities::name;

name!(TypeName);
name!(FieldName);
name!(FuncName);
name!(LocalName);
name!(GlobalName);
name!(LabelName);
name!(TableName);
name!(ElemName);
name!(MemName);
name!(DataName);
