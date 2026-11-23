//! Wasm symbol names. Spelled `kind/uniquifier[$hint]`, part of the naming scheme shared with `curios-anf` and `curios-cps` — see `documentation/design/tooling/a-printer-states-each-fact-once-where-it-is-bound.md`.

use curios_common::name;

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
