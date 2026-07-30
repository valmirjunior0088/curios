//! Wasm symbol names. Spelled `kind/uniquifier[$hint]`, part of the naming
//! scheme shared with `curios-ersd` and `curios-cont` — see "One naming
//! scheme for compiler identities" in `documentation/DESIGN.md`.

use curios_base::name;

name!(TypeName);
name!(FieldName);
name!(FuncName);
name!(LocalName);
name!(GlobalName);
name!(LabelName);
name!(DataName);
