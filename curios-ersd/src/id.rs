//! The typed arena identities of the erased representation.
//!
//! Each kind gets its own `u32`-backed newtype with a distinct `~`-sigil display prefix, following the naming scheme shared with `curios-cont` and `curios-wasm` — see `documentation/design/toolchain/one-naming-scheme-for-compiler-identities.md`. Identities are minted monotonically by their owning arena and never reused; removal tombstones the slot instead.

use curios_utilities::id;

id!(ValueId, "~v"; archive);
id!(FunctionId, "~f"; archive);
id!(BlockId, "~b"; archive);
id!(StatementId, "~s"; archive);
id!(ConstantId, "~c"; archive);
id!(ProductId, "~p"; archive);
id!(FamilyId, "~d"; archive);
id!(ConstructorId, "~t"; archive);
id!(ForeignId, "~x"; archive);
id!(RecGroupId, "~r"; archive);
