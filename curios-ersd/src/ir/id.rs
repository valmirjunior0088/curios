//! The typed arena identities of the erased representation.
//!
//! Each kind gets its own `u32`-backed newtype with a distinct `~`-sigil
//! display prefix, following the unified IR naming scheme (`~{kind}{index}`,
//! with an optional `$hint` suffix added by the printer). Identities are minted
//! monotonically by their owning arena and never reused; removal tombstones the
//! slot instead.

use curios_base::id;

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
