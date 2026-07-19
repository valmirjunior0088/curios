//! The typed arena identities of the erased representation.
//!
//! Each kind gets its own `u32`-backed newtype with a distinct `~`-sigil
//! display prefix, following the unified IR naming scheme (`~{kind}{index}`,
//! with an optional `$hint` suffix added by the printer). Identities are minted
//! monotonically by their owning arena and never reused; removal tombstones the
//! slot instead.

use curios_base::id;

id!(ValueId, "~v");
id!(FunctionId, "~f");
id!(BlockId, "~b");
id!(StatementId, "~s");
id!(ConstantId, "~c");
id!(ProductId, "~p");
id!(FamilyId, "~d");
id!(ConstructorId, "~t");
id!(ForeignId, "~x");
id!(RecGroupId, "~r");
