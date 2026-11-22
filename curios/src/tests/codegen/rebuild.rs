//! Rebuilding in place: `/std/Map/insert` rebuilding its node at every level, what looks fresh and is shared rebuilding nothing, and every program printing one answer whether it rebuilds or not.

mod test_support;

mod map;
mod shared;
