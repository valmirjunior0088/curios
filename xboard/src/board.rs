//! The board itself: every part of the judgment, each with the tickets filed under it.

mod admission;
use admission::*;

mod conversion;
use conversion::*;

mod elimination;
use elimination::*;

mod formation;
use formation::*;

mod introduction;
use introduction::*;

mod totality;
use totality::*;

use xboard::Part;

pub(super) const BOARD: &[&Part] = &[
    &FORMATION,
    &INTRODUCTION,
    &ELIMINATION,
    &CONVERSION,
    &TOTALITY,
    &ADMISSION,
];
