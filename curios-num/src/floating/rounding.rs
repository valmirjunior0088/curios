//! IEEE 754-2019's rounding-direction attributes (§4.3), which every operation of the model that rounds takes, and the two decisions a direction makes.

use std::cmp::Ordering;

/// Which representable value an inexact result becomes: IEEE 754's five rounding-direction attributes.
///
/// Carried as static data on each rounded intrinsic, as a `Grain` is on a packed one, so a direction is fixed where the operation is written and a fold, a lowering and the emitted code all read the same one. `TiesToEven` is the default the plain operations use (§4.3.3); `TiesToAway` is optional for a binary format and provided because round-to-integer and conversion to an integer name it.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[curios_archive::archived]
pub enum Rounding {
    /// The nearest value, and at a tie the one whose significand is even.
    TiesToEven,
    /// The nearest value, and at a tie the one of greater magnitude.
    TiesToAway,
    /// The nearest value no greater in magnitude.
    TowardZero,
    /// The nearest value no less.
    TowardPositive,
    /// The nearest value no greater.
    TowardNegative,
}

impl Rounding {
    /// Every direction, the default first.
    pub const ALL: [Self; 5] = [
        Self::TiesToEven,
        Self::TiesToAway,
        Self::TowardZero,
        Self::TowardPositive,
        Self::TowardNegative,
    ];

    /// The direction's name in snake case: the `/sys/Flt` module holding the operations rounded this way, and how every printer spells one. The default direction's operations live at `/sys/Flt` itself.
    pub fn label(self) -> &'static str {
        match self {
            Self::TiesToEven => "ties_to_even",
            Self::TiesToAway => "ties_to_away",
            Self::TowardZero => "toward_zero",
            Self::TowardPositive => "toward_positive",
            Self::TowardNegative => "toward_negative",
        }
    }

    /// What rounding to an integral value in this direction is called, the names C and IEEE's §5.3.1 readers know it by.
    pub fn integral_label(self) -> &'static str {
        match self {
            Self::TiesToEven => "nearest",
            Self::TiesToAway => "round",
            Self::TowardZero => "trunc",
            Self::TowardPositive => "ceil",
            Self::TowardNegative => "floor",
        }
    }

    /// Whether a magnitude that lost bits steps up to the next representable one. `dropped` places what was lost against half a unit in the last kept place, `odd` says whether the kept significand is odd, and `inexact` whether anything was lost at all — which the directed modes alone read, since for them any loss decides.
    pub(crate) fn rounds_up(
        self,
        negative: bool,
        dropped: Ordering,
        odd: bool,
        inexact: bool,
    ) -> bool {
        match self {
            Self::TiesToEven => dropped == Ordering::Greater || (dropped == Ordering::Equal && odd),
            Self::TiesToAway => dropped != Ordering::Less,
            Self::TowardZero => false,
            Self::TowardPositive => inexact && !negative,
            Self::TowardNegative => inexact && negative,
        }
    }

    /// Whether a result past the largest finite value becomes the infinity of its sign rather than that largest value (§7.4): always when rounding to nearest, and when directed only toward the infinity of its own sign.
    pub(crate) fn overflows_to_infinity(self, negative: bool) -> bool {
        match self {
            Self::TiesToEven | Self::TiesToAway => true,
            Self::TowardZero => false,
            Self::TowardPositive => !negative,
            Self::TowardNegative => negative,
        }
    }
}
