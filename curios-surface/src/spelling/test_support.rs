//! What the spelling suites build a shorten map from: the globals a fixture lists, where a render reads each unit's own symbols.

use super::*;

/// [`Shorten`] for a render a reader looks at from inside `own`'s unit, over `predecessors`, the globals around it; a global listed in both is the reader's. `own` empty is a render with no reader's tier.
pub fn build_shorten(own: &[Global], predecessors: &[Global]) -> Shorten {
    let own = Symbols::of(own.iter().copied());
    let around = Symbols::of(
        predecessors
            .iter()
            .copied()
            .filter(|name| !own.contains(name)),
    );

    Shorten::over(Rc::new(own), [Rc::new(around)])
}
