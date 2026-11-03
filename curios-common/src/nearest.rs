//! Which name a reader most likely meant by one that names nothing: a distance between two spellings, and the rule that picks a candidate by it.
//!
//! Neutral pieces: a name is text here, and whose names the candidates are is the caller's to say — the bindings in scope where a name failed to resolve, the members of the module a path stopped in.

#[cfg(test)]
mod tests;

/// The restricted Damerau–Levenshtein distance between `this` and `that`: the fewest insertions, deletions, substitutions and swaps of two neighbouring letters that turn one into the other, no letter edited twice. `None` where it is past `limit`.
///
/// A swap counts once because it is one slip of the hand: `coutn` is one edit from `count`, where the plain Levenshtein distance makes it two and a limit of a third of five letters offers nothing.
pub fn edit_distance(this: &str, that: &str, limit: usize) -> Option<usize> {
    let this = this.chars().collect::<Vec<_>>();
    let that = that.chars().collect::<Vec<_>>();
    if this.len().abs_diff(that.len()) > limit {
        return None;
    }

    // The distance to each prefix of `that`, from the prefix of `this` two letters back, from the one a letter back, and from the one being filled.
    let mut before = vec![0; that.len() + 1];
    let mut last = (0..=that.len()).collect::<Vec<_>>();
    let mut row = vec![0; that.len() + 1];
    for i in 1..=this.len() {
        row[0] = i;
        for j in 1..=that.len() {
            let substituted = last[j - 1] + usize::from(this[i - 1] != that[j - 1]);
            let edited = substituted.min(last[j] + 1).min(row[j - 1] + 1);
            let swapped =
                i > 1 && j > 1 && this[i - 1] == that[j - 2] && this[i - 2] == that[j - 1];
            row[j] = match swapped {
                true => edited.min(before[j - 2] + 1),
                false => edited,
            };
        }
        std::mem::swap(&mut before, &mut last);
        std::mem::swap(&mut last, &mut row);
    }

    let distance = last[that.len()];
    (distance <= limit).then_some(distance)
}

/// The name among `candidates` a reader most likely meant by `written`, or `None` where none is near: a candidate that differs from it in case alone, else the closest one within a third of its length by [`edit_distance`], three letters being the least a name counts for.
///
/// rustc's rule, for the reason rustc has it: a third of the name is wide enough for a slip and narrow enough that a short name is not offered every other short name. Ties go to the candidate that sorts first, so the answer is the same whatever order the candidates arrive in. A candidate spelled exactly as `written` is no near miss and is passed over: it is the same name in another namespace.
pub fn nearest<'a>(
    written: &str,
    candidates: impl IntoIterator<Item = &'a str>,
) -> Option<&'a str> {
    let limit = written.chars().count().max(3) / 3;
    let folded = written.to_lowercase();

    let mut cased: Option<&str> = None;
    let mut close: Option<(usize, &str)> = None;
    for candidate in candidates {
        if candidate == written {
            continue;
        }
        if candidate.to_lowercase() == folded {
            cased = Some(cased.map_or(candidate, |held| held.min(candidate)));
        } else if let Some(distance) = edit_distance(written, candidate, limit) {
            let found = (distance, candidate);
            close = Some(close.map_or(found, |held| held.min(found)));
        }
    }

    cased.or(close.map(|(_, candidate)| candidate))
}
