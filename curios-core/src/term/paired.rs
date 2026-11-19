//! Two eliminations of one scrutinee, case by case: what each takes at a case, its own arm or its catch-all, which is what both checkers compare two stuck matches by.
//!
//! A catch-all stands for every case its own arms do not write, so at a case one side writes an arm for and the other does not, the arm is compared with the other's catch-all, and where a case neither writes is left, the two catch-alls with each other. Each side's arm for a case is the first it writes, the one reduction takes; one written after it for the same case is read by nothing.

use crate::{Keys, Term};

/// One comparison two eliminations of one scrutinee owe at a case: the two arms they write for it, an arm and the catch-all the other takes there, or their two catch-alls.
pub enum Paired<'c, K, A> {
    Arms(&'c K, &'c A, &'c A),
    ArmAndDefault(&'c K, &'c A, &'c Term),
    DefaultAndArm(&'c K, &'c Term, &'c A),
    Defaults(&'c Term, &'c Term),
}

impl<'c, K> Paired<'c, K, Term> {
    /// The two bodies compared, where an arm binds nothing.
    pub fn bodies(&self) -> (&'c Term, &'c Term) {
        match *self {
            Paired::Arms(_, this, that)
            | Paired::ArmAndDefault(_, this, that)
            | Paired::DefaultAndArm(_, this, that)
            | Paired::Defaults(this, that) => (this, that),
        }
    }
}

/// The comparisons two eliminations of one scrutinee owe, each side given as its arms by case and its catch-all, or `None` where the two are apart: a case one side writes an arm for, or one its catch-all stands for, that the other has neither for. `cases` is how many cases the scrutinee's type has, `None` where they are unbounded or unknown; it is asked only where a catch-all stands, for whether a case neither side writes is left.
pub fn paired<'c, K: PartialEq, A>(
    this: (&'c [(K, A)], Option<&'c Term>),
    that: (&'c [(K, A)], Option<&'c Term>),
    cases: impl FnOnce() -> Option<usize>,
) -> Option<Vec<Paired<'c, K, A>>> {
    let ((this_arms, this_default), (that_arms, that_default)) = (this, that);
    let taken = |arms: &[(K, A)], at: usize| arms[..at].iter().all(|(case, _)| *case != arms[at].0);
    let finds = |arms: &'c [(K, A)], case: &K| arms.iter().find(|(other, _)| other == case);

    let mut paired = Vec::with_capacity(this_arms.len().max(that_arms.len()) + 1);
    // How many cases some side writes an arm for, each counted once.
    let mut written = 0;
    for (at, (case, this_arm)) in this_arms.iter().enumerate() {
        if !taken(this_arms, at) {
            continue;
        }
        written += 1;
        paired.push(match finds(that_arms, case) {
            Some((_, that_arm)) => Paired::Arms(case, this_arm, that_arm),
            None => Paired::ArmAndDefault(case, this_arm, that_default?),
        });
    }
    for (at, (case, that_arm)) in that_arms.iter().enumerate() {
        if !taken(that_arms, at) || finds(this_arms, case).is_some() {
            continue;
        }
        written += 1;
        paired.push(Paired::DefaultAndArm(case, this_default?, that_arm));
    }

    // A case no side writes an arm for is one each catch-all stands for, and one a side with none has no arm for.
    if this_default.is_some() || that_default.is_some() {
        let left = cases().is_none_or(|cases| written < cases);
        match (this_default, that_default) {
            (Some(this), Some(that)) if left => paired.push(Paired::Defaults(this, that)),
            (Some(_), None) | (None, Some(_)) if left => return None,
            _ => {}
        }
    }

    Some(paired)
}

/// The comparisons two dispatches of one scrutinee owe, each given as its keys and its catch-all, as the two bodies compared at each case ([`paired`]); `None` where the two are apart, as two of different carriers are.
pub fn keys_paired<'c>(
    this: (&'c Keys, &'c Term),
    that: (&'c Keys, &'c Term),
) -> Option<Vec<(&'c Term, &'c Term)>> {
    let ((this_keys, this_default), (that_keys, that_default)) = (this, that);
    match (this_keys, that_keys) {
        (Keys::Nat(this_keys), Keys::Nat(that_keys)) => paired(
            (this_keys.arms(), Some(this_default)),
            (that_keys.arms(), Some(that_default)),
            || None,
        )
        .map(|compared| compared.iter().map(Paired::bodies).collect()),
        (Keys::Int(this_keys), Keys::Int(that_keys)) => paired(
            (this_keys.arms(), Some(this_default)),
            (that_keys.arms(), Some(that_default)),
            || None,
        )
        .map(|compared| compared.iter().map(Paired::bodies).collect()),
        (Keys::Nat(_), Keys::Int(_)) | (Keys::Int(_), Keys::Nat(_)) => None,
    }
}
