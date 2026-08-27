use {
    super::*,
    curios_utilities::{Source, Span},
    std::{
        collections::hash_map::DefaultHasher,
        hash::{Hash, Hasher},
        rc::Rc,
    },
};

fn leq(lower: Level, upper: Level) -> UniverseConstraint {
    UniverseConstraint {
        lower,
        upper,
        origin: UniverseConstraintOrigin::new(UniverseConstraintKind::Cumulativity),
    }
}

fn param(index: usize) -> Level {
    Level::param(UniverseParam(index))
}

/// Closure is about what a context may name, and it has two halves.
///
/// Moved here with the predicate it covers. It used to live in `curios-cert`, beside a copy of the rule that has since become this method — a test of a transcription, which is what the two checkers deciding closure separately amounted to.
#[test]
fn a_context_names_only_what_it_declares() {
    let within = UniverseContext {
        parameter_count: 2,
        constraints: vec![leq(param(0), param(1))],
    };
    assert!(within.is_closed());

    let escaping = UniverseContext {
        parameter_count: 1,
        constraints: vec![leq(param(3), param(0))],
    };
    assert!(!escaping.is_closed());

    // A metavariable is elaboration residue: a zonked module carries none, so a context that does is not one any checker should interpret.
    let unsolved = UniverseContext {
        parameter_count: 1,
        constraints: vec![leq(Level::meta(UniverseMetaId(0)), param(0))],
    };
    assert!(!unsolved.is_closed());
}

fn hash(value: &impl Hash) -> u64 {
    let mut hasher = DefaultHasher::new();
    value.hash(&mut hasher);
    hasher.finish()
}

fn origin(label: &str) -> UniverseConstraintOrigin {
    UniverseConstraintOrigin::new(UniverseConstraintKind::Other(label.into()))
}

/// [`Level::substitute`] accumulates into its result instead of building one part per atom, and the arm that made that worth doing is the *unreplaced* one — where the part is the atom back again.
///
/// Both halves of that equivalence are asserted here, because a later edit could break either and the corpus would not notice: replacing nothing is the identity on a level carrying a constant and several offset atoms, and a replacement whose own constant exceeds the level's still raises it. The third case is the one normalization decides — an atom offset reaching the constant zeroes it — which is what makes the unreplaced arm contribute nothing to the constant rather than contributing `offset`.
#[test]
fn substituting_nothing_is_the_identity_and_a_replacement_still_raises_the_constant() {
    let u = LevelHead::Meta(UniverseMetaId(0));
    let v = LevelHead::Meta(UniverseMetaId(1));
    let level = Level::max([Level::constant(9), Level::atom(u, 2), Level::atom(v, 5)]);

    assert_eq!(level.substitute(|_| None).unwrap(), level);

    let raised = level
        .substitute(|head| (head == u).then(|| Level::constant(20)))
        .unwrap();
    assert_eq!(
        raised.constant_part(),
        22,
        "the replacement carries the atom's own offset"
    );
    assert_eq!(
        raised,
        Level::max([Level::constant(22), Level::atom(v, 5)]),
        "the untouched atom survives and the replaced head is gone"
    );

    // Normalization is what keeps the unreplaced arm from contributing its offset as a constant: an atom reaching the constant zeroes it, so `{0, {head → offset}}` is the whole part.
    let reached = Level::max([Level::constant(2), Level::atom(u, 2)]);
    assert_eq!(reached.constant_part(), 0);
    assert_eq!(reached.substitute(|_| None).unwrap(), reached);
}

#[test]
fn level_max_is_canonical() {
    let u = Level::meta(UniverseMetaId(0));
    let v = Level::meta(UniverseMetaId(1));
    let left = Level::max([
        Level::zero(),
        u.clone(),
        v.succ().unwrap(),
        u.checked_add(3).unwrap(),
    ]);
    let right = Level::max([
        u.checked_add(3).unwrap(),
        Level::max([v.succ().unwrap(), u]),
    ]);
    assert_eq!(left, right);
    assert_eq!(hash(&left), hash(&right));
    assert_eq!(left.to_string(), "max(?u0+3,?u1+1)");
}

#[test]
fn successor_distributes_and_overflow_is_checked() {
    let level = Level::max([
        Level::constant(2),
        Level::param(UniverseParam(0)).checked_add(4).unwrap(),
    ]);
    assert_eq!(level.checked_add(3).unwrap().to_string(), "u+7");
    assert_eq!(
        Level::constant(u32::MAX).succ(),
        Err(UniverseError::OffsetOverflow)
    );
}

#[test]
fn constraint_identity_ignores_diagnostic_provenance() {
    let semantic = || UniverseConstraint {
        lower: Level::param(UniverseParam(0)),
        upper: Level::param(UniverseParam(1)),
        origin: origin("first"),
    };
    let left = semantic();
    let mut right = semantic();
    right.origin = origin("second");
    right.origin.span = Some(Span {
        source: Rc::new(Source {
            path: None,
            text: "Type".into(),
        }),
        start: 0,
        end: 4,
    });

    assert_eq!(left, right);
    assert_eq!(hash(&left), hash(&right));
}

#[test]
fn level_parameter_names_stay_alphabetic_past_the_sixth() {
    let name = |index: usize| Level::param(UniverseParam(index)).to_string();
    assert_eq!(name(0), "u");
    assert_eq!(name(5), "z");
    // `u + 6` is `{` in ASCII; a stepping scheme that runs off `z` prints punctuation where a level name belongs.
    assert_eq!(name(6), "u1");
    assert_eq!(name(8), "w1");
    assert_eq!(name(12), "u2");
    for index in 0..64 {
        assert!(
            name(index).chars().all(|c| c.is_ascii_alphanumeric()),
            "level parameter {index} printed as {}",
            name(index)
        );
    }
}
