//! A term with its solved levels and qualities read through the solver that solved them.

use {
    crate::{Context, UniverseSolver},
    curios_core::{
        Bound, Level, Quality, UniverseError, UniverseMetaId,
        rewrite_universe_levels_scoped_shared, rewrite_universe_qualities,
        substitute_universe_metas_scoped,
    },
    std::{
        cell::RefCell,
        collections::{BTreeMap, BTreeSet},
        convert::Infallible,
        rc::Rc,
    },
};

/// `quality` as `table` has it ([`UniverseSolver::quality_table`]): a metavariable by its index, and anything else as it stands.
fn tabled_quality(table: &[Quality], quality: &Quality) -> Quality {
    match quality {
        Quality::Meta(meta) => table.get(meta.0).copied().unwrap_or(*quality),
        _ => *quality,
    }
}

/// `value` with each sort's quality as far as it is solved, and one nothing has decided as the `Type` it closes as: the form a report shows, to a reader who writes no quality metavariable.
pub fn settle_qualities<B: Bound>(context: &Context, value: &B) -> B {
    if !context.universes().has_qualities() {
        return value.clone();
    }
    let table = context.universes().quality_table(true);
    let settled: Result<B, Infallible> =
        rewrite_universe_qualities(value, move |quality| Ok(tabled_quality(&table, quality)));

    match settled {
        Ok(settled) => settled,
        Err(never) => match never {},
    }
}

/// Replace every solved universe metavariable under `value`'s binders by its recursively zonked solution, shifting each solution past the binders it crosses.
///
/// Lives here rather than beside the scoped-rewrite traversals it calls: those are representation, while [`UniverseSolver`] is elaboration state.
/// The level metavariables `value` holds, read by the walk [`substitute_universe_metas_scoped`] substitutes them by, which visits a node many terms share once.
fn held_universe_metas<B: Bound>(value: &B) -> BTreeSet<UniverseMetaId> {
    let metas = Rc::new(RefCell::new(BTreeSet::new()));
    let found = Rc::clone(&metas);
    let _: Result<_, Infallible> = rewrite_universe_levels_scoped_shared(value, move |_, level| {
        found.borrow_mut().extend(level.metas());
        Ok(level.clone())
    });
    Rc::try_unwrap(metas)
        .expect("the collector releases its traversal closure")
        .into_inner()
}

pub fn zonk_universe_levels_scoped<B: Bound>(
    value: &B,
    solver: &UniverseSolver,
) -> Result<B, UniverseError> {
    // A sort's quality as far as it is solved, read with the levels: one walk where no quality was minted, which is every declaration that leaves no sort to inference.
    let qualities = solver.has_qualities().then(|| solver.quality_table(false));
    // Each solved metavariable `value` holds through the solver's own zonk, which keeps a chain's zonked solution: followed afresh at every occurrence, a chain of levels each solved to the next — a nesting of list literals, each its own `List` — costs its length at each of its links. Read before the substitution, whose closure outlives this borrow of the solver: a copy of the solver for it would cost every level the solver holds at every zonk.
    let mut zonked = BTreeMap::new();
    for meta in held_universe_metas(value) {
        if solver.solution(meta).is_some() {
            zonked.insert(meta, solver.zonk(&Level::meta(meta))?);
        }
    }
    let levelled =
        substitute_universe_metas_scoped(value, move |meta| Ok(zonked.get(&meta).cloned()))?;

    match qualities {
        Some(table) => rewrite_universe_qualities(&levelled, move |quality| {
            Ok(tabled_quality(&table, quality))
        }),
        None => Ok(levelled),
    }
}
