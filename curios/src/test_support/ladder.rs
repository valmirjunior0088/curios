//! The counts the walk mirrors' claims are stated over, which the ladder's suites assert and the bench's measurement of the mirrors reads.

/// The module-wide counts the family's claims are stated over: arity-4 tuple constructions (the scan shape), arity-2 tuple constructions (the accumulator shape), rope-slice calls, and calls to the mirror's own `mstep`.
pub fn mirror_counts(wat: &str) -> (usize, usize, usize, usize) {
    let lines = |needle: &str| {
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with(needle))
            .count()
    };
    (
        lines("struct.new $tuple/4"),
        lines("struct.new $tuple/2"),
        lines("call $bytes/slice"),
        wat.lines()
            .map(str::trim)
            .filter(|line| line.starts_with("call $func/") && line.contains("mstep"))
            .count(),
    )
}
