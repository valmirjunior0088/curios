use {
    super::{Parser, ParserError, ParserState},
    curios_utilities::Source,
    std::{any::Any, cell::RefCell, collections::HashMap, rc::Rc},
};

/// One cached parse at a given grammar key and start offset: either the produced value (type-erased, since the table is shared across the memoized parsers) paired with the end offset to resume at, or the verbatim error the parser failed with.
type MemoEntry = Result<(Rc<dyn Any>, usize), ParserError>;

thread_local! {
    /// Packrat cache for [`memoize`]d parsers, keyed by `(grammar key, byte offset)`. Cleared at the start of every [`run_parser`] so offsets never collide across independent parses. Per-thread, so concurrent parses (e.g. the test suite) don't share it.
    static MEMO: RefCell<HashMap<(u32, usize), MemoEntry>> = RefCell::new(HashMap::new());
}

/// Wraps a parser so its result at each start offset is computed once and reused. This is what makes the term grammar linear instead of exponential: the same position is probed by several overlapping alternatives (a `(` is tried as a dependent function type, then a non-dependent one, then a lambda, then parens), and without memoization each retry re-parses the whole nested subterm.
///
/// Sound as straight packrat because the wrapped parsers (`parse_term`, `parse_atomic_term`) are pure functions of the offset — parsing carries no symbol table or other context that could make the same input parse differently. `key` distinguishes the grammar nonterminals that share the table. The wrapped parser must never re-enter itself at the *same* offset without consuming input (no left recursion), which the term grammar satisfies.
pub fn memoize<'a, A>(key: u32, parser: Parser<'a, A>) -> Parser<'a, A>
where
    A: Clone + 'static,
{
    Parser::new(move |state| {
        let offset = state.offset;

        if let Some(entry) = MEMO.with(|memo| memo.borrow().get(&(key, offset)).cloned()) {
            return match entry {
                Ok((value, end)) => {
                    let value = value
                        .downcast_ref::<A>()
                        .expect("memoized parser reused a key for a different type")
                        .clone();

                    Ok((value, state.jump_to(end)))
                }
                Err(error) => Err(error),
            };
        }

        let result = parser.parse(state);

        let entry: MemoEntry = match &result {
            Ok((value, next)) => Ok((Rc::new(value.clone()) as Rc<dyn Any>, next.offset)),
            Err(error) => Err(error.clone()),
        };

        MEMO.with(|memo| memo.borrow_mut().insert((key, offset), entry));

        result
    })
}

/// The entry point: runs `parser` from the start of `source`, first clearing the packrat table so [`memoize`]d results from a previous parse can never be replayed against this input. Does *not* require the input to be fully consumed — end the grammar with [`take_eof`](crate::take_eof) if trailing text should be an error.
pub fn run_parser<'a, A>(parser: Parser<'a, A>, source: &'a Rc<Source>) -> Result<A, ParserError>
where
    A: 'a,
{
    MEMO.with(|memo| memo.borrow_mut().clear());

    let result = parser.parse(ParserState::new(source)).map(|(item, _)| item);

    // Clear on the way out too, not only on the next entry: cached entries share the parsed tree (`Rc`-backed values), and leaving them alive until the thread-local's own destructor would drop a deep tree at thread teardown, where the guard page is all the stack that's left.
    MEMO.with(|memo| memo.borrow_mut().clear());

    result
}
