use {
    super::Parser,
    crate::{IntoParser, MemoEntry, Parser as _, remember, remembered},
    std::{any::Any, rc::Rc},
};

/// [`memoize`](crate::memoize), boxed.
pub fn memoize<'a, A, M>(key: u32, parser: impl IntoParser<'a, A, M> + 'a) -> Parser<'a, A>
where
    A: Clone + 'static,
    M: 'a,
{
    Parser::new(move |state| {
        let offset = state.offset;

        if let Some(entry) = remembered(key, offset) {
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

        let result = parser.into_parser().parse(state);

        let entry: MemoEntry = match &result {
            Ok((value, next)) => Ok((Rc::new(value.clone()) as Rc<dyn Any>, next.offset)),
            Err(error) => Err(error.clone()),
        };

        remember(key, offset, entry);

        result
    })
}
