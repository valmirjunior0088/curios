use {
    super::{Error, LowerResult},
    curios_cont::{ClsrName, ValueName},
};

pub(super) fn unsupported_sync_rec_item(term: &crate::Term) -> Error {
    Error::UnsupportedSyncRecItem {
        term: term.to_string(),
    }
}

/// Post-order (dependencies first) of the call/match-valued `rec` bindings, erroring
/// with the offending cycle if two such bindings depend on each other's value — that
/// case needs runtime fixpoint cells, which are out of scope.
pub(super) fn rec_computed_order(names: &[&str], deps: &[Vec<usize>]) -> LowerResult<Vec<usize>> {
    fn visit(
        node: usize,
        names: &[&str],
        deps: &[Vec<usize>],
        marks: &mut [u8],
        stack: &mut Vec<usize>,
        order: &mut Vec<usize>,
    ) -> LowerResult<()> {
        marks[node] = 1;
        stack.push(node);

        for &next in &deps[node] {
            match marks[next] {
                1 => {
                    let start = stack.iter().position(|&n| n == next).unwrap();
                    let cycle = stack[start..]
                        .iter()
                        .chain([&next])
                        .map(|&n| names[n].to_string())
                        .collect::<Vec<_>>();

                    return Err(Error::CyclicRecComputed { cycle });
                }
                0 => visit(next, names, deps, marks, stack, order)?,
                _ => {}
            }
        }

        stack.pop();
        marks[node] = 2;
        order.push(node);

        Ok(())
    }

    let mut marks = vec![0u8; names.len()];
    let mut stack = vec![];
    let mut order = vec![];

    for node in 0..names.len() {
        if marks[node] == 0 {
            visit(node, names, deps, &mut marks, &mut stack, &mut order)?;
        }
    }

    Ok(order)
}

/// A `rec`-bound closure shell and its captures. The `Func` is lowered eagerly so its
/// `clsr` name is shared by both the prealloc declaration (the shell) and the later fill.
/// Only closures need a shell: tuples and arrays are *not* prealloc'd — they lower as
/// computed items, so a cyclic one is rejected by [`rec_computed_order`] rather than
/// back-patched (see `plan_backpatch`). Confining cyclic recursion to closures is what lets
/// every `tpl`/`lst` wasm field stay immutable.
pub(super) struct Backpatch {
    pub clsr: ClsrName,
    pub captures: Vec<ValueName>,
}
