use crate::{cont, ersd};

pub fn unsupported_sync_rec_item(term: &ersd::Term) -> ! {
    panic!(
        "`to_cont` does not support a call-valued `rec` item in value position: \
         the following term reaches `Apply`/`Match`/`NatMatch` on its construction path \
         but is bound where a synchronous value is required: {term:?}",
    )
}

/// Post-order (dependencies first) of the call/match-valued `rec` bindings, panicking
/// with the offending cycle if two such bindings depend on each other's value — that
/// case needs runtime fixpoint cells, which are out of scope.
pub fn rec_computed_order(names: &[&str], deps: &[Vec<usize>]) -> Vec<usize> {
    fn visit(
        node: usize,
        names: &[&str],
        deps: &[Vec<usize>],
        marks: &mut [u8],
        stack: &mut Vec<usize>,
        order: &mut Vec<usize>,
    ) {
        marks[node] = 1;
        stack.push(node);

        for &next in &deps[node] {
            match marks[next] {
                1 => {
                    let start = stack.iter().position(|&n| n == next).unwrap();
                    let cycle = stack[start..]
                        .iter()
                        .chain([&next])
                        .map(|&n| names[n])
                        .collect::<Vec<_>>()
                        .join(" -> ");

                    panic!(
                        "`to_cont` does not support value-level mutual recursion through calls: \
                         {cycle} would require runtime fixpoint cells",
                    );
                }
                0 => visit(next, names, deps, marks, stack, order),
                _ => {}
            }
        }

        stack.pop();
        marks[node] = 2;
        order.push(node);
    }

    let mut marks = vec![0u8; names.len()];
    let mut stack = vec![];
    let mut order = vec![];

    for node in 0..names.len() {
        if marks[node] == 0 {
            visit(node, names, deps, &mut marks, &mut stack, &mut order);
        }
    }

    order
}

/// How a `rec`-bound prealloc'd shell is backpatched. A `Func` is lowered eagerly so
/// its `ClsrName` is shared by both the prealloc declaration and the patch; tuples and arrays
/// only need their length up front and lower their elements at patch time.
pub enum Backpatch<'b> {
    Clsr(cont::ClsrName, Vec<cont::ValueName>),
    Tpl(&'b [ersd::Subterm]),
    Arr(&'b [ersd::Subterm]),
}

impl Backpatch<'_> {
    pub fn prealloc(&self) -> cont::Prealloc {
        match self {
            Backpatch::Clsr(clsr, _) => cont::Prealloc::Clsr(clsr.clone()),
            Backpatch::Tpl(fields) => cont::Prealloc::Tpl(fields.len()),
            Backpatch::Arr(elems) => cont::Prealloc::Arr(elems.len()),
        }
    }
}
