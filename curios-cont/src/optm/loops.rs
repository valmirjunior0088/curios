//! Loop-shape recognition, shared by the passes that reason about the loops
//! [`tail_recursion`](super::tail_recursion) mints (and
//! [`function_inlining`](super::function_inlining) splices intact): a block is
//! a *loop header* when its own subtree transfers control back to it. Only
//! self-shaped loops are recognized — a cycle spelled through sibling blocks
//! is invisible here, which keeps every client sound-but-incomplete rather
//! than dependent on a general CFG analysis.

use super::*;

/// Does `block`'s subtree contain a tail that targets `name`? The back-edge
/// test: `true` makes `name` a loop header.
pub fn is_loop_header(name: &BlockName, block: &Block) -> bool {
    subtree_targets(&block.region, name)
}

fn subtree_targets(region: &Region, name: &BlockName) -> bool {
    if tail_targets(&region.tail).contains(&name) {
        return true;
    }

    region
        .blocks
        .iter()
        .any(|(_, block)| subtree_targets(&block.region, name))
}

/// Every loop header in a region tree, in declaration order.
pub fn find_self_loops(region: &Region) -> Vec<BlockName> {
    let mut headers = Vec::new();
    collect_self_loops(region, &mut headers);

    headers
}

fn collect_self_loops(region: &Region, headers: &mut Vec<BlockName>) {
    for (name, block) in &region.blocks {
        if is_loop_header(name, block) {
            headers.push(name.clone());
        }

        collect_self_loops(&block.region, headers);
    }
}
