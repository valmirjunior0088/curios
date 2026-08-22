//! Explicit control-flow structuring over a region's sibling blocks.
//!
//! An [`EmissionBody`] region is a straight-line prologue ending in a single [`EmissionTail`] transfer, plus a flat set of labeled join blocks its branches enter. The emitter must recover structured Wasm control flow — `block`/`loop`/`br`/`br_table` nesting — from that flat set. This module computes the analysis that recovery rests on: the region's control-flow graph over a virtual entry node and its blocks, its strongly connected components (the loop forest), and the reducibility of each component. From those it derives a [`region_layout`] — the loop, forward-block, and localized-dispatcher nesting the emitter walks.
//!
//! Every routine is iterative and orders its output by node index, so the layout is deterministic and stack-safe regardless of block count.

use {
    super::{
        EmissionBlockName, EmissionBody, EmissionCallTarget, EmissionHostTarget, EmissionTail,
    },
    std::collections::{BTreeSet, HashMap, HashSet},
};

/// The control-flow graph of one region over its sibling blocks plus a virtual entry node. Blocks are numbered by their position in `region.blocks`; the entry node — the region's prologue and tail transfer — is numbered one past the last block. The entry has no predecessors and is the root every block is reached from.
#[derive(Debug)]
pub(crate) struct RegionCfg {
    blocks: usize,
    succ: Vec<Vec<usize>>,
    pred: Vec<Vec<usize>>,
}

impl RegionCfg {
    pub(crate) fn new(region: &EmissionBody) -> Self {
        let blocks = region.blocks.len();
        let entry = blocks;
        let index = region
            .blocks
            .iter()
            .enumerate()
            .map(|(i, (name, _))| (name, i))
            // Keyed on a block name purely to look its position up; nothing iterates it, so no name ordering reaches the emitted module.
            .collect::<HashMap<_, _>>();

        let mut succ = vec![Vec::new(); blocks + 1];

        // The entry node branches wherever the region's own tail transfers — never recursing into the blocks, which are separate nodes. A match may name one block from several arms; keep one edge, ordered.
        let mut entry_targets = tail_targets(&region.tail)
            .into_iter()
            .filter_map(|name| index.get(name).copied())
            .collect::<Vec<_>>();
        entry_targets.sort_unstable();
        entry_targets.dedup();
        succ[entry] = entry_targets;

        // A block branches to a sibling whenever the block's region — through any of its own nested blocks — transfers there. One walk per block collects every transferred-to name; mapping through the sibling index and sorting keeps each successor list sorted and duplicate-free (the set already deduplicates).
        for (source, (_, block)) in region.blocks.iter().enumerate() {
            let mut names = HashSet::new();
            collect_region_targets(&block.region, &mut names);
            let mut targets = names
                .into_iter()
                .filter_map(|name| index.get(name).copied())
                .collect::<Vec<_>>();
            targets.sort_unstable();
            succ[source] = targets;
        }

        let mut pred = vec![Vec::new(); blocks + 1];
        for (source, targets) in succ.iter().enumerate() {
            for &target in targets {
                pred[target].push(source);
            }
        }

        Self { blocks, succ, pred }
    }

    /// Total node count: every block plus the entry.
    fn node_count(&self) -> usize {
        self.blocks + 1
    }

    /// The number of real blocks — every node but the entry.
    fn block_count(&self) -> usize {
        self.blocks
    }

    fn successors(&self, node: usize) -> &[usize] {
        &self.succ[node]
    }

    /// The entry nodes of a cyclic component: members reached by an edge from outside the component. A single-node component with a self-loop has one entry (itself); a component with two or more entries is irreducible and only a localized dispatcher can structure it.
    fn component_entries(&self, component: &[usize]) -> Vec<usize> {
        let member = |node: usize| component.binary_search(&node).is_ok();
        component
            .iter()
            .copied()
            .filter(|&node| self.pred[node].iter().any(|&pred| !member(pred)))
            .collect()
    }
}

/// Undefined-node sentinel for Tarjan state. `node_count()` never approaches `usize::MAX`, so it can never collide with a real node.
const NONE: usize = usize::MAX;

/// The blocks a single tail transfers to directly, in source order — not recursing into any nested region.
fn tail_targets(tail: &EmissionTail) -> Vec<&EmissionBlockName> {
    match tail {
        EmissionTail::Jump(target) => vec![&target.target],
        EmissionTail::Match(target) => target
            .cases
            .values()
            .map(|jump| &jump.target)
            .chain(target.default.as_ref().map(|jump| &jump.target))
            .collect(),
        EmissionTail::Call(
            EmissionCallTarget::Direct { resume, .. } | EmissionCallTarget::Indirect { resume, .. },
        ) => vec![resume],
        EmissionTail::Host(EmissionHostTarget::Foreign { resume, .. }) => vec![resume],
        EmissionTail::Host(EmissionHostTarget::Exit { .. }) => vec![],
        EmissionTail::Cell(cell) => vec![cell.resume()],
        EmissionTail::Unreachable => vec![],
    }
}

/// Every block name `region`, or any region nested inside its blocks, branches into — the sibling-level edge oracle, collected in one traversal: a branch from deep inside a block to a sibling is an edge between those two blocks.
fn collect_region_targets<'a>(region: &'a EmissionBody, out: &mut HashSet<&'a EmissionBlockName>) {
    out.extend(tail_targets(&region.tail));
    for (_, block) in &region.blocks {
        collect_region_targets(&block.region, out);
    }
}

/// The structured layout of a region's blocks: an ordered nesting of `loop`, forward `block`, and localized-dispatcher scopes the emitter walks to place each block. Items at one level are already in a topological order of the condensation, so a forward branch to a later item's entry is a forward `br` out of the enclosing scopes.
#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum LayoutItem {
    /// A block reached only by forward edges at this level: emit its body, enter it by a forward branch to its own label.
    Block(usize),
    /// A reducible loop with a single entry `header`. `body` structures the component's interior with the back edges to `header` removed, so the header leads and any nested loop nests inside. Back edges become a branch to the loop label; forward entry falls into the loop.
    Loop {
        header: usize,
        body: Vec<LayoutItem>,
    },
    /// An irreducible or multi-entry component structured by a localized `br_table` dispatcher over its `members`, each a possible entry.
    Dispatch { members: Vec<usize> },
}

/// Structure a region's blocks into a [`LayoutItem`] nesting driven entirely by the control-flow analysis: strongly connected components give the loop hierarchy, single-entry components become `loop`s, and only genuinely irreducible components fall back to a localized dispatcher.
pub(crate) fn region_layout(region: &EmissionBody) -> Vec<LayoutItem> {
    let cfg = RegionCfg::new(region);
    let all = (0..cfg.block_count()).collect();
    structure_set(&cfg, &all, &BTreeSet::new())
}

/// A view of the region CFG restricted to `nodes`, with every edge *into* a `suppressed` node removed. Suppressing a loop header's incoming edges turns its back edges into non-edges, exposing the interior — nested loops and the header itself — as a smaller acyclic-or-cyclic graph to structure.
struct Subgraph<'a> {
    cfg: &'a RegionCfg,
    nodes: &'a BTreeSet<usize>,
    suppressed: &'a BTreeSet<usize>,
}

impl Subgraph<'_> {
    fn successors(&self, node: usize) -> impl Iterator<Item = usize> + '_ {
        self.cfg
            .successors(node)
            .iter()
            .copied()
            .filter(|target| self.nodes.contains(target) && !self.suppressed.contains(target))
    }

    /// Strongly connected components within the subgraph, by iterative Tarjan. Roots are taken in ascending order and members sorted, so both the components and their order are deterministic.
    fn components(&self) -> Vec<Vec<usize>> {
        let bound = self.cfg.node_count();
        let mut index = vec![NONE; bound];
        let mut lowlink = vec![NONE; bound];
        let mut on_stack = vec![false; bound];
        let mut tarjan = Vec::new();
        let mut counter = 0;
        let mut components = Vec::new();

        for &root in self.nodes {
            if index[root] != NONE {
                continue;
            }
            let mut frames = vec![(root, self.successors(root).collect::<Vec<_>>(), 0usize)];
            index[root] = counter;
            lowlink[root] = counter;
            counter += 1;
            tarjan.push(root);
            on_stack[root] = true;
            while let Some((node, edges, cursor)) = frames.last_mut() {
                if *cursor < edges.len() {
                    let next = edges[*cursor];
                    *cursor += 1;
                    if index[next] == NONE {
                        index[next] = counter;
                        lowlink[next] = counter;
                        counter += 1;
                        tarjan.push(next);
                        on_stack[next] = true;
                        let successors = self.successors(next).collect::<Vec<_>>();
                        frames.push((next, successors, 0));
                    } else if on_stack[next] {
                        let node = *node;
                        lowlink[node] = lowlink[node].min(index[next]);
                    }
                } else {
                    let node = *node;
                    if lowlink[node] == index[node] {
                        let mut members = Vec::new();
                        loop {
                            let member = tarjan.pop().unwrap();
                            on_stack[member] = false;
                            members.push(member);
                            if member == node {
                                break;
                            }
                        }
                        members.sort_unstable();
                        components.push(members);
                    }
                    frames.pop();
                    if let Some((parent, _, _)) = frames.last() {
                        let parent = *parent;
                        lowlink[parent] = lowlink[parent].min(lowlink[node]);
                    }
                }
            }
        }

        components.sort_by_key(|members| members[0]);
        components
    }

    /// The components in a deterministic topological order of the condensation (Kahn, smallest-least-member first), so every cross-component edge points from an earlier item to a later one.
    fn condensation_order(&self, components: &[Vec<usize>]) -> Vec<usize> {
        let mut of = vec![NONE; self.cfg.node_count()];
        for (position, members) in components.iter().enumerate() {
            for &member in members {
                of[member] = position;
            }
        }

        let mut successors = vec![BTreeSet::new(); components.len()];
        let mut indegree = vec![0usize; components.len()];
        for (source, members) in components.iter().enumerate() {
            for &member in members {
                for target in self.successors(member) {
                    let target = of[target];
                    if target != source && successors[source].insert(target) {
                        indegree[target] += 1;
                    }
                }
            }
        }

        let mut ready = (0..components.len())
            .filter(|&component| indegree[component] == 0)
            .collect::<BTreeSet<_>>();
        let mut order = Vec::with_capacity(components.len());
        while let Some(&component) = ready.iter().next() {
            ready.remove(&component);
            order.push(component);
            for &next in &successors[component] {
                indegree[next] -= 1;
                if indegree[next] == 0 {
                    ready.insert(next);
                }
            }
        }
        order
    }
}

fn structure_set(
    cfg: &RegionCfg,
    nodes: &BTreeSet<usize>,
    suppressed: &BTreeSet<usize>,
) -> Vec<LayoutItem> {
    let subgraph = Subgraph {
        cfg,
        nodes,
        suppressed,
    };
    let components = subgraph.components();
    let order = subgraph.condensation_order(&components);

    let mut items = Vec::new();
    for position in order {
        let component = &components[position];
        if component.len() == 1 && !subgraph.successors(component[0]).any(|s| s == component[0]) {
            items.push(LayoutItem::Block(component[0]));
            continue;
        }

        let entries = cfg.component_entries(component);
        if let [header] = entries.as_slice() {
            // A reducible loop: structure its interior with the header's back edges removed, so the header leads and nested loops nest inside.
            let interior = component.iter().copied().collect();
            let mut inner_suppressed = suppressed.clone();
            inner_suppressed.insert(*header);
            let body = structure_set(cfg, &interior, &inner_suppressed);
            items.push(LayoutItem::Loop {
                header: *header,
                body,
            });
        } else {
            items.push(LayoutItem::Dispatch {
                members: component.clone(),
            });
        }
    }
    items
}

#[cfg(test)]
mod tests {
    use {
        super::{LayoutItem, RegionCfg, region_layout},
        crate::{
            EmissionBlock, EmissionBlockName, EmissionBody, EmissionJumpTarget,
            EmissionMatchTarget, EmissionTail, EmissionValueName,
        },
    };

    fn body(tail: EmissionTail) -> EmissionBody {
        EmissionBody {
            values: vec![],
            blocks: vec![],
            tail,
        }
    }

    fn jump(target: &EmissionBlockName) -> EmissionTail {
        EmissionTail::Jump(EmissionJumpTarget {
            target: target.clone(),
            params: vec![],
        })
    }

    fn switch(operand: &str, arms: &[(u32, &EmissionBlockName)]) -> EmissionTail {
        EmissionTail::Match(EmissionMatchTarget {
            operand: EmissionValueName::from(operand),
            cases: arms
                .iter()
                .map(|(tag, target)| {
                    (
                        *tag,
                        EmissionJumpTarget {
                            target: (*target).clone(),
                            params: vec![],
                        },
                    )
                })
                .collect(),
            default: None,
        })
    }

    fn region(blocks: Vec<(EmissionBlockName, EmissionTail)>, tail: EmissionTail) -> EmissionBody {
        EmissionBody {
            values: vec![],
            blocks: blocks
                .into_iter()
                .map(|(name, tail)| {
                    (
                        name,
                        EmissionBlock {
                            params: vec![],
                            region: body(tail),
                        },
                    )
                })
                .collect(),
            tail,
        }
    }

    #[test]
    fn entry_and_block_edges_are_sorted_and_deduplicated() {
        let first = EmissionBlockName::from("first");
        let second = EmissionBlockName::from("second");
        // The entry match names `first` from two arms: one deduplicated edge.
        let region = region(
            vec![
                (first.clone(), jump(&second)),
                (second.clone(), EmissionTail::Unreachable),
            ],
            switch("tag", &[(0, &first), (1, &first)]),
        );
        let cfg = RegionCfg::new(&region);

        // The entry node is one past the last block.
        assert_eq!(cfg.block_count(), 2);
        assert_eq!(cfg.node_count(), 3);
        assert_eq!(cfg.successors(cfg.block_count()), &[0]);
        assert_eq!(cfg.successors(0), &[1]);
        assert_eq!(cfg.successors(1), &[] as &[usize]);
    }

    #[test]
    fn component_entries_distinguish_reducible_from_irreducible() {
        // A single-entry cycle (reducible) versus a two-entry cross-jump (irreducible), read off `component_entries` directly.
        let header = EmissionBlockName::from("header");
        let repeat = EmissionBlockName::from("repeat");
        let reducible = region(
            vec![
                (header.clone(), switch("c", &[(0, &header), (1, &repeat)])),
                (repeat.clone(), jump(&header)),
            ],
            jump(&header),
        );
        let cfg = RegionCfg::new(&reducible);
        assert_eq!(cfg.component_entries(&[0, 1]), vec![0]);

        let left = EmissionBlockName::from("left");
        let right = EmissionBlockName::from("right");
        let irreducible = region(
            vec![(left.clone(), jump(&right)), (right.clone(), jump(&left))],
            switch("entry", &[(0, &left), (1, &right)]),
        );
        let cfg = RegionCfg::new(&irreducible);
        assert_eq!(cfg.component_entries(&[0, 1]), vec![0, 1]);
    }

    #[test]
    fn acyclic_region_lays_out_as_plain_blocks() {
        let branch = EmissionBlockName::from("branch");
        let left = EmissionBlockName::from("left");
        let right = EmissionBlockName::from("right");
        let join = EmissionBlockName::from("join");
        let region = region(
            vec![
                (branch.clone(), switch("c", &[(0, &left), (1, &right)])),
                (left.clone(), jump(&join)),
                (right.clone(), jump(&join)),
                (join.clone(), EmissionTail::Unreachable),
            ],
            jump(&branch),
        );

        let layout = region_layout(&region);
        // Every block is a plain forward block; no loop, no dispatcher. The order is topological: branch, its two arms, then the join.
        assert_eq!(
            layout,
            vec![
                LayoutItem::Block(0),
                LayoutItem::Block(1),
                LayoutItem::Block(2),
                LayoutItem::Block(3),
            ]
        );
    }

    #[test]
    fn single_loop_lays_out_as_one_loop_with_its_exit_outside() {
        let header = EmissionBlockName::from("header");
        let repeat = EmissionBlockName::from("repeat");
        let exit = EmissionBlockName::from("exit");
        let region = region(
            vec![
                (header.clone(), switch("c", &[(0, &exit), (1, &repeat)])),
                (repeat.clone(), jump(&header)),
                (exit.clone(), EmissionTail::Unreachable),
            ],
            jump(&header),
        );

        let layout = region_layout(&region);
        assert_eq!(
            layout,
            vec![
                LayoutItem::Loop {
                    header: 0,
                    body: vec![LayoutItem::Block(0), LayoutItem::Block(1)],
                },
                LayoutItem::Block(2),
            ]
        );
    }

    #[test]
    fn nested_loops_nest_in_the_layout() {
        let outer = EmissionBlockName::from("outer");
        let inner = EmissionBlockName::from("inner");
        let after = EmissionBlockName::from("after");
        let region = region(
            vec![
                (outer.clone(), switch("c", &[(0, &after), (1, &inner)])),
                (inner.clone(), switch("d", &[(0, &outer), (1, &inner)])),
                (after.clone(), EmissionTail::Unreachable),
            ],
            jump(&outer),
        );

        let layout = region_layout(&region);
        assert_eq!(
            layout,
            vec![
                LayoutItem::Loop {
                    header: 0,
                    body: vec![
                        LayoutItem::Block(0),
                        LayoutItem::Loop {
                            header: 1,
                            body: vec![LayoutItem::Block(1)],
                        },
                    ],
                },
                LayoutItem::Block(2),
            ]
        );
    }

    #[test]
    fn irreducible_cycle_lays_out_as_a_dispatcher() {
        let left = EmissionBlockName::from("left");
        let right = EmissionBlockName::from("right");
        let region = region(
            vec![(left.clone(), jump(&right)), (right.clone(), jump(&left))],
            switch("entry", &[(0, &left), (1, &right)]),
        );

        let layout = region_layout(&region);
        assert_eq!(
            layout,
            vec![LayoutItem::Dispatch {
                members: vec![0, 1]
            }]
        );
    }
}
