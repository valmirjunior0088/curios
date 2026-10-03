use {
    super::{LayoutItem, RegionCfg, region_layout},
    crate::{
        EmissionBlock, EmissionBlockName, EmissionBody, EmissionJumpTarget, EmissionMatchTarget,
        EmissionTail, EmissionValueName,
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
