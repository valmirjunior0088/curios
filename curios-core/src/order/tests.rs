use {
    super::*,
    crate::{Definition, DefinitionKind, Free, Term, Totality, UniverseContext},
    curios_utilities::Qualifier,
};

fn authored(name: &str) -> Global {
    Global::Authored(Qualifier::from([name]))
}

/// `name`, whose body mentions each of `mentioned`.
fn reading(name: &str, mentioned: &[&str]) -> Item {
    Item::Let(Definition {
        name: authored(name),
        kind: DefinitionKind::Authored,
        universe_context: UniverseContext::empty(),
        island: Qualifier::empty(),
        totality: Totality::default(),
        proof: false,
        variances: Vec::new(),
        field_variances: Vec::new(),
        parameter_field: None,
        type_: Term::type_ground(),
        body: Term::tuple(
            mentioned
                .iter()
                .map(|mentioned| Term::free_var(&Free::Global(authored(mentioned)))),
        ),
    })
}

/// A module of `items` and nothing else.
fn over(items: Vec<Item>) -> Module {
    Module {
        items: items.into(),
        mounts: Vec::new(),
        witnesses: BTreeSet::new(),
        tests: Vec::new(),
    }
}

/// The first name of each item, in the order the module holds them.
fn names(module: &Module) -> Vec<String> {
    module.items.iter().map(Item::describe).collect()
}

/// A module's stored order is every item at its depth and otherwise by name, whatever order the items were handed in.
#[test]
fn items_are_ordered_by_depth_and_then_by_name() {
    let written = [
        reading("d", &["b"]),
        reading("c", &[]),
        reading("b", &["z"]),
        reading("a", &["d"]),
        reading("z", &[]),
    ];

    let stored = names(&over(written.to_vec()).ordered());
    assert_eq!(stored, ["/c", "/z", "/b", "/d", "/a"]);

    for turn in 1..written.len() {
        let mut turned = written.to_vec();
        turned.rotate_left(turn);
        turned.swap(0, 1);
        assert_eq!(names(&over(turned).ordered()), stored);
    }
}

/// An item stands ahead of every deeper item, whatever their names: `c` reaches nothing, so it stands beside `a` and ahead of `b`, which reaches `a`.
#[test]
fn an_item_stands_ahead_of_every_deeper_item_whatever_its_name() {
    let module = over(vec![
        reading("b", &["a"]),
        reading("a", &[]),
        reading("c", &[]),
    ]);

    assert_eq!(names(&module.ordered()), ["/a", "/c", "/b"]);
}

/// Items that reach one another have no depth: the least-named of them takes the depth of what it reaches that has one, and every item is still stored once.
#[test]
fn items_that_reach_one_another_are_broken_at_the_least_named() {
    let module = over(vec![
        reading("c", &["b"]),
        reading("b", &["a"]),
        reading("a", &["b"]),
    ]);

    assert_eq!(names(&module.ordered()), ["/a", "/b", "/c"]);
}

/// The order says what each item reaches of the module, its own names and other units' left out, and the depth that gives it.
#[test]
fn the_order_holds_what_each_item_reaches_and_its_depth() {
    let (_, order) = over(vec![
        reading("b", &["a", "b", "elsewhere"]),
        reading("a", &[]),
    ])
    .in_order();

    assert_eq!(
        order.reach(&authored("b")),
        Some(&Reach {
            depth: 1,
            names: vec![authored("a")],
        })
    );
    assert_eq!(
        order.reach(&authored("a")),
        Some(&Reach {
            depth: 0,
            names: Vec::new(),
        })
    );
}

/// A module of named items, each by the names it mentions, as a test edits one: what a module is made whole of, and what a revision is told was made and dropped.
#[derive(Clone)]
struct Written(BTreeMap<String, Vec<String>>);

impl Written {
    fn of(items: &[(&str, &[&str])]) -> Self {
        Self(
            items
                .iter()
                .map(|(name, mentioned)| {
                    (
                        (*name).to_string(),
                        mentioned
                            .iter()
                            .map(|mentioned| (*mentioned).to_string())
                            .collect(),
                    )
                })
                .collect(),
        )
    }

    fn item(&self, name: &str) -> Item {
        let mentioned = self.0[name].iter().map(String::as_str).collect::<Vec<_>>();
        reading(name, &mentioned)
    }

    fn module(&self) -> Module {
        over(self.0.keys().map(|name| self.item(name)).collect())
    }
}

/// A module as a recompile holds it: its items, where it holds each name, and its order.
struct Held {
    module: Module,
    places: Places,
    order: Order,
}

impl Held {
    fn whole(written: &Written) -> Self {
        let (module, order) = written.module().in_order();
        let places = Places::of(&module);

        Self {
            module,
            places,
            order,
        }
    }

    /// This module revised into `now`, every item whose mentions differ made and every name `now` does not hold dropped, held to what ordering `now` whole gives.
    fn revised(&self, then: &Written, now: &Written) -> Self {
        let made = now
            .0
            .iter()
            .filter(|(name, mentioned)| then.0.get(*name) != Some(mentioned))
            .map(|(name, _)| now.item(name))
            .collect::<Vec<_>>();
        let dropped = then
            .0
            .keys()
            .filter(|name| !now.0.contains_key(*name))
            .map(|name| authored(name))
            .collect::<BTreeSet<_>>();
        let Revised {
            items,
            places,
            order,
        } = self
            .order
            .revised(&self.module, &self.places, &made, &dropped)
            .expect("a revision of a module whose items reach no cycle");
        let module = Module {
            items,
            ..over(Vec::new())
        };

        let whole = Held::whole(now);
        assert_eq!(
            names(&module),
            names(&whole.module),
            "the items stand in the order"
        );
        assert!(
            order == whole.order,
            "the order says what ordering whole says"
        );
        let now_places = Places::of(&module);
        if let Some(places) = &places {
            for name in now.0.keys() {
                let name = authored(name);
                assert_eq!(places.place(&name), now_places.place(&name), "{name:?}");
            }
        }

        Self {
            module,
            places: places.unwrap_or(now_places),
            order,
        }
    }
}

/// Where a revision holds the item first named `name`, and where the module it was made of held it, as the revision's version places it.
fn stood(then: &Held, now: &Held, name: &str) -> (Option<usize>, Option<usize>) {
    let name = authored(name);
    let before = then
        .places
        .place(&name)
        .map(|place| then.module.items.place_in_version(place.item()));

    (now.places.place(&name).map(|place| place.item()), before)
}

/// A reference that deepens an item moves it and every item that stands on it, and only those: `b` comes to reach `d`, so `b` and `a`, which reaches `b`, go deeper, while `c` and `d` stand where they stood.
#[test]
fn a_deeper_reference_moves_what_stands_on_it() {
    let then = Written::of(&[("a", &["b"]), ("b", &[]), ("c", &[]), ("d", &["c"])]);
    let now = Written::of(&[("a", &["b"]), ("b", &["d"]), ("c", &[]), ("d", &["c"])]);
    let held = Held::whole(&then);

    let revised = held.revised(&then, &now);
    assert_eq!(names(&revised.module), ["/c", "/d", "/b", "/a"]);
    for name in ["c", "d"] {
        let (now, before) = stood(&held, &revised, name);
        assert_eq!(now, before, "{name} stands where it stood");
    }
}

/// A reference taken away raises what stood on it: `b` stops reaching `d`, so `b` and `a` come up beside the items of their new depths.
#[test]
fn a_reference_taken_away_raises_what_stood_on_it() {
    let then = Written::of(&[("a", &["b"]), ("b", &["d"]), ("c", &[]), ("d", &["c"])]);
    let now = Written::of(&[("a", &["b"]), ("b", &[]), ("c", &[]), ("d", &["c"])]);

    let revised = Held::whole(&then).revised(&then, &now);
    assert_eq!(names(&revised.module), ["/b", "/c", "/a", "/d"]);
}

/// An item made at the key it had stands where it stood, and nothing around it moves: `b` comes to reach `c` as well as `a`, both at depth zero.
#[test]
fn an_item_made_at_its_key_stands_where_it_stood() {
    let then = Written::of(&[("a", &[]), ("b", &["a"]), ("c", &[])]);
    let now = Written::of(&[("a", &[]), ("b", &["a", "c"]), ("c", &[])]);
    let held = Held::whole(&then);

    let revised = held.revised(&then, &now);
    for name in ["a", "b", "c"] {
        let (now, before) = stood(&held, &revised, name);
        assert_eq!(now, before, "{name} stands where it stood");
    }
}

/// An item new to the module stands at its key, an item taken away with every reference to it is gone, and a version made of a version keeps the places it carries.
#[test]
fn revisions_over_revisions_carry_their_places() {
    let first = Written::of(&[("a", &[]), ("b", &["a"]), ("c", &["b"])]);
    let second = Written::of(&[("a", &[]), ("b", &["a"]), ("c", &["b"]), ("bb", &["a"])]);
    let third = Written::of(&[("a", &[]), ("c", &[]), ("bb", &["a"])]);

    let held = Held::whole(&first).revised(&first, &second);
    assert_eq!(names(&held.module), ["/a", "/b", "/bb", "/c"]);
    let held = held.revised(&second, &third);
    assert_eq!(names(&held.module), ["/a", "/c", "/bb"]);
}

/// Items put one after another between the same two run out of places between them, and are placed apart again in the order: `zz` stands after every `pNN`, each new one put after the last.
#[test]
fn a_run_put_between_the_same_two_is_placed_apart_again() {
    let mut written = Written::of(&[("base", &[]), ("zz", &["base"])]);
    let mut held = Held::whole(&written);
    for index in 0..40 {
        let mut now = written.clone();
        now.0
            .insert(format!("p{index:02}"), vec!["base".to_string()]);
        held = held.revised(&written, &now);
        written = now;
    }

    assert_eq!(names(&held.module).last().map(String::as_str), Some("/zz"));
}

/// A run of edits — references added and taken away, items new to the module, and items dropped with every reference to them — orders each text as ordering it whole does.
#[test]
fn a_run_of_edits_is_ordered_as_each_text_is_whole() {
    let mut state = 0x9e37_79b9_7f4a_7c15_u64;
    let mut draw = move |bound: usize| {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        let bound = u64::try_from(bound.max(1)).expect("a bound fits a u64");
        usize::try_from(state % bound).expect("a draw below a usize bound fits one")
    };
    let name = |index: usize| format!("n{index:03}");
    // Each item mentions only items named before it, so no edit closes a cycle.
    let mut written = Written(BTreeMap::new());
    let mut next = 0;
    for _ in 0..40 {
        let count = draw(4);
        let mentioned = (0..count)
            .filter(|_| next > 0)
            .map(|_| name(draw(next)))
            .collect();
        written.0.insert(name(next), mentioned);
        next += 1;
    }

    let mut held = Held::whole(&written);
    for _ in 0..80 {
        let mut now = written.clone();
        let present = now.0.keys().cloned().collect::<Vec<_>>();
        let chosen = present[draw(present.len())].clone();
        let index = chosen[1..].parse::<usize>().expect("a name made above");
        match draw(4) {
            0 if index > 0 => {
                let target = name(draw(index));
                if now.0.contains_key(&target) {
                    now.0.entry(chosen).or_default().push(target);
                }
            }
            1 => {
                let mentioned = now.0.entry(chosen).or_default();
                if !mentioned.is_empty() {
                    let at = draw(mentioned.len());
                    mentioned.remove(at);
                }
            }
            2 => {
                let count = draw(4);
                let mentioned = (0..count)
                    .map(|_| present[draw(present.len())].clone())
                    .collect();
                now.0.insert(name(next), mentioned);
                next += 1;
            }
            _ if present.len() > 1 => {
                now.0.remove(&chosen);
                for mentioned in now.0.values_mut() {
                    mentioned.retain(|held| *held != chosen);
                }
            }
            _ => {}
        }
        held = held.revised(&written, &now);
        written = now;
    }
}

/// A kept item that reaches a name taken away, and an edit that closes a cycle, are left to the order given whole.
#[test]
fn what_no_revision_resolves_is_left_to_the_whole_order() {
    let then = Written::of(&[("a", &[]), ("b", &["a"])]);
    let held = Held::whole(&then);
    let dropped = BTreeSet::from([authored("a")]);
    assert!(
        held.order
            .revised(&held.module, &held.places, &[], &dropped)
            .is_none()
    );

    let cycle = [reading("a", &["b"])];
    assert!(
        held.order
            .revised(&held.module, &held.places, &cycle, &BTreeSet::new())
            .is_none()
    );
}

/// An order is stored with each name once and restored as it was: every item's depth and what it reaches.
#[test]
fn an_order_is_restored_as_it_was_stored() {
    let (_, order) = Written::of(&[("a", &[]), ("b", &["a"]), ("c", &["a", "b"])])
        .module()
        .in_order();

    let bytes = curios_archive::to_bytes(&order).expect("an order archives");
    let restored = curios_archive::from_bytes::<Order>(&bytes).expect("an order restores");
    assert!(restored == order);
    assert_eq!(
        restored.reach(&authored("c")).map(|reach| reach.depth),
        Some(2)
    );
}

/// A revision that takes every item out places the items new to the module from the start, each a spacing after the one before.
#[test]
fn a_module_made_anew_is_placed_from_the_start() {
    let then = Written::of(&[("other", &[])]);
    let now = Written::of(&[("a", &[]), ("b", &["a"])]);

    let revised = Held::whole(&then).revised(&then, &now);
    assert_eq!(names(&revised.module), ["/a", "/b"]);
}
