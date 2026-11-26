//! A witness that resolves through a witness — its own entry, or another's.
//!
//! A witness declaration registers before its body elaborates, so a body may resolve the very entry it is defining. That makes the definition name itself, which no `let` can express: cross-definition value recursion exists only inside a `rec` item, so such a witness becomes a group of one and its self-reference is bound by that group rather than left free for the certifier to refuse.
//!
//! While its body elaborates, a witness read through its own entry is read as any group's member is: through its slot, once its turn finds it known to terminate, so a law stated over its superclasses reads them there as the registered witnesses, and a member not known to terminate stands for nothing, a read through it refused naming it.
//!
//! A cycle between *two* witnesses has no such binding unless the source declares it: one witness naming a later one is ordinary and works, because a declaration elaborates after what it reads, wherever each is written; two naming each other are declared as one `satisfy … and …` group, whose members register before any body elaborates and are bound in one another, and an undeclared pair is refused here, in the language's own terms, rather than reaching the certifier as an unbound internal name.

use {crate::test_support::*, curios_common::fill};

/// The head of the declaration of `name` in `elaborated`, with the decorator standing above it where it has one: what says whether a witness is a definition or a group of one, a group of one being declared under `#{node = Rec}`. A decorator begins at the line's start, and a block it opens continues on indented lines down to the head.
fn declaration(elaborated: &str, name: &str) -> String {
    let lines = elaborated.lines().collect::<Vec<_>>();
    let head = lines
        .iter()
        .position(|line| line.contains(&format!("let {name}:")))
        .unwrap_or_else(|| panic!("{name} is declared:\n{elaborated}"));
    let start = (0..head)
        .rev()
        .find(|&above| lines[above].starts_with("#{"))
        .filter(|&above| {
            lines[above + 1..head]
                .iter()
                .all(|line| line.starts_with(' '))
        })
        .unwrap_or(head);

    lines[start..=head].join("\n")
}

// A method stated by another of its witness's methods reads that method, not the witness: `apart` reads `same`, which names nothing of the witness, so the body recurses nowhere and the witness is an ordinary definition — where a group of one is a knot forced at every `apart`.

#[test]
fn a_witness_reading_another_of_its_methods_is_no_group() {
    let source = r#"
        use /std/{Nat, Bool};
        concept Same(A: Type): Type { same(A, A) -> Bool, apart(A, A) -> Bool, }
        pub struct Box: pub Type { value: Nat }
        satisfy Same(Box) {
            same(a, b) = a.value == b.value,
            apart(a, b) = Bool/not(Same/same(a, b)),
        }
        /std/print(Bool/to_str(Same/apart(Box { value = 1 }, Box { value = 2 })))
        "#;

    assert_eq!(run(source), b"true");
    let elaborated = core_elab(source);
    assert!(
        !declaration(&elaborated, "/witness~/Same(/Box)").contains("node = Rec"),
        "expected the witness as a definition, not a group of one:\n{elaborated}"
    );
}

// The same under parameters: the read stands at the witness's own binders, and is the method's term at them.
#[test]
fn a_parameterized_witness_reading_another_of_its_methods_is_no_group() {
    let source = r#"
        use /std/{Nat, Bool};
        use /std/ops/{Eql};
        concept Same(A: Type): Type { same(A, A) -> Bool, apart(A, A) -> Bool, }
        pub struct Wrap(A: Type): pub Type { value: A }
        satisfy (@A: Type, use Eql(A)) => Same(Wrap(A)) {
            same(a, b) = a.value == b.value,
            apart(a, b) = Bool/not(Same/same(a, b)),
        }
        /std/print(Bool/to_str(Same/apart(Wrap { value = 1 }, Wrap { value = 1 })))
        "#;

    assert_eq!(run(source), b"false");
    let elaborated = core_elab(source);
    assert!(
        !declaration(&elaborated, "/witness~/Same(/Wrap)").contains("node = Rec"),
        "expected the witness as a definition, not a group of one:\n{elaborated}"
    );
}

// A method that reaches itself keeps its witness a group, and so does the one beside it that reads it: `same` compares two chains link by link through its own entry, and `apart` reads that `same`.
#[test]
fn a_method_reading_one_that_recurses_stays_in_its_group() {
    let source = r#"
        use /std/{Nat, Bool};
        concept Same(A: Type): Type { same(A, A) -> Bool, apart(A, A) -> Bool, }
        induct Chain: pub Type | end_of() | link(Nat, Chain) end
        satisfy Same(Chain) {
            same(a, b) =
                match a
                | end_of() => match b | end_of() => true | link(_, _) => false end
                | link(x, xs) => match b | end_of() => false | link(y, ys) => x == y && Same/same(xs, ys) end
                end,
            apart(a, b) = Bool/not(Same/same(a, b)),
        }
        let one = Chain/link(1, Chain/link(2, Chain/end_of()));
        let other = Chain/link(1, Chain/link(3, Chain/end_of()));
        /std/print(Bool/to_str(Same/apart(one, other)))
        "#;

    assert_eq!(run(source), b"true");
    let elaborated = core_elab(source);
    assert!(
        declaration(&elaborated, "/witness~/Same(/Chain)").contains("node = Rec"),
        "expected the witness as a group of one:\n{elaborated}"
    );
}

// The shape the derived bodies of a structural concept take: match the constructors, call the concept's own method on each recursive payload.
#[test]
fn a_witness_resolves_through_its_own_entry() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct Tree : pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Show(Tree) {
            show(t) =
                match t
                | leaf(n) => Nat/to_str(n)
                | node(l, r) =>
                    Str/concat("(", Str/concat(Show/show(l), Str/concat(" ", Str/concat(Show/show(r), ")"))))
                end,
        }
        /std/print(Show/show(Tree/node(Tree/leaf(1), Tree/leaf(2))))
        "#;

    assert_eq!(run(source), b"(1 2)");
}

// A recursive occurrence under another type former, spelled through *that* former's witness. The recursion still returns to this witness's own entry, one level down, with no dictionary supplied by hand.
#[test]
fn a_witness_recurses_through_another_formers_witness() {
    let source = r#"
        use /std/{Nat, Str, List, Show};
        induct Rose : pub Type | rose(Nat, List(Rose)) end
        satisfy Show(Rose) {
            show(r) =
                match r
                | rose(n, kids) =>
                    Str/concat(Nat/to_str(n), Str/concat("[", Str/concat(Show/show(kids), "]")))
                end,
        }
        /std/print(Show/show(Rose/rose(1, [Rose/rose(2, [])])))
        "#;

    assert_eq!(run(source), b"1[[2[[]]]]");
}

// One witness naming another declared after it is not recursion and must keep working: the read of the later witness asks for it, so it has elaborated by the time the first one is kept, and the module stores it first.
#[test]
fn a_witness_naming_a_later_witness_is_ordered_not_refused() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct P : pub Type | p(Nat) end
        induct Q : pub Type | q(Nat) end
        satisfy Show(P) { show(x) = match x | p(n) => Show/show(Q/q(n)) end, }
        satisfy Show(Q) { show(x) = match x | q(n) => Nat/to_str(n) end, }
        /std/print(Show/show(P/p(4)))
        "#;

    assert_eq!(run(source), b"4");
}

// Two witnesses resolving each other have no order to elaborate in: each needs the other before it can finish. Refused once, at the first of them as written, naming both and the way out — where the certifier would have said `unbound name /witness~1` about an id the reader cannot find in their own program.
#[test]
fn two_witnesses_resolving_each_other_are_refused_with_the_way_out() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct A : pub Type | a(Nat) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        satisfy Show(A) { show(x) = match x | a(n) => Nat/to_str(n) | ab(y) => Show/show(y) end, }
        satisfy Show(B) { show(x) = match x | b(n) => Nat/to_str(n) | ba(y) => Show/show(y) end, }
        /std/print(Show/show(A/ab(B/ba(A/a(5)))))
        "#;

    let report = error(source);
    assert!(
        report.contains("the witness of `Show(A)` and the witness of `Show(B)` need one another"),
        "expected the cycle named by its two concept applications:\n{report}"
    );
    assert!(
        report.contains(
            "declare witnesses that resolve through one another as one group, `satisfy ... and ...`"
        ),
        "expected the report to name the way out:\n{report}"
    );
    assert_eq!(
        report.matches("need one another").count(),
        1,
        "expected one report for the cycle, at its first member:\n{report}"
    );
}

// A definition and the witness it resolves to, each needing the other, are a cycle like two witnesses': `describe`'s goal resolves to `Show(Box)`, whose `show` calls `describe`. Refused by name in either order they are written in — where the elaborator once accepted both and certifier refusal `/describe` for ``unbound name `/witness~0` ``.
#[test]
fn a_definition_and_the_witness_it_resolves_to_are_refused_as_a_cycle_in_either_order() {
    let describe = "let describe(b: Box) -> Str = Show/show(b);";
    let witness = r#"satisfy Show(Box) { show(b) = match b.value | 0 => "empty" | _ => describe(Box { value = 0 }) end }"#;
    let program = |first: &str, second: &str| {
        fill!(
            r#"
            use /std/{Nat, Show, Str};
            pub struct Box: pub Type { value: Nat }
            ${first}
            ${second}
            /std/print(describe(Box { value = 3 }))
            "#,
            first,
            second
        )
    };

    for source in [program(describe, witness), program(witness, describe)] {
        let report = error(&source);
        assert!(
            report.contains("need one another"),
            "expected a cycle, by name:\n{report}"
        );
        assert!(
            report.contains("`describe`") && report.contains("the witness of `Show(Box)`"),
            "expected both members named:\n{report}"
        );
        assert!(
            !report.contains("unbound name"),
            "the certifier was left to refuse the cycle:\n{report}"
        );
        assert!(
            report.contains("no group holds a definition beside a witness")
                && !report.contains("`let ... and ...`"),
            "expected the way out a definition and a witness have, not a group neither fits:\n{report}"
        );
    }
}

// The way out, run: the recursion lives in one top-level group and each witness delegates to it. This is the form every witness in `/std` already takes, and it must stay the answer the refusal above points at.
#[test]
fn mutual_recursion_hoisted_into_one_group_is_admitted() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct A : pub Type | a(Nat) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        let show_a(x : A) -> Str =
            match x | a(n) => Nat/to_str(n) | ab(y) => show_b(y) end
        and show_b(x : B) -> Str =
            match x | b(n) => Nat/to_str(n) | ba(y) => show_a(y) end;
        satisfy Show(A) { show(x) = show_a(x), }
        satisfy Show(B) { show(x) = show_b(x), }
        /std/print(Show/show(A/ab(B/ba(A/a(5)))))
        "#;

    assert_eq!(run(source), b"5");
}

// The declared form: one `satisfy … and …` group, every member registered before any body elaborates, so each resolves through the other's entry and the certifier meets one recursive group.
#[test]
fn a_witness_group_resolves_its_members_through_the_table() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct A : pub Type | a(Nat) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        satisfy Show(A) { show(x) = match x | a(n) => Nat/to_str(n) | ab(y) => Show/show(y) end, }
        and Show(B) { show(x) = match x | b(n) => Nat/to_str(n) | ba(y) => Show/show(y) end, }
        /std/print(Show/show(A/ab(B/ba(A/a(5)))))
        "#;

    assert_eq!(run(source), b"5");
}

// A parameterized group: each member's premises are its own, and the group shares one universe context.
#[test]
fn a_parameterized_witness_group_resolves_under_its_premises() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct Tree(A : Type) : pub Type | node(A, Forest(A))
        and Forest(A : Type) : pub Type | nil() | cons(Tree(A), Forest(A)) end
        satisfy (@A : Type, use Show(A)) => Show(Tree(A)) {
            show(t) = match t | node(a, f) => Str/concat(Show/show(a), Str/concat("[", Str/concat(Show/show(f), "]"))) end,
        }
        and (@A : Type, use Show(A)) => Show(Forest(A)) {
            show(f) = match f | nil() => "" | cons(t, rest) => Str/concat(Show/show(t), Show/show(rest)) end,
        }
        /std/print(Show/show(Tree/node(1, Forest/cons(Tree/node(2, Forest/nil()), Forest/nil()))))
        "#;

    assert_eq!(run(source), b"1[2[]]");
}

// A member may also recurse through its own entry inside the group: the group's binder covers every member, itself included. Its knot's forcing function, initializer and built closure reach one another through a definition rather than a call, so it also holds the CPS inliner's recursion verdict to what an inline copies.
#[test]
fn a_group_member_also_resolves_through_its_own_entry() {
    let source = r#"
        use /std/{Nat, Str, Show};
        induct A : pub Type | a(Nat) | aa(A) | ab(B) and B : pub Type | b(Nat) | ba(A) end
        satisfy Show(A) {
            show(x) = match x | a(n) => Nat/to_str(n) | aa(y) => Str/concat("(", Str/concat(Show/show(y), ")")) | ab(y) => Show/show(y) end,
        }
        and Show(B) { show(x) = match x | b(n) => Nat/to_str(n) | ba(y) => Show/show(y) end, }
        /std/print(Show/show(A/aa(A/ab(B/ba(A/a(7))))))
        "#;

    assert_eq!(run(source), b"(7)");
}

/// Over a mutual `Tree` and `Forest`, a concept `Law` whose law reads its superclass `Base`, a witness of each declared as one group, and `Law(Forest)`'s `same` reading its own law at `${rest}`.
const LAWS: &str = r#"
use /std/{Nat, Eq, print};
induct Tree: pub Type | leaf(Nat) | node(Forest) and Forest: pub Type | nil() | cons(Tree, Forest) end
pub concept Base(A: Type): pub Type { size(a: A) -> Nat, }
pub concept Law(A: Type): pub Type { use Base(A), same(a: A) -> Eq()(Base/size(a), Base/size(a)), }
satisfy Base(Tree) { size(t) = match t | leaf(n) => n | node(f) => Base/size(f) end, }
and Base(Forest) { size(f) = match f | nil() => 0 | cons(t, rest) => Base/size(t) + Base/size(rest) end, }
satisfy Law(Tree) {
    same(t) =
        match t
        | leaf(_) => Eq/refl()
        | node(f) =>
            let _: Eq()(Base/size(f), Base/size(f)) = Law/same(f);
            Eq/refl()
        end,
}
and Law(Forest) {
    same(f) =
        match f
        | nil() => Eq/refl()
        | cons(t, rest) =>
            let _: Eq()(Base/size(t), Base/size(t)) = Law/same(t);
            let _: Eq()(Base/size(${rest}), Base/size(${rest})) = Law/same(${rest});
            Eq/refl()
        end,
}
print("ok")
"#;

// A group's law over a superclass reads each member's superclass through the member's entry once the member's turn finds it known to terminate: every law here recurses on a smaller value, so `Law(Tree)` reads `Law(Forest)`'s `Base` as the registered one, and `Law(Forest)` its own.
#[test]
fn a_law_over_a_superclass_reads_through_each_members_entry_in_its_group() {
    assert_eq!(run(&fill!(LAWS, rest = "rest")), b"ok");
}

// A member whose recursion does not descend stands for nothing in its group, so a read through it is refused as that, naming the member, where the conversion stuck on it would show a local the program never wrote.
#[test]
fn a_read_through_a_member_not_known_to_terminate_is_refused_naming_the_member() {
    let report = error(&fill!(LAWS, rest = "f"));
    assert!(
        report.contains("read through a member not known to terminate")
            && report.contains(
                "the witness of `Law(Forest)` is read here, and its recursion is not known to descend, so it stands for nothing"
            )
            && report.contains("its recursion is here"),
        "expected the read named by its member and that member's recursion:\n{report}"
    );
}

// A lone witness reads its own entry as a group's member is read, once its turn finds it known to terminate: a written key whose arms recurse through its own `mono` state that law over the entry's `Ord` and `Hash`, which read there as the registered ones, so each part converts with the derived order and encoding, and a map of its keys walks them in their order.
#[test]
fn a_written_key_recurses_through_its_own_entry_over_its_superclasses() {
    let source = r#"
        use /std/{Nat, Str, Hash, Map, Ord, Ordering, Eq, print};
        use /std/ops/{Eql};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        satisfy Eql(Tree) { .. }
        satisfy Ord(Tree) { .. }
        satisfy Hash(Tree) { .. }
        satisfy Hash/Key(Tree) {
            mono(a, b) =
                match a
                | leaf(n) =>
                    match b
                    | leaf(m) => Hash/Key/last(Ordering/eq(), Ord/ord(n, m), Hash/hash(n), Hash/hash(m), Hash/Key/mono(n, m))
                    | node(_, _) => Eq/refl()
                    end
                | node(l, r) =>
                    match b
                    | leaf(_) => Eq/refl()
                    | node(s, u) =>
                        Hash/Key/then(
                            Ordering/eq(),
                            Ord/ord(l, s),
                            Hash/hash(l),
                            Hash/hash(s),
                            Hash/hash(r),
                            Hash/hash(u),
                            Hash/Key/mono(l, s),
                            Ordering/then(Ordering/then(Ordering/eq(), Ord/ord(l, s)), Ord/ord(r, u)),
                            Hash/Key/last(
                                Ordering/then(Ordering/eq(), Ord/ord(l, s)),
                                Ord/ord(r, u),
                                Hash/hash(r),
                                Hash/hash(u),
                                Hash/Key/mono(r, u)))
                    end
                end,
        }
        let m = Map/of_list([
            (Tree/node(Tree/leaf(1), Tree/leaf(0)), "3"),
            (Tree/node(Tree/leaf(0), Tree/leaf(9)), "2"),
            (Tree/leaf(5), "1")]);
        print(Str/join(" ", Map/values(m)))
        "#;

    assert_eq!(run(source), b"1 2 3");
}

// A method that names no member of its witness reads through the entry as it is written: `lt` and `le` read `Ord/ord` through the entry, which is `reversed` there, so the laws that tie them to it close by reduction.
#[test]
fn a_witness_reads_its_own_method_through_its_entry_as_written() {
    let source = r#"
        use /std/{Ord, Ordering, Eq, print};
        use /std/ops/{Eql};
        induct Two: pub Type | low() | high() end
        satisfy Eql(Two) { .. }
        let reversed(x: Two, y: Two) -> Ordering =
            match x
            | low() => match y | low() => Ordering/eq() | high() => Ordering/gt() end
            | high() => match y | low() => Ordering/lt() | high() => Ordering/eq() end
            end;
        satisfy Ord(Two) {
            ord = reversed,
            lt(x, y) = Ordering/is_lt(Ord/ord(x, y)),
            le(x, y) = Ordering/is_le(Ord/ord(x, y)),
            flips(x, y) =
                match x
                | low() => match y | low() => Eq/refl() | high() => Eq/refl() end
                | high() => match y | low() => Eq/refl() | high() => Eq/refl() end
                end,
            trans(x, y, z, p, q) =
                match x
                | high() => match z | low() => /std/Bool/True/qed() | high() => /std/Bool/True/qed() end
                | low() =>
                    match z
                    | low() => /std/Bool/True/qed()
                    | high() => match y | low() => match q end | high() => match p end end
                    end
                end,
            eq(x, y, h) =
                match x
                | low() => match y | low() => Eq/refl() | high() => match h end end
                | high() => match y | low() => match h end | high() => Eq/refl() end
                end,
            is_lt(_, _) = Eq/refl(),
            is_le(_, _) = Eq/refl(),
        }
        print(match Ord/lt(Two/high(), Two/low()) | true => "reversed" | false => "declared" end)
        "#;

    assert_eq!(run(source), b"reversed");
}

// A lone witness whose recursion does not descend stands for nothing through its own entry, as a group's member does, so a law reading its superclass through it is refused naming the witness.
#[test]
fn a_lone_witness_not_known_to_terminate_reads_nothing_through_its_entry() {
    let source = r#"
        use /std/{Nat, Eq, print};
        induct Tree: pub Type | leaf(Nat) | node(Tree, Tree) end
        pub concept Base(A: Type): pub Type { size(a: A) -> Nat, }
        pub concept Law(A: Type): pub Type { use Base(A), same(a: A) -> Eq()(Base/size(a), Base/size(a)), }
        satisfy Base(Tree) { size(t) = match t | leaf(n) => n | node(l, r) => Base/size(l) + Base/size(r) end, }
        satisfy Law(Tree) {
            same(t) =
                match t
                | leaf(_) => Eq/refl()
                | node(_, _) =>
                    let _: Eq()(Base/size(t), Base/size(t)) = Law/same(t);
                    Eq/refl()
                end,
        }
        print("no")
        "#;

    let report = error(source);
    assert!(
        report.contains("read through a member not known to terminate")
            && report.contains("the witness of `Law(Tree)` is read here"),
        "expected the read named by the witness:\n{report}"
    );
}
