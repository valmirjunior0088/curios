//! A type declared under a `use` premise, and the heads that take a call's marks.

use crate::tests::{error, run};

// A struct over `use Key(K)` is built, read and witnessed at the registered dictionary: the bare literal takes it from its expected type, the applied head and the signatures resolve it, a field's type resolves through the premise, and the witness over the type states the premise it is keyed under.
#[test]
fn a_struct_under_a_premise_is_built_read_and_witnessed() {
    let source = r#"
        use /std/{Nat, Bool, Str, Show, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V,
            agrees: (K) -> Bool
        }
        let slot(@K: Type, use Key(K), @V: Type, key: K, value: V) -> Slot(K, V) =
            Slot { key = key, value = value, agrees = (other) => Key/same(key, other) };
        let holds(@K: Type, use Key(K), @V: Type, at: Slot(K, V), key: K) -> Bool =
            Key/same(at.key, key);
        satisfy (@K: Type, use Key(K), @V: Type, use Show(V)) => Show(Slot(K, V)) {
            show(at) = Show/show(at.value)
        }
        let one: Slot(Nat, Str) = slot(1, "one");
        let written: Slot(Nat, Str) = Slot { key = 2, value = "two", agrees = (other) => false };
        let headed = Slot(Nat, Str) { key = 3, value = "three", agrees = (other) => true };
        print(Str/concat(
            Str/concat(Bool/to_str(holds(one, 1)), Bool/to_str(holds(written, 1))),
            Str/concat(Show/show(one), Show/show(headed))))
        "#;

    assert_eq!(run(source), b"truefalseonethree");
}

// An inductive's `use` parameter is a witness slot of its value constructors too: each resolves the dictionary the expected type names, and the recursive payload's type, which leaves the slot out, resolves to the constructor's own premise.
#[test]
fn an_inductive_under_a_premise_is_built_and_matched() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub induct Chain(K: Type, use Key(K), V: Type): pub Type
        | done()
        | link(K, V, Chain(K, V))
        end
        let find(@K: Type, use Key(K), @V: Type, chain: Chain(K, V), key: K, otherwise: V) -> V =
            match chain
            | done() => otherwise
            | link(at, value, rest) => choose
                | Key/same(at, key) => value
                | _ => find(rest, key, otherwise)
                end
            end;
        let chain: Chain(Nat, Str) = Chain/link(1, "one", Chain/link(2, "two", Chain/done()));
        print(Str/concat(find(chain, 2, "none"), find(chain, 3, "none")))
        "#;

    assert_eq!(run(source), b"twonone");
}

// Reading a value leaves the dictionary where it is: a struct pattern's head infers it from the scrutinee's type, and a spread copies the fields under the parameters its expected type states.
#[test]
fn a_struct_under_a_premise_is_destructured_and_updated() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V
        }
        let value_of(@K: Type, use Key(K), @V: Type, at: Slot(K, V)) -> V =
            let Slot { key = key, value = value } = at;
            value;
        let swap(@K: Type, use Key(K), @V: Type, at: Slot(K, V), value: V) -> Slot(K, V) =
            Slot { ..at, value = value };
        let one: Slot(Nat, Str) = Slot { key = 1, value = "one" };
        print(Str/concat(value_of(one), value_of(swap(one, "uno"))))
        "#;

    assert_eq!(run(source), b"oneuno");
}

// A derivation reads the fields and the payloads, which a premise is not among, so a derived witness over such a type states the premise in its telescope and is otherwise the one any type gets.
#[test]
fn a_type_under_a_premise_derives() {
    let source = r#"
        use /std/{Nat, Bool, Str, Spell, print};
        use /std/ops/{Eql};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V
        }
        pub induct Chain(K: Type, use Key(K), V: Type): pub Type
        | done()
        | link(K, V, Chain(K, V))
        end
        satisfy (@K: Type, use Key(K), @V: Type, use Eql(K), use Eql(V)) => Eql(Slot(K, V));
        satisfy (@K: Type, use Key(K), @V: Type, use Spell(K), use Spell(V)) => Spell(Slot(K, V));
        satisfy (@K: Type, use Key(K), @V: Type, use Spell(K), use Spell(V)) => Spell(Chain(K, V));
        let one: Slot(Nat, Str) = Slot { key = 1, value = "one" };
        let chain: Chain(Nat, Str) = Chain/link(1, "one", Chain/done());
        print(Str/concat(Bool/to_str(one == one), Str/concat(Spell/spell(one), Spell/spell(chain))))
        "#;

    assert_eq!(
        run(source),
        b"trueSlot { key = 1, value = \"one\" }Chain/link(1, \"one\", Chain/done())"
    );
}

// The dictionary is an argument of the type, compared as any argument is: a value under the registered one is not a value under another, and the report writes each under its mark.
#[test]
fn two_dictionaries_make_two_types() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V
        }
        let other: Key(Nat) = Key { same(a, b) = false };
        let one: Slot(Nat, Str) = Slot { key = 1, value = "one" };
        let wrong: Slot(Nat, use other, Str) = one;
        print("no")
        "#;

    let message = error(source);
    assert!(message.contains("type mismatch"), "got: {message}");
    assert!(
        message.contains("inferred: Slot(Nat, use Key { (a, b) => a == b }, Str)"),
        "got: {message}"
    );
    assert!(
        message.contains("expected: Slot(Nat, use Key { (a, b) => false }, Str)"),
        "got: {message}"
    );
}

// A type is printed with its dictionary only where resolution would not restore it, so at the registered one it reads as it was written.
#[test]
fn a_type_at_the_registered_dictionary_prints_as_written() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        satisfy Key(Nat) {
            same(a, b) = a == b
        }
        pub struct Slot(K: Type, use Key(K), V: Type): pub Type {
            key: K,
            value: V
        }
        let one: Slot(Nat, Str) = Slot { key = 1, value = "one" };
        let seen: ? = one;
        print("no")
        "#;

    let message = error(source);
    assert!(message.contains("? = Slot(Nat, Str)"), "got: {message}");
}

// A concept's premise is a superclass field. A `use` parameter would index the concept by a dictionary, which no witness key reads, and the refusal names the rule.
#[test]
fn a_concept_takes_no_use_parameter() {
    let source = r#"
        use /std/{Nat, Bool, Str, print};
        pub concept Key(K: Type): pub Type {
            same(K, K) -> Bool
        }
        pub concept Keyed(K: Type, use Key(K)): pub Type {
            label(K) -> Str
        }
        print("no")
        "#;

    let message = error(source);
    assert!(
        message.contains(
            "concept `Keyed` takes a `use` parameter: a concept states a premise as a superclass field"
        ),
        "got: {message}"
    );
}

// A literal's applied head and a witness's concept application are calls of the type former, so a declaration over an `@` parameter has a literal and a witness: `Box(@Nat) { … }` is the type `Box(@Nat)` written before its fields, and `satisfy Named(@Nat)` registers at `Nat`.
#[test]
fn a_literal_head_and_a_witness_head_take_a_calls_marks() {
    let source = r#"
        use /std/{Nat, Str, print};
        pub struct Box(@A: Type): pub Type {
            value: A
        }
        pub concept Named(@A: Type): pub Type {
            name: Str
        }
        satisfy Named(@Nat) {
            name = "nat"
        }
        let box = Box(@Nat) { value = 1 };
        let bare: Box(@Nat) = Box { value = 2 };
        print(Str/concat(Named/name(@Nat), Nat/to_str(Nat/add(box.value, bare.value))))
        "#;

    assert_eq!(run(source), b"nat3");
}

// A head is checked as the application it is: a plain argument where the former declares an implicit parameter is one argument too many, as it is in a signature.
#[test]
fn a_literal_head_is_held_to_the_formers_marks() {
    let source = r#"
        use /std/{Nat, Str, print};
        pub struct Box(@A: Type): pub Type {
            value: A
        }
        let box = Box(Nat) { value = 1 };
        print("no")
        "#;

    let message = error(source);
    assert!(
        message.contains("wrong number of arguments"),
        "got: {message}"
    );
}
