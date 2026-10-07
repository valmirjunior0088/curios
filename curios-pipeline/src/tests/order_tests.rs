//! A unit stores the same module wherever its declarations are written.

use {super::test_support::unit_of, curios_core::Sharing, curios_unit::Unit};

/// One unit's declarations, each reaching some of the others: a family, a record, definitions over them, and a concept with two witnesses and a definition that resolves through both.
const DECLARATIONS: [&str; 11] = [
    "pub induct Shape: pub Type\n| dot()\n| box(Nat, Nat)\nend",
    "pub struct Pair: pub Type {\n    first: Nat,\n    second: Nat,\n}",
    "pub let double(n: Nat) -> Nat = n + n;",
    "pub let twice: Nat = double(2);",
    "pub let area(shape: Shape) -> Nat =\n    match shape | dot() => 0 | box(w, h) => w * h end;",
    "pub let pair: Pair = Pair { first = twice, second = area(Shape/box(2, 3)) };",
    "pub let unrelated: Nat = 7;",
    "pub concept Measured(A: Type): pub Type {\n    measure(A) -> Nat,\n}",
    "satisfy Measured(Shape) {\n    measure(shape) = area(shape),\n}",
    "satisfy Measured(Pair) {\n    measure(pair) = pair.first + pair.second,\n}",
    "pub let measured: Nat = Measured/measure(Shape/box(2, 3)) + Measured/measure(pair);",
];

/// The unit whose source writes [`DECLARATIONS`] in the order `places` gives.
fn written_in(places: [usize; 11]) -> Unit {
    let mut source = String::from("use /std/{Nat};\n");
    for place in places {
        source.push('\n');
        source.push_str(DECLARATIONS[place]);
        source.push('\n');
    }

    unit_of(&source)
}

/// The names a unit's items declare first, in the order it stores them.
fn stored_order(unit: &Unit) -> Vec<String> {
    unit.core()
        .items
        .iter()
        .map(|item| item.describe())
        .collect()
}

/// The unit's elaborated module, consed up to spelling under no position, as it is stored.
fn elaborated(unit: &Unit) -> Vec<u8> {
    curios_archive::to_bytes(&unit.core().shared(&Sharing::new()))
        .expect("a module serializes")
        .to_vec()
}

/// The unit's items' erased forms, as they are stored.
fn erased(unit: &Unit) -> Vec<u8> {
    curios_archive::to_bytes(unit.erased())
        .expect("an erased unit serializes")
        .to_vec()
}

/// A unit and the same declarations written in another order store the same elaborated module, the same record and the same erased forms. The order a module's items are stored in is a function of what they declare and reach (`curios_core::Module::ordered`), and nothing a declaration elaborates or erases to follows where it is written: a witness among them, which is named by what it witnesses (`curios_core::WitnessId`) and so is the same declaration wherever its module writes it. The text stage's part is left out of the comparison, being the text.
#[test]
fn a_unit_stores_the_same_module_wherever_its_declarations_are_written() {
    let written = written_in([0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]);

    for places in [
        [10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0],
        [3, 9, 6, 0, 10, 5, 8, 2, 7, 1, 4],
    ] {
        let moved = written_in(places);

        assert_eq!(
            stored_order(&written),
            stored_order(&moved),
            "written as {places:?}, the items are stored in another order",
        );
        assert!(
            elaborated(&written) == elaborated(&moved),
            "written as {places:?}, the elaborated module differs",
        );
        assert_eq!(
            written.certification(),
            moved.certification(),
            "written as {places:?}, the certifier's record differs",
        );
        assert!(
            erased(&written) == erased(&moved),
            "written as {places:?}, the erased forms differ",
        );
    }
}
