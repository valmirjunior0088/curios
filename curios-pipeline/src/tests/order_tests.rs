//! A unit stores the same module wherever its named declarations are written.

use {super::test_support::unit_of, curios_core::Sharing, curios_unit::Unit};

/// One unit's declarations, each reaching some of the others: a family, a record, and definitions over them.
const DECLARATIONS: [&str; 7] = [
    "pub induct Shape: pub Type\n| dot()\n| box(Nat, Nat)\nend",
    "pub struct Pair: pub Type {\n    first: Nat,\n    second: Nat,\n}",
    "pub let double(n: Nat) -> Nat = n + n;",
    "pub let twice: Nat = double(2);",
    "pub let area(shape: Shape) -> Nat =\n    match shape | dot() => 0 | box(w, h) => w * h end;",
    "pub let pair: Pair = Pair { first = twice, second = area(Shape/box(2, 3)) };",
    "pub let unrelated: Nat = 7;",
];

/// The unit whose source writes [`DECLARATIONS`] in the order `places` gives.
fn written_in(places: [usize; 7]) -> Unit {
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

/// The unit's erased arena, as it is stored.
fn erased(unit: &Unit) -> Vec<u8> {
    curios_archive::to_bytes(&unit.arena())
        .expect("an arena serializes")
        .to_vec()
}

/// A unit and the same declarations written in another order store the same elaborated module, the same record and the same erased arena. The order a module's items are stored in is a function of what they declare and reach (`curios_core::Module::ordered`), and nothing a declaration elaborates or erases to follows where it is written. The text stage's part is left out of the comparison, being the text.
#[test]
fn a_unit_stores_the_same_module_wherever_its_declarations_are_written() {
    let written = written_in([0, 1, 2, 3, 4, 5, 6]);

    for places in [[6, 5, 4, 3, 2, 1, 0], [3, 6, 0, 5, 2, 1, 4]] {
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
            "written as {places:?}, the erased arena differs",
        );
    }
}
