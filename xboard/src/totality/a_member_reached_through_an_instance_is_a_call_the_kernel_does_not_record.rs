use {
    crate::{Date, Proof, Status, Ticket, Witness, false_type, refused},
    curios_core::{Free, Level, Module, Term},
};

pub(super) const TICKET: Ticket = Ticket {
    title: "A member reached through an instance is a call the kernel does not record",
    found_on: Date::new(2026, 10, 2),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "a nullary group at `False` whose body is an instance of its own member",
        proof: Proof::Module(a_member_reached_through_an_instance),
        expect: refused![kernel: curios_cert::Error::NotDescending { .. }],
    }],
};

/// `rec bad : False = bad` with the self-reference spelled as an instance of the member rather than as a bare occurrence.
///
/// The kernel records a call for a member named at a spine head and for a bare member variable; an `Instance` node wrapping the member reaches neither rule, so the group closes with no call at all and descends vacuously.
fn a_member_reached_through_an_instance() -> (Module, Term) {
    let member = Free::local(0, Some("bad"));

    let proof = Term::rec(
        [(
            member,
            false_type(),
            Term::instance_of(&member, vec![Level::zero()]),
        )],
        Term::free_var(&member),
    );

    (Module::default(), proof)
}
