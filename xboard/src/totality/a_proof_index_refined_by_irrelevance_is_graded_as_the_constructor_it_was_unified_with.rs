use crate::{Date, Proof, Status, Ticket, Witness, refused};

pub(super) const TICKET: Ticket = Ticket {
    title: "A proof index refined by irrelevance is graded as the constructor it was unified with",
    found_on: Date::new(2026, 10, 30),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "a recursion that hands its own proof back, graded below the index the proof's target refined",
        proof: Proof::Module(A_RECURSION_ON_A_REFINED_PROOF_INDEX),
        expect: refused![kernel: curios_cert::Error::NotDescending { .. }],
    }],
};

/// The arm's inversion refines `w0` to `Wit/w(v)`, so the size context reads `v` below `w0`; but at a proposition `Wit/w(v)` is every proof of `Wit`, `q` is at `Q(v)` by irrelevance, and the call hands `f` the very `q` it matched, whose payload is `v` again.
const A_RECURSION_ON_A_REFINED_PROOF_INDEX: &str = r#"
    #{absent = [/Wit/base, /Wit/w]} induct /Wit: pub Prop
    | base()
    | w(inner: #{node = InductType} /Wit())
    end
    #{absent = [/Q/c]} induct /Q: (i: #{node = InductType} /Wit()) -> pub Prop
    | c(v: #{node = InductType} /Wit()): (#{node = Variant} /Wit/w(v))
    end
    #{node = Rec} (let f: (w0: #{node = InductType} /Wit(), q: #{node = InductType} /Q()(w0)) -> /sys/Bool/False =
        (w02: #{node = InductType} /Wit(), q2: #{node = InductType} /Q()(w02)) =>
            match q2: (x, x2) => /sys/Bool/False
            | c(v) =>
                f(v, q2)
            end;
    f(#{node = Variant} /Wit/base(), #{node = Variant} /Q/c(#{node = Variant} /Wit/base())))
    "#;
