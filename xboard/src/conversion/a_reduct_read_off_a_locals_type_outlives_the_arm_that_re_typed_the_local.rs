use crate::{Date, Proof, Status, Ticket, Witness, refused};

pub(super) const TICKET: Ticket = Ticket {
    title: "A reduct read off a local's type outlives the arm that re-typed the local",
    found_on: Date::new(2026, 10, 9),
    status: Status::Fixed,
    witnesses: &[Witness {
        what: "a match on a proof fired inside an arm at the payload the arm's case states, and answered again after the arm",
        proof: Proof::Module(A_REDUCT_READ_UNDER_AN_ARMS_RETYPING),
        expect: refused![kernel: curios_cert::Error::Mismatch { .. } in entry],
    }],
};

/// `read` is the match on `p` that reads its payload back.
///
/// A match on a proposition whose indices pin its payload fired on the type its scrutinee was assumed at, so `read` was `b` wherever `p : Read(b)` and named `p` and not `b`. The `true` arm of the match on `b` re-types `p` at `Read(true)`, where `read` was `true` and `Holds(read)` `True`, and the kernel remembered that reduct under `read`. Leaving the arm puts `p` back at `Read(b)` and drops no case equation, and the reducts remembered of terms naming a local were cleared on the way out only where one was dropped. So the `qed` after the match was checked at a `Holds(read)` that still answered `True`, for every `b`, and at `false` what it proved is `Holds(false)`. A bracket that closes an arm's re-typing now clears them as the re-typing does; and the match states the instance it eliminates, so the arm's case is a substitution into it and what is remembered inside the arm is remembered of another term. After the arm `Holds(read)` is `Holds(b)`, which `qed` does not inhabit.
const A_REDUCT_READ_UNDER_AN_ARMS_RETYPING: &str = r#"
    #{absent = [/Read/mk]} induct /Read: (i: %BoolType) -> pub Prop
    | mk(x: %BoolType): (x)
    end
    ((b: %BoolType, p: #{node = InductType} /Read()(b)) => ((w: #{node = InductType} /sys/Bool/True()) => (k: /sys/Bool/Holds(#{cases = {stated = #{node = InductType} /Read()(b)}} match p: (x3, x4) => %BoolType
                    | mk(x) =>
                        x
                    end)) => k)(match b: (x5) => #{node = InductType} /sys/Bool/True()
            | false =>
                #{node = Variant} /sys/Bool/True/qed()
            | true =>
                ((h: /sys/Bool/Holds(#{cases = {stated = #{node = InductType} /Read()(b)}} match p: (x6, x7) => %BoolType
                        | mk(x2) =>
                            x2
                        end)) => #{node = Variant} /sys/Bool/True/qed())(#{node = Variant} /sys/Bool/True/qed())
            end)(#{node = Variant} /sys/Bool/True/qed()))(false, #{node = Variant} /Read/mk(false))
    "#;
