//! What a later walk believes of a unit an earlier walk filed — a record withheld in part or whole, a unit compiled over its earlier step — each attack a history the kernel refuses where it holds.

use {
    super::*,
    curios_cert::{Error, Verdict},
    curios_common::{Qualifier, fill},
    curios_core::{Entrypoint, Program, Term, Universes, Zonked},
    curios_pipeline::recheck,
};

/// The declaration `/<path>`.
fn named(path: &str) -> Global {
    Global::Authored(Qualifier::from(path.split('/')))
}

/// What the kernel says of `proof`, Core text closing with a term, put at `/std/Bool/False` over the prelude and every unit `history` built, through the walk a compilation puts a program to.
fn put(history: &History, proof: &str) -> Vec<Verdict> {
    history
        .built(|predecessors| {
            let file = proof.parse::<CoreFile>().expect("the proof parses");
            let (definitions, entry) =
                to_core(&file, Some(&FormScope::over(SYNTAX, predecessors.cores())))
                    .expect("the proof reads over the units in scope");
            let program = Program {
                definitions,
                entrypoint: Entrypoint {
                    body: entry.expect("the proof closes with a term").body,
                    type_: Some(Term::instance(
                        Global::Authored(SYNTAX.proof.false_type.qualifier()),
                        Universes::default(),
                    )),
                },
            };
            let program =
                Zonked::project(&program).expect("the proof holds nothing elaboration solves");

            recheck(&program, DEFAULT_BUDGET, predecessors, &SYNTAX)
        })
        .expect("the history builds")
}

/// `loopy` loops at a type holding a proof of `False`, and `unbox` takes the proof out, so a proof unpacking `loopy(0)` has only (V)'s reading of `loopy` as partial between it and `False`.
const PARTIAL_BOX: &str = "\
use /std/{Nat};
use /std/Bool/{False};

pub induct Box: pub Type
| box(p: False)
end

pub let loopy(n: Nat) -> Box = loopy(n);

pub let unbox(b: Box) -> False =
    match b
    | box(p) => p
    end;
";

/// A proof position reaching `/lib/loopy`.
const UNPACKED: &str = "/lib/unbox(/lib/loopy(0))\n";

/// Whether `verdicts` refuse the closing term as reaching what is not known to terminate.
fn refused_as_partial(verdicts: &[Verdict]) -> bool {
    verdicts
        .iter()
        .any(|verdict| verdict.name.is_none() && matches!(verdict.error, Error::NotTotal { .. }))
}

/// A record withheld whole leaves its unit's items classified from their terms, every recursion among them one no walk typed and so partial. Mutation-checked: with `Totals::seeded` classifying none of `Globals::unclassified`, the proof is admitted.
#[test]
fn a_partial_definition_whose_record_is_withheld_whole_is_classified_partial_from_its_terms() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, PARTIAL_BOX)
        .expect("the library builds");
    history
        .withhold("lib", None)
        .expect("the record is withheld whole");

    let verdicts = put(&history, UNPACKED);

    assert!(refused_as_partial(&verdicts), "{verdicts:?}");
}

/// A record naming fewer definitions than its unit is read of none of them, so withholding the partial definition's verdict alone leaves every item, `unbox` among them, classified from its terms. Mutation-checked as the test above.
#[test]
fn a_partial_definition_whose_verdict_alone_is_withheld_is_classified_partial_from_its_terms() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, PARTIAL_BOX)
        .expect("the library builds");
    history
        .withhold("lib", Some(&[named("lib/loopy")]))
        .expect("the verdict is withheld");

    let verdicts = put(&history, UNPACKED);

    assert!(refused_as_partial(&verdicts), "{verdicts:?}");
}

/// `Neg` uses its parameter negatively.
const NEG: &str = "\
use /std/Bool/{False};

pub induct Neg(A: Type): pub Type
| mk(f: (A) -> False)
end
";

/// `Wrap` is negative in its parameter through the `Neg` it holds and in no other way.
const WRAP: &str = "\
use /lib/{Neg};

pub induct Wrap(A: Type): pub Type
| w(inner: Neg(A))
end
";

/// `Bad` holds a function out of itself, through `/lib/Neg`, and `self_app` applies one to itself: no recursion to grade, so only strict positivity stands between it and `False`.
const BAD_THROUGH_NEG: &str = r"
induct /Bad: pub Type
| c(
    n:
        #{node = InductType, universes = {levels = [0]}}
        /lib/Neg(
            #{node = InductType}
            /Bad())
)
end
let /self_app : (
    b:
        #{node = InductType}
        /Bad()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /Bad()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | c(m) =>
            #{
                result = Ambient(
                    #{node = InductType}
                    /sys/Bool/False())}
            match m
            | mk(f) =>
                f(b)
            end
        end;
/self_app(
    /Bad/c(
        (
            #{universes = {levels = [0]}}
            /lib/Neg/mk)(
            @
                #{node = InductType}
                /Bad(),
            /self_app
        )
    )
)
";

/// The same through `/wrap/Wrap`, negative only through the `Neg` it holds.
const BAD_THROUGH_WRAP: &str = r"
induct /Bad: pub Type
| c(
    n:
        #{node = InductType, universes = {levels = [0]}}
        /wrap/Wrap(
            #{node = InductType}
            /Bad())
)
end
let /self_app : (
    b:
        #{node = InductType}
        /Bad()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /Bad()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | c(m) =>
            #{
                result = Ambient(
                    #{node = InductType}
                    /sys/Bool/False())}
            match m
            | w(i) =>
                #{
                    result = Ambient(
                        #{node = InductType}
                        /sys/Bool/False())}
                match i
                | mk(f) =>
                    f(b)
                end
            end
        end;
/self_app(
    /Bad/c(
        (
            #{universes = {levels = [0]}}
            /wrap/Wrap/w)(
            @
                #{node = InductType}
                /Bad(),
            (
                #{universes = {levels = [0]}}
                /lib/Neg/mk)(
                @
                    #{node = InductType}
                    /Bad(),
                /self_app
            )
        )
    )
)
";

/// Whether `verdicts` refuse `/Bad` as not strictly positive.
fn refused_as_negative(verdicts: &[Verdict]) -> bool {
    verdicts.iter().any(|verdict| {
        verdict.name == Some(named("Bad")) && matches!(verdict.error, Error::NotPositive { .. })
    })
}

/// A family whose vector its unit's record no longer files is analysed by the walk that mounts the unit, before anything is judged, and `Bad` composes through what that analysis found. No mutation is claimed: an unfiled family answers `Mixed`, which refuses as well.
#[test]
fn a_family_whose_record_is_withheld_is_composed_through_as_its_terms_say() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, NEG)
        .expect("the library builds");
    history
        .withhold("lib", Some(&[named("lib/Neg")]))
        .expect("the family's verdict and vectors are withheld");

    let verdicts = put(&history, BAD_THROUGH_NEG);

    assert!(refused_as_negative(&verdicts), "{verdicts:?}");
}

/// The analysis of a unit mounted with no vector on record composes through the vectors of the units before it, so `Wrap`, withheld, is found negative through `Neg`, kept. No mutation is claimed, as above.
#[test]
fn a_family_whose_record_is_withheld_is_analysed_through_the_vectors_filed_before_it() {
    let mut history = History::new();
    history
        .step("lib", Written::Program, NEG)
        .expect("the library builds");
    history
        .step("wrap", Written::Program, WRAP)
        .expect("the wrapper builds");
    history
        .withhold("wrap", None)
        .expect("the wrapper's record is withheld whole");

    let verdicts = put(&history, BAD_THROUGH_WRAP);

    assert!(refused_as_negative(&verdicts), "{verdicts:?}");
}

/// A family holding a type of its own level, which `/down` takes a level lower. The nearest attack on a variance vector withheld: it holds whether the walk mounting `/lib` computes the vector or answers invariant, `Big` comparing invariantly over its record as filed too, so no mutation is claimed.
#[test]
fn a_large_familys_instance_is_not_taken_a_level_down_once_its_record_is_withheld() {
    let mut history = History::new();
    history
        .step(
            "lib",
            Written::CoreText,
            r"
#{universe_context = {parameter_count = 1}}
induct /lib/Big: pub
    #{level = u + 1}
    Type
| big(
    T:
        #{level = u}
        Type
)
end
",
        )
        .expect("the library builds");
    history
        .withhold("lib", None)
        .expect("the record is withheld whole");

    let verdicts = put(
        &history,
        r"
let /down : (
    b:
        #{node = InductType, universes = {levels = [1]}}
        /lib/Big()
) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Big() =
    (
        b:
            #{node = InductType, universes = {levels = [1]}}
            /lib/Big()
    ) =>
        b;
0
",
    );

    assert!(
        verdicts
            .iter()
            .any(|verdict| verdict.name == Some(named("down"))
                && matches!(verdict.error, Error::Mismatch { .. })),
        "{verdicts:?}"
    );
}

/// `r` loops at a type holding a proof of `False`, written as Core text so the kernel alone judges the unit.
const PARTIAL_BOX_TEXT: &str = r"
induct /app/Box: pub Type
| box(
    p:
        #{node = InductType}
        /sys/Bool/False()
)
end
#{node = Rec}
let /app/r : (n: #NatType) ->
    #{node = InductType}
    /app/Box() =
    (n: #NatType) => /app/r(n);
let /app/unbox : (
    b:
        #{node = InductType}
        /app/Box()
) ->
    #{node = InductType}
    /sys/Bool/False() =
    (
        b:
            #{node = InductType}
            /app/Box()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /sys/Bool/False())}
        match b
        | box(p) =>
            p
        end;
";

/// A baseline whose record misses a definition is no baseline, so the unit is judged whole and `r`, which no edit reaches, is classified rather than read as the baseline's record leaves it. Mutation-checked: with a baseline taken whatever its record covers (`verdicts_within`'s `covers` filter dropped), the step builds.
#[test]
fn a_definition_added_beside_one_whose_verdict_is_withheld_is_judged_against_it_as_partial() {
    let mut history = History::new();
    history
        .step("app", Written::CoreText, PARTIAL_BOX_TEXT)
        .expect("the unit builds");
    history
        .withhold("app", Some(&[named("app/r")]))
        .expect("the verdict is withheld");

    let stepped = history.step(
        "app",
        Written::CoreText,
        &fill!(
            r"${unit}let /app/t :
    #{node = InductType}
    /sys/Bool/False() =
    /app/unbox(/app/r(0));
",
            unit = PARTIAL_BOX_TEXT,
        ),
    );

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("app/t")) && matches!(verdict.error, Error::NotTotal { .. })
        ),
        "{stepped:?}"
    );
}

/// `p` proves `c == 0` by `refl`, so it reads `c`'s body; the edit to `c` leaves `p`'s terms as they were. Mutation-checked: with `Changed::of` following no reader of what moved, the step builds.
#[test]
fn a_proof_unfolding_a_definition_an_edit_moved_is_judged_again() {
    let unit = |value: u8| {
        fill!(
            r"let /lib/c : #NatType = ${value};
let /lib/p :
    #{node = InductType, universes = {levels = [0], qualities = [Type]}}
    /std/Eq/Eq(#NatType)(/lib/c, 0) =
    (
        #{universes = {levels = [0], qualities = [Type]}}
        /std/Eq/Eq/refl)(@#NatType, @0);
",
            value,
        )
    };
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &unit(0))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &unit(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/p")) && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// `/app/p` unfolds `/mid/m`, a unit that does not move, into `/lib/c`, which an edit moves: both bodies were read by `p`'s judgment, so the read of `/lib` is asked again. Mutation-checked: with `read_stands` answering that every read of a unit in scope stands, the step builds.
#[test]
fn a_proof_unfolding_through_a_unit_that_did_not_move_into_one_that_did_is_judged_again() {
    let lib = |value: u8| fill!("use /std/{Nat};\n\npub let c: Nat = ${value};\n", value);
    let mut history = History::new();
    history
        .step("lib", Written::Program, &lib(0))
        .expect("the library builds");
    history
        .step(
            "mid",
            Written::Program,
            "use /std/{Nat};\nuse /lib/{c};\n\npub let m: Nat = c;\n",
        )
        .expect("the middle unit builds");
    history
        .step(
            "app",
            Written::CoreText,
            r"
let /app/p :
    #{node = InductType, universes = {levels = [0], qualities = [Type]}}
    /std/Eq/Eq(#NatType)(/mid/m, 0) =
    (
        #{universes = {levels = [0], qualities = [Type]}}
        /std/Eq/Eq/refl)(@#NatType, @0);
",
        )
        .expect("the proof's unit builds");

    let stepped = history.step("lib", Written::Program, &lib(1));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { mount, why: Unbuilt::Unmade(Unmade::Kernel(verdict)) })
                if mount == "app"
                    && verdict.name == Some(named("app/p"))
                    && matches!(verdict.error, Error::Mismatch { .. })
        ),
        "{stepped:?}"
    );
}

/// The unit of `t`, which unpacks `w(0)` at `Q`: `w` total and `Q` true, or `w` looping at the same type and `Q` false. `t`'s terms are the same in both, and it reads `w` and `Q` by their signatures alone.
fn verdict_moved(looping: bool) -> String {
    let (q, w) = match looping {
        false => (
            "/sys/Bool/True()",
            r"let /lib/w : (n: #NatType) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Box(/lib/Q) =
    (n: #NatType) =>
        (
            #{universes = {levels = [0]}}
            /lib/Box/box)(
            @
                #{node = InductType}
                /sys/Bool/True(),
            /sys/Bool/True/qed()
        );
",
        ),
        true => (
            "/sys/Bool/False()",
            r"#{node = Rec}
let /lib/w : (n: #NatType) ->
    #{node = InductType, universes = {levels = [0]}}
    /lib/Box(/lib/Q) =
    (n: #NatType) => /lib/w(n);
",
        ),
    };

    fill!(
        r"#{universe_context = {parameter_count = 1}}
induct /lib/Box(
    P:
        #{level = u}
        Prop
): pub
    #{level = u}
    Type
| box(p: P)
end
let /lib/Q : Prop =
    #{node = InductType}
    ${q};
#{universe_context = {parameter_count = 1}}
let /lib/unbox : (
    @P:
        #{level = u}
        Prop,
    b:
        #{node = InductType, universes = {levels = [u]}}
        /lib/Box(P)
) -> P =
    (
        @P:
            #{level = u}
            Prop,
        b:
            #{node = InductType, universes = {levels = [u]}}
            /lib/Box(P)
    ) =>
        #{result = Ambient(P)}
        match b
        | box(p) =>
            p
        end;
${w}let /lib/t : /lib/Q =
    (
        #{universes = {levels = [0]}}
        /lib/unbox)(@/lib/Q, /lib/w(0));
",
        q,
        w,
    )
}

/// The edit moves `w`'s verdict and `Q`'s body and nothing `t` reads but `w`'s verdict, which a signature's digest holds.
#[test]
fn a_proof_reaching_a_definition_whose_verdict_alone_moved_is_judged_again() {
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &verdict_moved(false))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &verdict_moved(true));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/t")) && matches!(verdict.error, Error::NotTotal { .. })
        ),
        "{stepped:?}"
    );
}

/// The unit of `f`, which covers `T` with the arm `a` alone and answers `Q(t)`, whose one case is at `a`: `T` holds `a`, or `a` and `b`. `f`'s terms are the same in both, so only `T`'s registry entry, read where `f` matches, moved.
fn covered(cases: &str) -> String {
    fill!(
        r"induct /lib/T: pub Type
${cases}end
induct /lib/Q: (
    t:
        #{node = InductType}
        /lib/T()
) -> pub Prop
| qa(): (/lib/T/a())
end
let /lib/f : (
    t:
        #{node = InductType}
        /lib/T()
) ->
    #{node = InductType}
    /lib/Q()(t) =
    (
        t:
            #{node = InductType}
            /lib/T()
    ) =>
        #{
            result = Ambient(
                #{node = InductType}
                /lib/Q()(t))}
        match t
        | a() =>
            /lib/Q/qa()
        end;
",
        cases,
    )
}

/// Adopted, `f(T/b())` would be a `Q(T/b())`, which has no case. Mutation-checked: with `Baseline::entries_alike` answering alike whatever the entries, the step builds.
#[test]
fn a_match_over_a_family_an_edit_gave_a_case_is_judged_again() {
    let mut history = History::new();
    history
        .step("lib", Written::CoreText, &covered("| a()\n"))
        .expect("the unit builds");

    let stepped = history.step("lib", Written::CoreText, &covered("| a()\n| b()\n"));

    assert!(
        matches!(
            &stepped,
            Err(Stopped { why: Unbuilt::Unmade(Unmade::Kernel(verdict)), .. })
                if verdict.name == Some(named("lib/f")) && matches!(verdict.error, Error::MissingArm { .. })
        ),
        "{stepped:?}"
    );
}
