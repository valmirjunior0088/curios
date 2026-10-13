# A learner is taught by what the compiler answers

Working specification for `/learn-curios`, the command that teaches the language inside an agent's session. Today it states a stance, a fixed opening, one destination and a page of facts about the language, and it works in a checkout of this repository alone. This spec has it state the stance alone, so that the route is the learner's; holds each of its turns to what a reader takes in at a glance, so that it teaches someone who will not read a page a model wrote; and has it reach a learner who installed the compiler and never cloned it.

It is independent of every other spec. [A refusal names the form it met](../surface/200-a-refusal-names-the-form-it-met.md) and [A disequality is a fact](../arithmetic/200-a-disequality-is-a-fact.md) each remove something the command has to apologize for today.

## What this builds on

- **The command as it stands** (`.claude/commands/learn-curios.md`, 97 lines). It fixes an end — the learner has "stated a proposition about a function they wrote and proved it" — and says of the way there that "there is no lesson plan here and you must not invent one". It fixes the first program, two `Eq()` claims of which one is false, and the first question, which of them the compiler takes. It keeps the learner's work in `.learn/`, which the tree ignores, and it holds the teacher to showing the compiler's output whole, to closing a goal privately before setting it, and to never filling a hole the learner is working on.
- **What it reads is a checkout.** `documentation/syntax.md` in full, `README.md`'s taste, `curios-std/src/` for idiom, `curios/src/tests/laws.rs` for what conversion decides, and for a compiler `./target/release/curios`, then `cargo xtask compiler build`, then the installer.
- **What an installed learner has is one file.** The installer writes `~/.local/bin/curios` and nothing beside it (`xtask/templates/install.sh`). A release is tagged `release/<version>` and `curios --version` prints the version, so the reference a binary was built beside has an address. `curios document --std <dir>` writes the library's pages from the prelude the binary embeds.
- **What the compiler gives a teacher.** A written goal reports its scope, its type and what fits, every goal of a program in one run, and a program that holds goals exits 2 where a refusal exits 1; a run reports the refusals of several declarations; `curios run -` takes a program on standard input and leaves nothing behind. Each holds at `d5046c5cc`, by probe.
- **How a session holds such a command.** A command and a skill are one thing to Claude Code, and a skill is a directory: its `SKILL.md` stays in context once loaded, "so every line is a recurring token cost", and the files beside it are read when it points at them ([Skills](https://code.claude.com/docs/en/skills.md)). A plugin ships skills, and a repository is a marketplace by one file ([Plugins](https://code.claude.com/docs/en/plugins.md)). An output style applies to every response of a session and is the user's to pick; the built-in Learning style adds `Insight` blocks and leaves `TODO(human)` in the user's files, and both it and the Explanatory style "produce longer responses than Default by design" ([Output styles](https://code.claude.com/docs/en/output-styles.md)).
- **A fact lives in one place.** `.claude/rules/documentation.md` gives `.claude/` how an agent works here, `documentation/syntax.md` the surface language, and the compiler what a program means.

## The gap

- **It teaches in a checkout and nowhere else.** A learner who followed `README.md`'s "Try it" has a binary and no file the command reads, and `README.md` names the command nowhere. Building the compiler from source is what the command falls back to before the installer, and takes a C++ toolchain, CMake and several minutes.
- **It ends at a proof and nowhere else.** `$ARGUMENTS` redirects the way and not the end. A learner who came to write a program has no finish the command recognizes.
- **It opens on the form that needs the most explaining.** The first line a learner reads is `let a: Eq()(2 + 3, 5) = Eq/refl();`, whose empty list is [one parameter group per call](../../design/theory/a-call-fills-one-parameter-group.md), where the command itself says of `Eq` to "reach it late".
- **It states the language.** That linear arithmetic on `Nat` is computation, which carriers bind a hypothesis after `;`, what a program's final term is, how a goal reports: each is the reference's or the compiler's, restated. One has drifted: `Eq.crs` is "seventeen lines" in the command and twenty-three in the tree.
- **Nothing bounds a turn.** The command says to ask before explaining and to teach one thing at a time, and says nothing of how much a message holds. An agent's register in an engineering session is explanatory, and a learner who has an explanatory style selected is sent `Insight` blocks beside whatever the command asks for.
- **A session resumes from programs alone.** Files in `.learn/` are read as an earlier session; what the learner knew, where they stalled and what they asked is nowhere.
- **Done is checked for the program and not for the learner.** The proof compiled, they saw the errors, and they "can say what makes it a proof rather than a test". Nothing asks that they close a goal the teacher took no part in.

## What is known of teaching

Each line is a result and what it asks of the command.

- **Help that gives the answer harms, and help that withholds it does not.** Nearly a thousand high-school students practised mathematics with GPT-4: with a plain chat they did 48% better in practice and 17% worse on the exam taken without it; with a tutor prompted to give hints and never the answer, and handed the solutions, 127% better in practice and about level on the exam with those who had no access (Bastani et al., [Generative AI without guardrails can harm learning](https://papers.ssrn.com/abstract=4895486), PNAS 2025). The hole stays the learner's. The guarded tutor reached parity and no gain, so withholding is the floor and not the method.
- **A model asked to tutor tells.** Asked to tutor a student, ChatGPT revealed the solution in 66% of cases and gave incorrect feedback in 59%; the dataset's dialogues, between human teachers and a simulated student, average 17.3 words an utterance (Macina et al., [MathDial](https://arxiv.org/abs/2305.14536), Findings of EMNLP 2023). The second figure is what a turn is measured against.
- **Explanations are not what tutoring teaches by.** With tutors "suppressed from giving explanations and feedback" and left to prompt, "students learned just as effectively", and "What they learned from reading was limited, however, by their reading abilities" (Chi, Siler, Jeong, Yamauchi and Hausmann, [Learning from human tutoring](https://doi.org/10.1016/S0364-0213(01)00044-1), Cognitive Science 2001). A turn that is a question costs the learner nothing a paragraph would have given.
- **Learners ask for short turns, in so many words.** They want a tutor to "use short communications that guide them in small steps"; educators object to "the lack of adaptation to the learner's level and cognitive load in gen AI's 'wall-of-text' responses"; and the model built from it answers "in shorter messages that get to the point faster" (Jurenka et al., [Towards Responsible Development of Generative AI for Education](https://arxiv.org/html/2407.12687v4), 2024). Its five principles are active learning, managed cognitive load, metacognition, curiosity and adapting to the learner.
- **Where a tutor did beat a classroom, the structure was not the prompt's.** An AI tutor in a Harvard physics course taught more than an active-learning class in a median of 49 minutes, by 0.73 to 1.3 standard deviations; its authors found that a prompt alone "could not reliably provide enough structure", built the sequence into the platform, and handed the model "comprehensive, step-by-step answers" to keep it right (Kestin et al., [AI tutoring outperforms in-class active learning](https://pmc.ncbi.nlm.nih.gov/articles/PMC12179260/), Scientific Reports 2025). A command with no plan has to get both from somewhere else: here the sequence is the goals of a file the learner fills in order, and the answers are goals the teacher closed and a compiler that says so.
- **A tutor nudged toward questions asks them.** Tutors given a model's suggestions asked guiding questions more often and gave answers away less, and their students were four points likelier to master a topic, nine among the students of lower-rated tutors (Wang et al., [Tutor CoPilot](https://arxiv.org/html/2410.03017v1), 2024).
- **Expert tutors are indirect, and watch motivation beside understanding**: leading questions and hints over instruction, feedback that does not grade (Lepper and Woolverton's INSPIRE model, summarized in [these guidelines](https://www.eoas.ubc.ca/research/cwsei/resources/INSPIRE-Guidelines.pdf)).
- **Hints offered on request are clicked through unread.** In one unit of a cognitive tutor, students abused hints on 36% of their actions, stepping to the last hint, which states the answer ([Help abuse](https://learnlab.org/research/wiki/index.php/Help_abuse), citing Aleven et al. 2006). A stalled learner is handed a smaller task, never a longer hint.
- **A novice helped by a model can finish sure of what they cannot do.** Of twenty-one novices observed with generative tools, those who struggled "finished with an illusion of competence" (Prather et al., [The Widening Gap](https://arxiv.org/abs/2405.17739v1), ICER 2024). Done is something the learner does with no one beside them.
- **Predicting comes before running, and reading before writing** (PRIMM: Sentance, Waite and Kallia, summarized in [this quick read](https://static.teachcomputing.org/pedagogy/QR11-PRIMM.pdf)); the command's opening is a prediction already. **A partial program to complete**, set beside a worked example, raised success and persistence in a first programming course ([a 2025 classroom study](https://par.nsf.gov/biblio/10598698-impact-connecting-worked-examples-completion-problems-introductory-programming-practice)); a program with `?` in it is one.
- **Being asked is what makes it stay.** Practice testing and distributed practice are the two techniques of ten rated high in utility; rereading and highlighting are rated low (Dunlosky et al., [Improving Students' Learning With Effective Learning Techniques](https://www.psychologicalscience.org/news/releases/which-study-strategies-make-the-grade.html), 2013). A session that resumes opens by asking, not by recapping.
- **What made one proof assistant's material work for a newcomer was exercises a machine checks**: Coq's "comes bundled with both exercises and an autograder", where Lean's was "very choppy" with "a sudden cliff of complexity in the first chapter" ([ntietz](https://ntietz.com/blog/first-impressions-of-lean-and-coq/)). The compiler is that checker, and a goal is that exercise.
- **An instruction is followed, not enforced.** "An output style gives Claude instructions to follow. It doesn't guarantee that something always happens or never happens" (Output styles, above), and a study mode shipped as system instructions was criticized as a prompt a user talks past ([Mishra](https://punyamishra.com/2025/08/07/prompts-vs-principles-contrasting-openais-study-mode-to-real-educational-ai/)). What the command asks for is therefore measured, by the protocol below, and not assumed.

## Decisions

- **The command is as free-form as it can be made.** Settled on 2026-10-13. It states no plan, no order of topics and no script of turns.
- **It teaches a reader reluctant to sit down and read what a model writes at length.** Settled on 2026-10-13. That such a reader skims, takes the author's word for little and came to do something is this spec's reading of it, and what the first card of the protocol plays.

## What the command becomes

Recommended, and none of it settled.

- **It states a stance, a finish and its limits, and nothing else.** Four sentences carry the stance. *The compiler is the one believed*: a claim about Curios reaches the learner as output on their screen or as a run handed to them, and the teacher asserts nothing it could have run; a learner who distrusts the author is given nothing to take on trust. *A turn is one move*: one thing for the learner to do — predict, run, change, choose — and no more prose than is read at a glance, with the compiler's output shown whole and counted as none of it; more is given when it is asked for. *A hole is theirs*: never filled while they work, and made smaller where they stall. *Nothing is set that has not been closed*: the teacher closes each goal privately first. Beside them stand what done is and where the learner's files live.
- **The lesson is a file, and the conversation points at it.** A step is a program in `.learn/` with `?` where the learner writes: the compiler lists every goal with its type and what fits, an editor shows them in place, and the program is not accepted until each is filled. What would have been an explanation is a program to predict and run. The order a plan would have given is the order of a file's goals, and the answers a plan would have held are the goals the teacher closed.
- **Done is one act the compiler checks and the teacher takes no part in.** The end is the learner's to choose among what the language is for: a proposition about a function of theirs, proved; a type under which a wrong call does not compile, as `README.md`'s taste shows; a bound a guard discharges where another language would trap. Whichever it is, the last goal is a sibling of one done together, set and then left: no turn of the teacher stands between its statement and the run that accepts it.
- **A choice is asked as a choice.** A prediction or a fork in the way is put through the session's structured question where it has one, each option a line or a snippet, answered in a keystroke. What fills a hole is never an option: that would make a recognition of a construction.
- **Where the learner stood is written down.** At each stop the teacher writes a few lines in `.learn/notes.md`: what they knew coming in, what they have closed unaided, where they stalled, what they asked sideways, what is next. A resumed session reads it and opens with a question about the last thing done.
- **What only a teacher knows is one file beside the command**, read when it bears: what a newcomer writes and what Curios writes instead — no `if`, an effect sequenced as `let _ = …!;`, one call a place of a format, bytes where text is expected, `0 < n` where a zero check is meant — what `Eq/refl()` closes so that a first theorem is not one of them, and a few openings. Each line goes when the tree stops needing it, and the file is the learning material's once that exists.
- **It runs wherever a compiler does.** In a checkout it uses the tree's compiler and reads the tree. Elsewhere it uses the `curios` on the path and reads the reference and the library's sources at that binary's tag. It ships as a skill of a plugin this repository lists, and `README.md`'s "Try it" names it.

One turn, as it would read, the compiler's lines taken from what `d5046c5cc` prints:

```text
teacher   Two claims, one of them false. Which line does the compiler refuse?
          ! curios run .learn/first.crs
compiler  type mismatch
            inferred: Eq(@Nat)(5, 5)
            expected: Eq(@Nat)(5, 6)
teacher   It worked out `2 + 3` to check you. Change one character so that both hold, and run it again.
```

## Open questions

- **How the turn's bound is stated.** By a count — three sentences, none saying again what the output says — or by its reason alone. A count is what a model holds to most reliably and is the nearest thing here to a script. Recommended: the protocol decides. Both wordings are run, and the looser is kept if it holds.
- **Whether the end widens.** The command's description is "until they have stated and proved a proposition of their own", and the proof is what sets the language apart. The two other ends are reached by a learner who would have left before a proof. Recommended: all three, the proof named as where the others lead.
- **Whether the opening program stays.** Its question is right and its first token is the hardest in the language. Recommended: the command states what an opening is — a program read in ten seconds and one question a run answers — and the file beside it holds two or three, among them one with no `Eq`.
- **A command or a skill directory, and whether it ships.** The tree keeps `.claude/commands/`. A file beside the command needs the directory form, and a learner outside a checkout needs a plugin. The tree ignores `/.agents` and `/.codex`, so other agents work here too, and whether they are handed the same text is not decided.
- **What the teacher reads with no network.** The library's pages come out of the binary; the language reference does not.
- **Whether a real session is kept.** A transcript under `.learn/`, with the learner's word, is the only evidence of teaching the tree could hold.
- **What hosts the simulated sessions.** Not read: whether `claude plugin eval` drives a learner that answers turn by turn.

## Stages

1. **The command states the stance.** The four sentences, done, and the limits; every fact of the language leaves, to the reference it restated or to the file beside it; the stale figure goes. Check, by reading: each sentence is a rule of teaching, a limit or a pointer; each path it names exists; nothing it says of the language is said by `documentation/syntax.md` or answered by a probe.
2. **A turn is one move, and the lesson is a file.** Check: the protocol's counted figures, against the command as it stands at the commit before.
3. **Done is unaided, the end is theirs, and a session leaves notes.** Check: under the protocol, each session that reaches done closes its last goal with no teacher turn before the accepting run; a session resumed from another's `.learn/` asks nothing its notes answer.
4. **It runs where a compiler does.** The directory form, the plugin, the line in `README.md`, the reference read at the binary's tag. Check: in an empty directory with the release binary on the path and no checkout in reach, the protocol's sessions reach done, and the reference read is the tag's.

## Verification

Prose and agent configuration alone change, so the tree's check is `cargo xtask workspace format`. What holds the command is a measurement, counted, taken at the commit of each stage.

**The protocol.** A session runs in an empty directory outside the tree with the release compiler of the commit under test on the path. A driver invokes the command with no argument and plays the learner from a card, one reply to each turn of the teacher, until the teacher says the session is done, the card has the learner quit, or forty replies have been given. Four cards:

- **Skims.** Reads the first two lines of the teacher's prose and every line of code and of compiler output. Replies in at most eight words. Asks for no explanation. Quits after two turns in a row that give it nothing to do.
- **Wants a program.** Writes Python for a living, came to read a number and print something, and declines a proof once.
- **Doubts.** Answers a claim with no output beside it with "how do you know?", and otherwise skims.
- **Runs ahead.** Has used a proof assistant, fills a hole at once, and asks sideways.

Counted from each transcript, by reading: the words of the teacher's prose in a turn, outside code and compiler output, as median and maximum; turns that end with nothing for the learner to do; holes the teacher filled unasked; statements about Curios with neither output nor a run beside them; goals set with no private run before them; replies before the first compiler output is on screen; whether done was reached, and whether no teacher turn stood between the last goal and the run that accepted it; and, for the first card, turns whose one thing to do stood below the second line. Each version of the command is run five times a card, the versions interleaved, and a figure is its median and span over the five.

**What it does not measure.** A simulated learner retains nothing, so the protocol holds the command to its own rules and says nothing of learning. Where a real session is kept, it is read for the same counts, and one question is asked a week on, as the experiment above asked it: a sibling goal, with no teacher.

## Rejected

- **A sequence of levels in the command**, each a step with its own hint. It is the plan the first decision rules out, and it is learning material, which is written elsewhere.
- **An output style shipped to hold the bound**, or the built-in Learning style as the vehicle. A style is the user's to pick and applies to every response of their session, a plugin that forces one overrides their choice, and the built-in one is longer by design.
- **A ladder of hints the learner asks down.** It is what is clicked through unread.
- **Options for what fills a hole.**
- **The reference carried in the command.** It is a second copy, and it is in context on every turn.

## Completion and retirement

The command holds its own contract, and the file beside it what a teacher knows; why it is shaped so, with what was rejected, goes to a decision under `documentation/design/tools/`; the protocol stays there as the measurement a reader can retake. The line in `documentation/roadmap.md` becomes a checked summary linking that decision, and this file is deleted.
