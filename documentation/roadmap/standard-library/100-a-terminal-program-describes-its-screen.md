# A terminal program describes its screen

Working specification for rebuilding `/std/Tui` around a terminal that is asked what it supports, a loop that owns its session alone, and widgets over the screen description that has landed. Three rules the tree states are broken today. [A piece of a stream is cut from the bytes its reader holds](../../../curios-std/README.md#a-piece-of-a-stream-is-cut-from-the-bytes-its-reader-holds) rejects "a branch that reads the stream and pushes its pieces onto a channel", and `Tui/run` reads its keys so. [A fallible operation returns `Try`](../../design/standard-library/a-fallible-operation-returns-try-and-bang-lifts-along-declared-edges.md) declares an operation that can fail as a `Try`, and `Tui/Session/draw` is an `Io(Session)` that drops the failure of its write. [`/std` is laid out by tier, subject and kind](../../../curios-std/README.md#std-is-laid-out-by-tier-subject-and-kind) gives one meaning one spelling, and a widget's reaction to a key and a terminal's size are each spelled two ways in `/std/Tui`.

## What is wrong

- **Saving and quitting drops the save.** `Directive` (`curios-std/src/Tui/Directive.crs`) says a directive that stops and carries work "runs the work and then stops"; `Tui/run` spawns that work into its scope and returns, and the scope cancels it. `Directive/batch([Directive/perform(save), Directive/quit()])` ran no part of `save` under a pseudo-terminal, a `save` that waits on nothing included.
- **A failed frame is dropped.** `Session/draw` discards its write's failure, where a failure raised in `Try/scope` would still release the scope: the session writes the controlling terminal it sized, and a frame it could not write is a failure its program never hears of.
- **The session is half handle, half value.** Its reader is a handle; its previous frame and size are values `draw` and `size` hand back, and `Tui/run`'s reader, ticker and loop each hold a copy that drifts from the others. A stale copy diffs against the wrong frame. `Session/last_size` is an `Io` that reads a field.
- **A field moves by character.** `Field/handle` steps its cursor a `Char` at a time and `Field/view` scrolls by each character's columns alone, so a cursor stops between a letter and its combining mark, which `Str/graphemes` keeps one cluster, and a sequence joined by ZWJs scrolls as the sum of its parts.
- **Spellings.** `Field/handle(f, Key)` and `Listing/move(l, Key/Code)` are two shapes for one reaction. A size is `w, h` in a view and a pane and `cols, rows` in `Tty/Size` and `Event/resize`.

## What this builds on

- **The terminal is one device the host gives back.** `Tty/open` opens the controlling terminal read and write, and `Tty/raw` takes it with the bytes that undo what the program sets on it: `tty_take` records them beside the termios, and `tty_give`, a closed handle, the host's `Drop` after a trap or an `exit`, and a thread the first take starts for SIGTERM, SIGHUP, SIGQUIT and SIGINT write them and restore the termios before the process ends (`curios-runtime/src/os_host.rs`).
- **The screen is a description.** `Tui/View(w, h)` is read as its tiles by `Frame/of_view` and as its cursor request by `View/cursor`; a `Pane` is a view at the size it is given, which a program's `view`, `Listing/view` and `Field/view` answer and `Pane/rows` and `Pane/cols` share an axis among. Text is drawn as `Line`s of `Glyph`s, printable by proof, a control as its Control Picture, and the renderer moves the cursor after every glyph that is not ASCII ([the screen's decision](../../../curios-std/README.md#a-screen-is-a-description-at-its-size-and-a-layout-shares-an-axis-among-panes)).
- **What the terminal sends is read apart from what it says of itself.** `Input/decode` answers the inputs — a `Key` as the kitty keyboard protocol has it, decoded alike from either encoding, a paste, a size reported in band, a change of focus — and beside them the `Reply`s to what a session asks: a mode's report, the protocol's flags, the device attributes. `Mode` names the modes a session sets, and `Session/read` answers inputs, the program's type no part of it.
- **A wait is a suspension.** `Io/race` cancels the losers where they wait, and a read through `Io/Reader` puts back what it took before every suspension, so a read under a race loses nothing ([A wait is a suspension, and only the host waits](../../design/standard-library/a-wait-is-a-suspension-and-only-the-host-waits.md)). State a branch keeps lives in a channel of one slot, as `Io/Reader` keeps its bytes.
- **The Unicode Character Database has one authority.** `curios-unicode` pins version 18.0.0 by the SHA-256 of each file its build script fetches, answers Rust's questions and hands its tables packed to `curios-text`, which bakes them into `/sys/unicode`; `/std/Char` answers the properties a segmentation and a width read, and `/std/Str` steps by extended grapheme cluster: `Str/At/next_grapheme`, `try_next_grapheme`, `try_prev_grapheme` and `Str/graphemes`.
- **A frame costs milliseconds.** At 200 by 50 tiles, building a frame from lines takes 9 ms (8 to 9), a first render 20 ms (20 to 21) and a diff of one changed row 13 ms (13 to 13): timed, the median of three runs of `Frame/of_lines` and `Tui/render` read through `time/Instant` in one release-compiled program at `34f5e7600`. So the raster stays a `Vec` of `Vec`s and the description is drawn once a frame.
- **Prior art.** The kitty keyboard protocol is detected by `CSI ? u` with a primary device attributes query as sentinel, as libvaxis detects every feature; mode 2048 reports a resize in the input stream; terminals that cluster graphemes under mode 2027 still disagree on widths, so a library cannot be exact everywhere.

## Decisions

Settled with the user on 2026-10-17.

- **The terminal is asked, not guessed.** A session queries the kitty keyboard flags and modes 2026, 2027 and 2048 with a primary device attributes query behind them, and turns on what the terminal confirmed: with kitty keys, Escape needs no timeout; with mode 2048, the size is never polled.
- **The loop owns its session alone.** `Tui/run` is one task racing a read of the session, the answers of its directives and, where the terminal has no mode 2048, a deadline for the size; the session keeps its state in a channel of one slot.
- **A directive is data.** `none`, `perform`, `batch`, `redraw`, `release` and `quit`; `quit` ends the program once the work its update issued has answered, and cancels older work still running. `release` gives the terminal back around an action and takes it again.
- **A widget with state is a type module; one without is an operation of what it answers.** `Listing(A)`, `Field` and `Focus(P)` each have `handle` and `view`; `View/text` is a view's, and `Pane/paragraph`, `Pane/border`, `Pane/window` and `Pane/wave` are a pane's.
- **What waits for a consumer.** The mouse and named regions, kitty release events and report-all keys, subscriptions, an inline mode, images, a stop and continue on `SIGTSTP`, and a terminal in the browser harness are not part of this specification.

## Stages

Each stage lands as its own commits and rewrites this specification to the work left.

1. **The session and the loop.** The handshake, the session as a handle, `Tui/run` as one task, the directives, and `drive` following a resize. Check: save and quit runs the save, a failed draw ends the run with its error, a resize arrives by either path.
2. **Widgets, and a whole program.** `Listing(A)`, `Field` over graphemes, `Focus(P)`; a program of a field and a listing in bordered panes run against the scripted terminal. Check: the program's frames, its cursor and its answer.

## Verification

`cargo xtask workspace format` and `cargo xtask workspace lint` between stages, with each stage's check by name; `cargo xtask workspace test`, `cargo xtask instrumented lint` and `cargo xtask docs build` once the last stage lands. The figures the stages may move are named, not retaken: the prelude's elaboration, the `tui` workloads of `curios/src/tests/instrumented/coordination.rs`, `host_boundary.rs` and `fixpoint.rs`, and the launcher's size.

## Rejected

- **A signal handler beside the host's restore**, or a reset the host spells itself: the bytes that undo the modes are the library's, written once.
- **A reader branch feeding a channel**, which the reader's decision rejects; **polling the size** where the terminal reports it.

## Completion and retirement

The contracts go to the doc comments of `/std/Tui`, and the rationale to `curios-std/README.md`'s decision on the screen, whose sentences on the session and the loop are rewritten by the second stage; the sentence about `Tui/Session` in "A loose printer that cannot write ends the program" goes. Once no reference to this specification remains, this file is deleted.
