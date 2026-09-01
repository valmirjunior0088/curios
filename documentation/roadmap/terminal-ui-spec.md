# A terminal program draws a screen and reads keys

## Status

Researched and probed, not designed. This file records a survey of twelve terminal-interface libraries across nine languages, the distance between what they agree on and `/std` — read from source on 2026-09-01 — the smallest architecture that closes it, the one mechanism the tree's compiler was probed to support (every claim marked *probed* was elaborated through `wonder`, and *run* where it says so), where dependent types earn their place and where they do not, and the decisions still open. The module keeps its own contract in `curios-prelude-archive/std/Tui.crs` once written, and nothing here restates one. Nothing is started.

## Why it exists

`/std` can write bytes to a terminal and read a line from it, and nothing more. A program that wants a screen spells escape sequences by hand, cannot see a keystroke before Enter because the terminal is never taken out of canonical mode, cannot learn how wide the terminal is, and has no vocabulary for a key, a cell, a style or a box. Every interactive program — a menu, a picker, a progress view, a REPL with editing — is therefore not writable in Curios today, and the language's own tooling has no surface to be interactive on.

## What the survey settles

Ratatui and cursive (Rust), Bubble Tea (Go), brick and vty (Haskell), Notty (OCaml), Textual (Python), Ink and OpenTUI (TypeScript), FTXUI (C++), libvaxis (Zig), tview and tcell (Go), and termbox were read for what each ships and how it is structured. A capability is in the tier below when every one of them has it and a first program meets its absence within an hour.

**What every one of them has.**

- A session bracket: raw mode on, alternate screen entered, cursor hidden, and every one of those undone on exit, whether the exit was orderly or not.
- A cell-grid double buffer: the program draws a whole frame into memory, the library diffs it against the previous frame and writes only the cells that changed. Ratatui, tcell, notcurses, OpenTUI and libvaxis state this as the design; Ink diffs at the row level and Bubble Tea at the line level, and those two are the ones whose users report flicker.
- A key vocabulary: a key is a code — a character or a named key — with modifiers; resize is an event; paste is an event where bracketed paste is enabled.
- Styles: foreground, background, and the six SGR attributes, over the sixteen named colors, the 256-color cube and 24-bit color.
- Text measurement: a string's width in cells, which is not its length in characters.
- A box layout: split an area along one axis into parts sized by a fixed length, a percentage, a minimum, or what is left.
- A small widget set: styled wrapped text, a bordered box with a title, a list with a selection, a single-line text input, and a scrolling viewport. Table, tabs, gauge and spinner come next in most and are universal in none.
- An in-memory backend so a frame can be asserted in a test without a terminal.

**Two families, and which fits a pure language.** The app-owned loop with immediate rendering (ratatui, termbox, Notty, libvaxis's low level) leaves the event loop, the state and the redraw to the program; the framework-owned loop hands the program a model, an update and a view (Bubble Tea, brick) or a retained widget tree (Textual, cursive, tview, Ink, libvaxis's vxfw). A retained tree is object identity plus mutation and is the wrong shape for this language. Model–update–view is the natural shape of a pure program over `Io`, brick is the precedent that it works in a pure language, and Notty is the precedent that the drawing half is an algebra of values rather than commands to a screen. Notty's core was designed after vty's, brick is built on vty, and ratatui's buffer diff is the same idea with mutation: they converge.

**What the framework family gets wrong, so the MVP does not.** Bubble Tea's recorded pain is nested components, message routing and focus, and having no layout — the program concatenates strings and hopes. So the layout split and a bounded focus index are in the MVP, and nesting is left to the program: a widget is a pure function from its state and its size to an image, the model is the program's own type, and composition is function composition rather than a framework's routing.

**What the terminal itself gets wrong, and what the MVP does about it.** Legacy input encoding cannot distinguish a lone Escape from the start of a sequence, `Ctrl+I` from Tab, or `Shift+Enter` from Enter; the kitty keyboard protocol fixes it and is spoken by kitty, ghostty, WezTerm, foot, Alacritty, iTerm2 and Windows Terminal, while tmux and most others still speak the legacy encoding. Cell width has no exact answer: `wcwidth` is per code point, emoji sequences and combining marks break it, and Mode 2027 lets a terminal say it clusters graphemes. Resize reaches a process as a signal, which the ABI has no shape for. Each has a bounded answer below.

## What is certain

Read from source, and probed where it says so.

- **The host rows are item 10 of [the indispensable tier](standard-library-indispensable-tier-spec.md), and so is every host fact beneath them.** That the ABI has no terminal row, that `poll` accepts stdin while `set_nonblocking` on it is a no-op, that stdin is served through a buffer `poll` cannot see, that termios is the whole of raw mode on the two release targets, and that the browser harness holds stdin at EOF are recorded there once; this file writes over `/sys/tty` and states nothing about the host.
- **`Async/read` reads before it waits.** Its first step is `/sys/Handle/read`, and only a `would_block` status sends it to `wait`; on stdin that first read blocks the whole scheduler until a byte arrives. A terminal loop spells `Async/wait(stdin, /sys/poll/read)` and then the raw read, and does not use `Async/read`.
- **The scheduler already has every piece the loop needs.** `wait`, `sleep`, `select`, `timeout`, `go`, `spawn`/`join`, `park` with a `Waker`, and `using` with release on both exits, all in `curios-prelude-archive/std/Async.crs`. Nothing about the loop is new scheduling.
- **`Str` counts scalars and measures nothing.** `Str/len` is a scalar count; there is no width, no `split`, no `lines`, no `pad`, no `of_char` — those are item 3 of [the indispensable tier](standard-library-indispensable-tier-spec.md). `Char` has no width table and no general category.
- **`Vec` has `append` at `n + m` and `map`; it cannot be indexed, zipped or built from a list** — item 4 of the same specification. `Nat/Lt` and `Nat/Le` are decided propositions with `try`, and `Str/get(s, i, ok: Nat/Lt(i, len(s)))` is the shape a bounded access takes.
- **A size-indexed image composes through `Vec` with no lemma** (probed, run). With `Image(w, h)` declared as `Vec(Vec(Cell, w), h)`, `beside` is a zip of `Vec/append` over rows at `Image(w1 + w2, h)`, `above` is `Vec/append` at `Image(w, h1 + h2)`, and `fit(img, w, h) -> Image(w, h)` is a `resize` written by recursion on the *target* length — `0` answers `nil`, `p + 1` takes the head or the fill and recurses — so cropping and padding are one structural function and no `Eq/subst` appears anywhere. A `text(s) -> {w: Nat, Image(w, 1)}` is built by `Str/fold` into a dependent pair. A `view(w, h, title) -> Image(w, h)` that cases on `h` and writes `above(fit(header, w, 1), blank(w, hb))` in the `hb + 1` arm elaborates — `1 + hb` converts with `hb + 1` — and runs, printing the padded header over blank rows. The oracle reports `beside(blank(4, 2), blank(4, 2))` at `Image(4 + 4, 2)`.
- **The wrong spellings are refused where they are written** (probed). `above(blank(w, 1), blank(w, h - 1))` against `Image(w, h)` reports `inferred: Vec(Vec(Cell, w), (h - 1) + 1), expected: Vec(Vec(Cell, w), h)`, which is the correct refusal since nothing says `h` is positive; casing on `h` is the idiom. `beside(blank(3, 2), blank(4, 3))` reports the height mismatch at the second argument.
- **One elaborator wart sits on the natural spelling** (probed). `let t = text(title); fit(t.img, w, 1)` is refused with a type mismatch whose inferred side has unfolded `t` into `text`'s whole fold, so the implicit width is never solved from `t.0`; the same term passes when the pair arrives as a parameter, as a lambda argument, or with `@t.w` written explicitly. A finding for `curios-elab`, worked around in the library by taking the pair as a parameter.
- **`Fmt` is the precedent for a type computed from a runtime value, and `Vec` for a size in an index.** Nothing in `/std` yet indexes a two-dimensional structure.
- **A test is a declaration and a property draws its arguments.** A `view` that is a pure function of a model and a size is asserted by `Test/equal` on the image, and an input decoder that is a pure function of bytes is a property over drawn `Bytes`.
- **The roadmap's "Terminal" under IO is the byte stream, and the indispensable tier deliberately leaves out a `Draw` derivation slot.** No item names a screen.

## The design

Six layers, pure except the lowest and the highest. Names are proposals.

### 1. The host floor

`/sys/tty/raw` and `/sys/tty/size` are item 10 of [the indispensable tier](standard-library-indispensable-tier-spec.md), with the rows' semantics, the four-place obligation, and the rejected alternatives to a size row and to a resize signal. Nothing in this design touches the host except through them, and `Term` below is where they are wrapped.

### 2. Values

- `Color`: `default()`, `ansi(Nat)` for the sixteen, `indexed(Byte)`, `rgb(Byte, Byte, Byte)`.
- `Style`: `fg`, `bg`, and `bold`, `dim`, `italic`, `underline`, `reverse`, `strike` as `Bool`; `Style/plain`, with the rest written as struct updates.
- `Cell`: a `symbol: Str` of width one or two — one base scalar followed by any zero-width scalars — and a `Style`; `Cell/blank` is a space in `Style/plain`. A wide symbol occupies its cell and the next, which holds a continuation marker the renderer skips. This is ratatui's and notcurses's cell, chosen over a `Char` cell because a combining mark has nowhere else to live.
- `Text/width(s: Str) -> Nat` over a small range table: zero for combining marks, variation selectors and the zero-width joiner; two for the East Asian Wide and Fullwidth blocks and emoji presentation; one otherwise. It is approximate and says so; `Text/wrap(s, w) -> List(Str)` and `Text/truncate(s, w)` are written over it.

### 3. Images

The drawing half is Notty's algebra with the size in the type, as probed above:

```
pub let Image(w: Nat, h: Nat) -> Type = Vec(Vec(Cell, w), h);

pub let blank(w: Nat, h: Nat) -> Image(w, h);
pub let text(style: Style, s: Str) -> {w: Nat, img: Image(w, 1)};
pub let beside(@w1, @w2, @h, a: Image(w1, h), b: Image(w2, h)) -> Image(w1 + w2, h);
pub let above(@w, @h1, @h2, a: Image(w, h1), b: Image(w, h2)) -> Image(w, h1 + h2);
pub let fit(@w0, @h0, img: Image(w0, h0), w: Nat, h: Nat) -> Image(w, h);
pub let overlay(@w, @h, @w1, @h1, base: Image(w, h), top: Image(w1, h1), x: Nat, y: Nat) -> Image(w, h);
pub let restyle(@w, @h, f: (Style) -> Style, img: Image(w, h)) -> Image(w, h);
```

`beside` and `above` refuse a mismatched join where it is written rather than padding it silently. `fit` is the one coercion — crop what is too large, pad what is too small, by recursion on the target — and it is what a text of runtime width is passed through before it is joined with anything. `overlay` clips `top` to `base`, so it needs no bound. A frame of height `h` that stacks a fixed header over the rest cases on `h`, as the probe does; the spelling `h - 1` is refused because it is wrong.

`Layout/split(len: Nat, parts: List(Constraint)) -> List(Nat)`, with `Constraint` as `length(Nat)`, `min(Nat)`, `percent(Nat)` and `fill(Nat)`, distributes a length along one axis: lengths and minimums first, percentages next, the remainder shared among fills by weight, in a deterministic pass with no solver; the sizes sum to `len`. It answers `Nat`s rather than proofs because `fit` at each part absorbs any composition, so the sum is a fact the value has and the type does not need.

### 4. Input, and the renderer

Both pure, both tested without a terminal.

```
pub induct Code: pub Type
| char(Char) | enter() | escape() | backspace() | tab() | delete() | insert()
| up() | down() | left() | right() | home() | end() | page_up() | page_down() | f(Nat)
end
pub struct Key: pub Type { code: Code, shift: Bool, alt: Bool, ctrl: Bool }
pub induct Event(E: Type): pub Type
| key(Key) | resize(cols: Nat, rows: Nat) | paste(Str) | custom(E)
end

pub let decode(input: Bytes) -> {keys: List(Key), pasted: List(Str), rest: Bytes, pending_escape: Bool};
pub let render(@w, @h, previous: Option(Image(w, h)), next: Image(w, h), cursor: Option({Nat, Nat})) -> Bytes;
```

`decode` understands the legacy encoding — control bytes, `CSI A`–`D` and the `~` family, `SS3`, an Escape prefix as Alt, the bracketed-paste envelope — and the kitty `CSI u` form, so on a terminal that accepted the protocol every key arrives unambiguous and on one that did not the legacy rules apply. A lone Escape at the end of a chunk is `pending_escape`, and the loop settles it by waiting a short interval for a continuation, which is what every surveyed library does and the only place time enters the decoder's contract. `render` walks two images of the same type row by row and emits a cursor move and an SGR change only where a cell differs, wrapped in synchronized-output begin and end (`CSI ? 2026 h`/`l`), which a terminal that lacks the mode ignores; with no previous image it emits the whole frame. Because both images carry the same `w` and `h`, the walk has no bound to check.

### 5. The session and the loop

`Term` is the effectful floor: `enter` puts stdin in raw mode, enters the alternate screen (`CSI ? 1049 h`), hides the cursor, enables bracketed paste (`CSI ? 2004 h`) and pushes the kitty flag `1` (`CSI > 1 u`); `leave` undoes each in reverse, and it is what `using` releases on both scheduler exits. `Term/size`, `Term/draw` and `Term/read` wrap `/sys/tty`, the decoder and the renderer, so a program that wants its own loop has one.

`App` is the framework on top — brick's record in Curios's types:

```
pub struct App(M: Type, E: Type): pub Type {
    init: {M, Cmd(E)},
    update(model: M, event: Event(E)) -> {M, Cmd(E)},
    view(model: M, w: Nat, h: Nat) -> Image(w, h),
    cursor(model: M, w: Nat, h: Nat) -> Option({Nat, Nat}),
}
pub induct Cmd(E: Type): pub Type
| none() | quit() | perform(Async(E)) | batch(List(Cmd(E)))
end
pub let run(@M: Type, @E: Type, app: App(M, E)) -> Async(M);
```

`run` holds one queue of events in a `Cell` and one `Waker`. Three kinds of fiber feed it: the reader, which waits on stdin, reads a chunk, decodes, and pushes; the ticker, which sleeps a fixed interval, reads `/sys/tty/size`, and pushes a `resize` only when it changed; and one fiber per `perform`, which runs its `Async` and pushes the result as `custom`. The loop parks until the queue is non-empty, drains it through `update`, and if anything arrived draws `view` at the current size once — so a burst of keys costs one frame. `quit` returns the model. `view` is typed at the size it is given, so a frame that does not fill the terminal is a type error, and `run` never crops.

### 6. Widgets

Each is a pure function of its state and its size, and each state type is the program's to hold in its model: `Border/around(title, img)` at `Image(w + 2, h + 2)`; `Paragraph/draw(style, text, w, h)`; `Listing` with `select`, `move(key)` and `draw(w, h)`, its selection bounded by its length; `Input` with `handle(key)` and `draw(w)`, its cursor bounded by its text; `Viewport/draw(offset, img, w, h)`, which is a crop followed by `fit`. That is the intersection of every surveyed set and nothing beyond it.

## Where dependent types help, and where they do not

The question was put to the survey and the literature directly. They help at three places, each already the language's idiom, and the two heavier uses in the literature are not what a library user wants.

**They help.**

- **A size in the image's type.** `Image(w, h)` is `Vec` twice, and it does what `Vec` does: `beside` and `above` refuse a mismatched join, `view` must produce exactly the frame it was asked for, and the renderer's diff needs no bounds. The class of bug this removes — a column that does not fill its pane, a widget that overflows it, an off-by-one border — is the class every surveyed issue tracker is full of, and Notty and brick both pad it away silently. The probe shows the cost: one coercion, `fit`, at every runtime-sized value, and a `match` where a frame is split by a fixed part.
- **A bound in the type where the value already decides it.** A cursor at `(x, y)` under `Nat/Lt(x, w)` and `Nat/Lt(y, h)`, a cell read under the same, a listing's selection under `Nat/Lt(i, len)`: each is `Str/get`'s shape, discharged by reduction on a literal and by `try` on a computed value, and it is what keeps `Listing/move` from ever selecting past the end.
- **A focus that cannot dangle.** Adelsberger, Setzer and Walkingshaw's Agda GUI library (PPDP 2018) types a frame's handler at the frame's own button count, so "button `i` was pressed" is a `Fin n` and a frame with no buttons takes no events. The same shape here is a focus index bounded by the number of focusable widgets the model declares, which Bubble Tea programs manage by hand and get wrong.

**They do not, for this tier.**

- **A typestate for the session.** Idris's `Control.ST` would type `enter` and `leave` as transitions on a resource so a draw before `enter` or after `leave` is refused. `using` brackets the session once and releases it on both exits; every draw happens inside the bracket by construction, and an indexed monad beside `Async` would be a second sequencing discipline for a property one function already holds.
- **A handler whose type depends on the GUI.** The Agda library's contribution is proving properties of interaction sequences — that a state is unreachable without passing another — which is a proof-assistant use over one application, not a facility a library ships. `update` stays a plain function; a program that wants the proof writes its `Event` type narrowly and proves over it, which Curios already permits.
- **A proof that the layout sums.** `split` could return the evidence that its parts sum to the length and a fold of `above` could carry it into `Image(w, h)`. That is `Eq/subst` through `Vec/append`'s `n + m` for a fact `fit` makes irrelevant at the join. Not in this tier.
- **Cell widths.** No type helps a table that is approximate by nature; the width is a runtime `Nat` and the image's index is computed from it.

## Decisions to take

Each names the alternatives and what the recommendation rests on.

1. **Sized images, or Notty's implicit padding.** Recommended: sized. It is `Vec`'s idiom, it is the one place the type system pays for itself in this library, the probe shows the composition costs no proof, and the escape hatch is one function.
2. **Which loop is public.** Recommended: both `Term` and `App`, as ratatui exposes the terminal and tui-realm the framework, so a program with its own loop is not refused.
3. **The resize tick.** Recommended: 100 ms — one `ioctl`, and what every library does on the platform without signals. That the size is a row and resize is not a signal is decided in the indispensable tier, not here.
4. **Kitty keyboard protocol in the MVP.** Recommended: yes, the disambiguation flag alone, since the legacy decoder must exist regardless and the protocol's form is one extra arm.
5. **Mouse.** Recommended: out. It is the one capability the survey splits on, and it brings hit-testing, which the image algebra has no rectangles for.
6. **Width by a small table, or every scalar at one.** Recommended: the table, under twenty ranges, with the approximation stated; `Str` decomposition (item 3 of the indispensable tier) lands first.
7. **Cell symbol as `Str` or `Char`.** Recommended: `Str`, for combining marks.
8. **The widget five.** Recommended: border, paragraph, listing, input, viewport; table, tabs and gauge wait for a consumer.
9. **The name.** `/std/Tui` is proposed; `/std/Term` reads as the byte stream `/std/Handle` already is.

## Findings that are not this specification's

- An implicit whose solution is the projection of a transparent local binding to a fold is not solved (above); an elaborator finding, worked around here.
- `Async/read` on stdin blocks the scheduler on its first read; a comment in `Async.crs` could say so.
- The two host findings this survey turned up — stdin's buffered handle hiding input from `poll`, and the native host restoring nothing at a trap — are recorded with the rows in the indispensable tier.

## Deliberately not specified

Mouse and focus events. Images and graphics protocols. Grapheme clustering (Mode 2027) and the text-sizing protocol. A retained widget tree, CSS-like styling, and a constraint solver. Windows consoles and the browser harness. Restoring the alternate screen after a trap, which follows [A failing program names what failed](runtime-failure-legibility-spec.md). A `Draw`-style derivation for the widgets. Performance beyond the diff: a frame is rebuilt from values every time, and that is correct first.
