# `/std` answers the questions a program cannot avoid, where each has one correct answer

**Decision.** `/std` answers the questions a program cannot avoid, where each has one correct answer: one whose best approach is already known, so that what is left to change is its internals, not its shape. The questions come from three places. The language asks some of every program: what equality, order and sequencing are, and what a test is (`Eq`, `Bool`, `Ord`, `Monad`, `Try`, `Test`). Any two libraries must agree on others to hand each other values: what a number, a text, a list or a map is. And each thing the host hands a program asks the rest: a socket, how to talk to the web (HTTP/1.1, with JSON for data and HTML for pages); the command line, how its arguments read (GNU's conventions); the terminal, how it is driven (ECMA-48); a file, how configuration is written (TOML); the clock, how a moment is spelled (RFC 3339).

A question already answered is not answered twice: a format that is fixed but answers no open question stays out, as XML, whose questions JSON, HTML and TOML answer. A question whose answer is still argued, or differs from one platform to the next, stays out too. The terminal is a solved problem every platform agrees on, so `/std/Tui` belongs; a graphical window is not, since each platform's gatekeeper requires its own WebView renderer (WebKit on Apple's systems, WebView2 on Windows, WebKitGTK on Linux), so it is a package.

An entry is the general answer, the one a program reaches for first, and a specialization of it, shaped by a cost or a workload a program chose, is a package: a ring buffer beside `List`, a dataframe beside `List` and `Map`. The test is whether a program that knows nothing yet about its workload would reach for it; one it reaches for only after choosing a cost or a shape is a package. Size does not decide it, big or small.

**Rationale.**

- **A question with one correct answer has no second answer a program would want**, so shipping it with the compiler spares every program a choice and a dependency, and leaves nothing a package's own versioning would buy.
- **A value two libraries exchange must be one type**, or a text from one package is no text to another: Rust keeps `Option` and `String` in `std` for that reason while leaving HTTP to crates.
- **What a program reaches for first owes more than correctness**: it answers its standard, where answering otherwise is a defect and never a design choice, and it serves the common case well enough that a program leaves it only for a narrower need, never because it fell short.
- **A specialization answers a narrower question than the one a program cannot avoid**: Python keeps `list`, `dict` and `deque` and leaves dataframes to pandas, Rust keeps `Vec` and `HashMap` and leaves `smallvec` and `indexmap` to crates, while Go's `container/ring` and `container/list` are the standard-library pieces its programs pass over.
- **What Python's [PEP 594](https://peps.python.org/pep-0594/) removed were modules whose answer had moved on**, formats and protocols the world left, not modules that were too big.

**Rejected.**

- **A thin library**, as Rust's `std` and Haskell's `base` keep, with the rest in packages: every program then chooses again among packages for questions with one answer.
- **A standard library versioned apart from the compiler**, as Deno's `@std`: a separate version exists so a shape can change, and these shapes do not.
- **Size or frequency of use as the test.**
