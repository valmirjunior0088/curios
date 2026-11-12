// The runner of a test serve, which `curios test --target browser` answers at its root. It drives the run over a socket it opens at `/`, which the command takes from one runner at a time: each step it is sent it runs in an iframe of its own — the current subject's page at `index.html`, the test's index as the program's argument — admitting the page's posts by the window they come from and the origin they come at, sends each write the test makes on as it is made and then the test's outcome, and shows what the command wrote — its whole report so far in the first reply, what it wrote since in each after — keeping the end of it in view while its reader stands there. The document hands it the key the page spells its arguments under in a JSON block; the pure pieces are exported for the Node suite beside this file, and `main` runs where a document stands.

/** The outcome reported for a page whose document loaded with no `started` posted: its loader never ran, which a gateway that fails to import causes, and a run with no timeout would otherwise wait on it forever. */
export const NEVER_STARTED = {
  exitCode: null,
  trap: "the loader of index.html did not start; the console names what stopped it",
  held: null,
};

/** Whether `event` is a post from `iframe`'s window at `origin`, the runner's own: a message on the window can come from any site, and only the page the runner opened is listened to. */
export const admitted = (event, iframe, origin) =>
  event.source === iframe.contentWindow && event.origin === origin;

/** `bytes` in base64. */
export const encoded = (bytes) => {
  let binary = "";

  for (const byte of bytes) {
    binary += String.fromCharCode(byte);
  }

  return btoa(binary);
};

/** The bytes `text` spells in base64. */
export const decoded = (text) =>
  Uint8Array.from(atob(text), (character) => character.charCodeAt(0));

/** `bytes`, which the test in hand wrote on `stream`, as the socket carries them to the command: in base64, as they were written. */
export const wrote = (stream, bytes) =>
  JSON.stringify({ wrote: { stream, bytes: encoded(bytes) } });

/** `outcome` as the socket carries it to the command: how the test ended, as its page posted it. */
export const posted = (outcome) =>
  JSON.stringify({
    outcome: {
      exitCode: outcome.exitCode,
      trap: outcome.trap,
      held: outcome.held,
    },
  });

/** Whether a reader `scrolled` so far down a document of `height`, in a window of `inner`, stands at its end, a line's slack allowed: what says the page goes on following what is appended, and stays where a reader scrolled back to. */
export const following = (scrolled, inner, height) =>
  scrolled + inner >= height - 24;

/** What the runner says where its socket closes before the run is complete: never opened, it was refused, and once opened, the command went away. */
export const UNREACHED = "the run is driven by another page, or has ended\n";
export const DROPPED = "the command closed the run before it was complete\n";

/** Open `step`'s test in an iframe under `container`, the page's key `argument` carrying the test's index, hand `written` each write its page posts, and resolve to the outcome its page posts — or to `NEVER_STARTED` once the document has loaded with no `started` taken, the check deferred one task so a post made while the loader evaluated is taken first. The iframe is removed once its page has answered. */
const ran = (step, argument, container, written) =>
  new Promise((resolve) => {
    const iframe = document.createElement("iframe");
    const origin = globalThis.location.origin;
    let started = false;

    const settle = (outcome) => {
      globalThis.removeEventListener("message", taken);
      iframe.remove();
      resolve(outcome);
    };
    const taken = (event) => {
      if (!admitted(event, iframe, origin)) {
        return;
      }

      if (event.data?.type === "started") {
        started = true;
      } else if (event.data?.type === "wrote") {
        written(event.data.stream, event.data.bytes);
      } else if (event.data?.type === "outcome") {
        settle(event.data.outcome);
      }
    };

    globalThis.addEventListener("message", taken);
    iframe.addEventListener("load", () => {
      setTimeout(() => {
        if (!started) {
          settle(NEVER_STARTED);
        }
      }, 0);
    });
    iframe.src = `/index.html?${argument}=${step.test}`;
    container.append(iframe);
  });

/** Drive the run over a socket at `/`: show what the command writes in each reply, run the step it names, send what its test writes and then its outcome, until a reply names none. A socket closed before then was refused, or dropped by the command, and is said so. */
export const main = () => {
  const run = JSON.parse(document.getElementById("run").textContent);
  const lines = document.getElementById("lines");
  const frame = document.getElementById("frame");
  const decoder = new TextDecoder();
  const socket = new WebSocket(`ws://${globalThis.location.host}/`);
  let opened = false;
  let complete = false;

  socket.addEventListener("open", () => {
    opened = true;
  });
  socket.addEventListener("message", async (event) => {
    const reply = JSON.parse(event.data);
    const root = document.documentElement;
    const follows = following(
      globalThis.scrollY,
      globalThis.innerHeight,
      root.scrollHeight,
    );

    lines.append(decoder.decode(decoded(reply.written)));
    if (follows) {
      globalThis.scrollTo(0, root.scrollHeight);
    }

    if (reply.next === null) {
      complete = true;

      return;
    }

    socket.send(
      posted(
        await ran(reply.next, run.argument, frame, (stream, bytes) =>
          socket.send(wrote(stream, bytes)),
        ),
      ),
    );
  });
  socket.addEventListener("close", () => {
    if (!complete) {
      lines.append(opened ? DROPPED : UNREACHED);
    }
  });
};

if (globalThis.document !== undefined) {
  main();
}
