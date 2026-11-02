// The runner of a test serve, which `curios test --target browser` answers at its root. It opens one iframe per test, one at a time — the current subject's page at `index.html`, the test's index as the program's argument — admits the page's posts by the window they come from and the origin they come at, reports each test's outcome to the command with `POST /` under the run's token, and shows the lines the command wrote back. The document hands it the token, the first step and the key the page spells its arguments under in a JSON block; the pure pieces are exported for the Node suite beside this file, and `main` runs where a document stands.

/** The outcome reported for a page whose document loaded with no `started` posted: its loader never ran, which a gateway that fails to import causes, and a run with no timeout would otherwise wait on it forever. */
export const NEVER_STARTED = {
  stdout: new Uint8Array(0),
  stderr: new Uint8Array(0),
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

/** The report of `step`'s `outcome` under `token`, as `POST /` takes it: the streams in base64, the rest as the page posted it. */
export const report = (token, step, outcome) =>
  JSON.stringify({
    run: token,
    subject: step.subject,
    test: step.test,
    outcome: {
      stdout: encoded(outcome.stdout),
      stderr: encoded(outcome.stderr),
      exitCode: outcome.exitCode,
      trap: outcome.trap,
      held: outcome.held,
    },
  });

/** Open `step`'s test in an iframe under `container`, the page's key `argument` carrying the test's index, and resolve to the outcome its page posts — or to `NEVER_STARTED` once the document has loaded with no `started` taken, the check deferred one task so a post made while the loader evaluated is taken first. The iframe is removed once its page has answered. */
const ran = (step, argument, container) =>
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

/** Report `step`'s `outcome` to the command under `token`, and resolve to its reply: what it wrote, whether it took the report, and the next step. */
const reported = async (token, step, outcome) => {
  const response = await fetch("/", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: report(token, step, outcome),
  });

  if (!response.ok) {
    throw new Error(`the report was answered ${response.status}`);
  }

  return response.json();
};

/** Run every step from the document's first, showing what the command writes for each, until the command says the run is complete or that the step was not the one in hand — another runner is reporting the run. */
export const main = async () => {
  const run = JSON.parse(document.getElementById("run").textContent);
  const lines = document.getElementById("lines");
  const frame = document.getElementById("frame");
  const decoder = new TextDecoder();
  let step = run.next;

  try {
    while (step !== null) {
      const outcome = await ran(step, run.argument, frame);
      const reply = await reported(run.token, step, outcome);

      lines.append(decoder.decode(decoded(reply.stdout)));
      lines.append(decoder.decode(decoded(reply.stderr)));

      if (!reply.taken) {
        lines.append("the run is being reported by another page\n");

        return;
      }

      step = reply.next;
    }
  } catch (error) {
    lines.append(`${String(error)}\n`);
  }
};

if (globalThis.document !== undefined) {
  main();
}
