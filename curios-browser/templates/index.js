{#- A program's loader: an import of every gateway, the harness whole, what this page was built with, and the `start` that runs the program — called here where the loader stands in a document, and by whoever imports it otherwise, with the program's bytes. Rendered with no escaping, since every value spliced in is JSON. -#}
// The loader of a Curios program's page, which `curios compile --target browser` wrote.
{%- for gateway in gateways %}
import * as gateway{{ loop.index0 }} from {{ gateway.specifier }};
{%- endfor %}

{{ harness }}

/** What this page was built with: the name the program is run by, the harness's configuration, the program's rows, the bridge in base64, the program's integrity, where it stands and the element its output is written into. */
const page = {
  name: {{ name }},
  configuration: {{ configuration }},
  rows: {{ rows }},
  bridge: {{ bridge }},
  integrity: {{ integrity }},
  program: {{ program }},
  output: {{ output }},
};

/** Each gateway the page links: the row, the package that declares it, where it stands in the page, its module, and each declaration of the program it answers beside the export that answers it. */
const gateways = [
{%- for gateway in gateways %}
  { ...{{ gateway.described }}, module: gateway{{ loop.index0 }} },
{%- endfor %}
];

const encoder = new TextEncoder();

/** The bytes `text` spells in base64. */
const decoded = (text) =>
  Uint8Array.from(atob(text), (character) => character.charCodeAt(0));

/** The program's arguments `query` spells, the part of a URL from its `?`: each `arg=` one argument, a `%` before two hex digits one byte, and any other character its UTF-8 — `+` among them, which is no space here. */
const argumentsOf = (query) =>
  query
    .replace(/^\?/, "")
    .split("&")
    .filter((pair) => pair.startsWith("arg="))
    .map((pair) => {
      const text = pair.slice("arg=".length);
      const bytes = [];

      for (let i = 0; i < text.length; ) {
        if (text[i] === "%" && /^[0-9A-Fa-f]{2}$/.test(text.slice(i + 1, i + 3))) {
          bytes.push(Number.parseInt(text.slice(i + 1, i + 3), 16));
          i += 3;
        } else {
          const character = String.fromCodePoint(text.codePointAt(i));
          bytes.push(...encoder.encode(character));
          i += character.length;
        }
      }

      return Uint8Array.from(bytes);
    });

/** A stream of the program's output: each write decoded and written into `element` while it stands in the document, and each line handed to `log`, the console's. */
const stream = (element, log) => {
  const decoder = new TextDecoder();
  let line = "";

  const shown = (text) => {
    if (element?.isConnected) {
      element.append(text);
    }

    const lines = (line + text).split("\n");
    line = lines.pop();

    for (const complete of lines) {
      log(complete);
    }
  };

  return {
    write: (bytes) => shown(decoder.decode(bytes, { stream: true })),
    end: () => {
      shown(decoder.decode());

      if (line !== "") {
        log(line);
      }
    },
  };
};

/** The program, fetched beside this loader and held to its integrity: a reply that is no success is refused by its status rather than compiled. */
const fetched = async () => {
  const response = await fetch(new URL(page.program, import.meta.url), {
    integrity: page.integrity,
  });

  if (!response.ok) {
    throw new Error(`${page.program} answered ${response.status}`);
  }

  return new Uint8Array(await response.arrayBuffer());
};

/** What answers each declaration the page's gateways answer, or a refusal naming the first export that is no function. */
const answers = () => {
  const foreign = {};

  for (const { row, package: owner, path, module, answers } of gateways) {
    for (const [declaration, name] of answers) {
      if (typeof module[name] !== "function") {
        return {
          refusal: `the gateway ${path} of the foreign row ${JSON.stringify(row)} of ${owner} exports no function ${name}, which answers ${declaration}`,
        };
      }

      foreign[declaration] = module[name];
    }
  }

  return { foreign };
};

/**
 * Run the page's program: `program`, its bytes, or what `index.wasm` holds, fetched beside this loader against its integrity; its arguments from `query`, the location's own where there is one. Its output is written into the page's element while that stands in the document and to the console, and a trap is reported in both.
 *
 * Resolves to what the harness's `run` resolves to.
 */
export async function start({ program, query = globalThis.location?.search ?? "" } = {}) {
  const element = globalThis.document?.getElementById(page.output) ?? null;
  const stdout = stream(element, (line) => console.log(line));
  const stderr = stream(element, (line) => console.error(line));

  const outcome = await (async () => {
    const { refusal, foreign } = answers();

    if (refusal !== undefined) {
      return { stdout: new Uint8Array(0), stderr: new Uint8Array(0), exitCode: null, trap: refusal };
    }

    try {
      const bytes = program ?? (await fetched());

      return await run({
        ...page.configuration,
        compiled: { program: bytes, foreigns: page.rows },
        bridge: decoded(page.bridge),
        args: [encoder.encode(page.name), ...argumentsOf(query)],
        hooks: { onStdout: stdout.write, onStderr: stderr.write, foreign },
      });
    } catch (error) {
      return { stdout: new Uint8Array(0), stderr: new Uint8Array(0), exitCode: null, trap: String(error) };
    }
  })();

  stdout.end();
  stderr.end();

  if (outcome.trap !== null) {
    if (element?.isConnected) {
      element.append(`${outcome.trap}\n`);
    }

    console.error(outcome.trap);
  }

  return outcome;
}

if (globalThis.document !== undefined) {
  start();
}
