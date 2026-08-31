#!/usr/bin/env node
/*
 * run.js -- friendly CLI over the node:test suite.
 *
 *   node run.js --env=cpo|embed|embed-static|vscode|vscode-ovsx [--compiler=pyret|ts] [--grep=<regex>] [--suites=all|a,b] [--reporter=spec|tap|dot]
 *
 * Examples:
 *   node run.js --env=embed --grep tables        # one feature, in the embed instance
 *   node run.js --env=cpo   --grep 'is-not'      # regex over test names
 *   node run.js --env=vscode                     # everything, in the vscode webview
 *   node run.js --env=cpo --compiler=ts          # same suite, on the TS-compiler flavor
 *
 * --compiler selects which compiler backend the environment loads (the
 * editor's ?compiler= flavor; default pyret). It maps to PYRET_COMPILER,
 * which each env adapter honors; the assertions themselves are identical.
 *
 * This just shells out to `node --test` with the right flags + PYRET_ENV, so you
 * get node:test's native reporters and exit code. `--grep` maps to
 * --test-name-pattern (matches suite names and individual test names).
 *
 * It needs no build of its own; cpo/embed need the CPO server running, vscode
 * does not (its assets come from the built extension).
 */
const path = require("path");
const { spawn } = require("child_process");

const KNOWN_FLAGS = ["env", "grep", "suites", "reporter", "compiler"];
const USAGE = "usage: node run.js --env=cpo|embed|embed-static|vscode|vscode-ovsx [--compiler=pyret|ts] [--grep=<regex>] [--suites=all|a,b] [--reporter=spec|tap|dot]";

function die(msg) {
  console.error(msg);
  console.error(USAGE);
  process.exit(2);
}

function arg(name) {
  // supports "--name=value" and "--name value"
  const argv = process.argv.slice(2);
  for (let i = 0; i < argv.length; i++) {
    if (argv[i] === "--" + name) return argv[i + 1];
    if (argv[i].startsWith("--" + name + "=")) return argv[i].slice(name.length + 3);
  }
  return undefined;
}

// Reject anything unrecognized rather than ignoring it. A misspelled flag used
// to fall through to the defaults, so `--suite=url-imports` (singular) ran the
// FULL suite in every environment -- which reads as "my filter matched
// everything" rather than "my filter was never applied".
(function rejectUnknownArgs() {
  const argv = process.argv.slice(2);
  for (let i = 0; i < argv.length; i++) {
    const tok = argv[i];
    if (!tok.startsWith("--")) die("unexpected argument: " + tok);
    const name = tok.slice(2).split("=")[0];
    if (!KNOWN_FLAGS.includes(name)) die("unknown flag: " + tok);
    if (!tok.includes("=")) i++; // "--name value": step over the value
  }
})();

const env = arg("env");
if (!env || !["cpo", "embed", "embed-static", "vscode", "vscode-ovsx"].includes(env)) {
  die("--env must be one of cpo | embed | embed-static | vscode | vscode-ovsx (got " + JSON.stringify(env) + ")");
}
const grep = arg("grep");
// Only an ABSENT --suites means "run everything"; `--suites=` is an empty
// selection, which suite.test.js rejects rather than quietly widening to all.
const suitesArg = arg("suites");
const suites = suitesArg === undefined ? "all" : suitesArg;
const reporter = arg("reporter") || "spec";
const compiler = arg("compiler") || process.env.PYRET_COMPILER || "pyret";
if (!["pyret", "ts"].includes(compiler)) {
  console.error("--compiler must be pyret or ts");
  process.exit(2);
}

const nodeArgs = ["--test", "--test-reporter=" + reporter];
if (grep) nodeArgs.push("--test-name-pattern=" + grep);
nodeArgs.push(path.join(__dirname, "tests", "suite.test.js"));

// Serve the url-file fixtures ourselves, for every environment.
//
// They used to be reached through the CPO server's dev-only test-util mount,
// which meant the three environments that run no CPO server (embed-static,
// vscode, vscode-ovsx) either skipped those tests or failed them. Serving them
// here makes the "no external network" cases genuinely hermetic and identical
// across all five envs.
//
// It has to start HERE rather than in an env's setup(): suite.test.js builds its
// spec list at module load, before before() ever runs, so the origin must be
// known before the child is spawned. Hence a parent-side server and an env var.
const { startStaticServer } = require("./shared/static-server");
const FIXTURE_ROOT = path.resolve(__dirname, "..", "code.pyret.org", "test-util");

(async () => {
  const fixtures = await startStaticServer({ roots: [FIXTURE_ROOT] });

  const child = spawn(process.execPath, nodeArgs, {
    stdio: "inherit",
    env: {
      ...process.env,
      PYRET_ENV: env,
      PYRET_SUITES: suites,
      PYRET_COMPILER: compiler,
      PYRET_FIXTURE_BASE: fixtures.origin,
    },
  });
  child.on("exit", async (code, signal) => {
    await fixtures.close();
    if (signal) process.kill(process.pid, signal);
    else process.exit(code == null ? 1 : code);
  });
})();
