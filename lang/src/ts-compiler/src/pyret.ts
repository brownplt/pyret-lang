// Port of src/arr/compiler/pyret.arr — the command-line entry point.
// Invoked as `node build/ts-compiler/pyret.js <options>`.

import * as P from 'path';
import * as fs from 'fs';
import * as C from './cmdline';
import * as CLI from './cli-module-loader';
import * as CS from './compile-structs';
import * as B from './locators/builtin';
import * as S from './server';
import * as PP from './parse-pyret';
import * as RED from './render-error-display';
import { raise } from './shared';

// this value is the limit of number of steps that could be inlined in case body
export const defaultInlineCaseLimit = 5;

const successCode = 0;
const failureCode = 1;

// Set when --serve starts a server on the event loop; suppresses the final
// process.exit so the process stays alive (the Pyret original instead
// blocks inside S.serve until shutdown).
let startedServer = false;

function print(s: string): void { process.stdout.write(s); }
function printError(s: string): void { process.stderr.write(s); }

export async function main(args: string[]): Promise<number> {

  const thisPyretDir = P.dirname(P.resolve(C.fileName));

  const options = new Map<string, C.Param>([
    ['serve',
      C.flag(C.once, 'Start the Pyret server')],
    ['port',
      C.nextValDefault(C.Str, '1701', undefined, C.once, 'Port to serve on (default 1701, can also be UNIX file socket or windows pipe)')],
    ['build-standalone',
      C.nextVal(C.Str, C.once, 'Main Pyret (.arr) file to build as a standalone (deprecated)')],
    ['build-runnable',
      C.nextVal(C.Str, C.once, 'Main Pyret (.arr) file to build as a standalone')],
    ['require-config',
      C.nextVal(C.Str, C.once, 'JSON file to use for requirejs configuration of build-runnable')],
    ['outfile',
      C.nextVal(C.Str, C.once, 'Output file for build-runnable')],
    ['build',
      C.nextVal(C.Str, C.once, 'Pyret (.arr) file to build')],
    ['run',
      C.nextVal(C.Str, C.once, 'Pyret (.arr) file to compile and run')],
    ['standalone-file',
      C.nextValDefault(C.Str, 'src/js/base/handalone.js', undefined, C.once, 'Path to override standalone JavaScript file for main')],
    ['builtin-js-dir',
      C.nextVal(C.Str, C.many, 'Directory to find the source of builtin js modules')],
    ['builtin-arr-dir',
      C.nextVal(C.Str, C.many, 'Directory to find the source of builtin arr modules')],
    ['allow-builtin-overrides',
      C.flag(C.once, 'Allow overlapping builtins defined between builtin-js-dir and builtin-arr-dir')],
    ['no-display-progress',
      C.flag(C.once, 'Skip printing the "Compiling X/Y" progress indicator')],
    ['compiled-read-only-dir',
      C.nextVal(C.Str, C.many, 'Additional directories to search to find precompiled versions of modules')],
    ['compiled-dir',
      C.nextValDefault(C.Str, 'compiled', undefined, C.once, 'Directory to save compiled files to; searched first for precompiled modules')],
    ['library',
      C.flag(C.once, "Don't auto-import basics like list, option, etc.")],
    ['module-load-dir',
      C.nextValDefault(C.Str, '.', undefined, C.once, 'Base directory to search for modules')],
    ['checks',
      C.nextVal(C.Str, C.once, 'Specify which checks to execute (all, none, or main, only:<string>)')],
    ['checks-format',
      C.nextValDefault(C.Str, 'text', undefined, C.once, 'Specify check block output format (text, json) (default text)')],
    ['profile',
      C.flag(C.once, 'Add profiling information to the main file')],
    ['check-all',
      C.flag(C.once, 'Run checks all modules (not just the main module)')],
    ['no-check-mode',
      C.flag(C.once, 'Skip checks')],
    ['no-spies',
      C.flag(C.once, 'Disable printing of all `spy` statements')],
    ['allow-shadow',
      C.flag(C.once, 'Run without checking for shadowed variables')],
    ['improper-tail-calls',
      C.flag(C.once, 'Run without proper tail calls')],
    ['collect-times',
      C.flag(C.once, 'Collect timing information about compilation')],
    ['type-check',
      C.flag(C.once, 'Type-check the program during compilation')],
    ['inline-case-body-limit',
      C.nextValDefault(C.Num, defaultInlineCaseLimit, undefined, C.once, 'Set number of steps that could be inlined in case body')],
    ['deps-file',
      C.nextVal(C.Str, C.once, 'Provide a path to override the default dependencies file')],
    ['html-file',
      C.nextVal(C.Str, C.once, 'Name of the html file to generate that includes the standalone (only makes sense if deps-file is the result of browserify)')],
    ['no-module-eval',
      C.flag(C.once, "Produce modules as literal functions, not as strings to be eval'd (may break error source locations)")],
    ['no-user-annotations',
      C.flag(C.once, 'Ignore all annotations in .arr files, treating them as if they were blank.')],
    ['no-runtime-annotations',
      C.flag(C.once, 'Ignore all annotations in the runtime, treating them as if they were blank.')],
    ['url-file-mode',
      C.nextValDefault(C.Str, 'all-remote', undefined, C.once, 'How to handle url-file imports (all-remote, all-local, or local-if-present)')],
    ['pause-schedule',
      C.nextVal(C.Str, C.once, 'JS file computing initial GAS/RUNGAS for the runtime; its code is baked into the standalone')],
  ]);

  const paramsParsed = C.parseArgs(options, args);

  if (C.isSuccess(paramsParsed)) {
    const r = paramsParsed.parsed;
    const rest = paramsParsed.unknown;
    const checks =
      (r.has('no-check-mode') || r.has('library')) ? 'none'
      : r.has('checks') ? r.get('checks')
      : 'all';
    const checksFormat = r.get('checks-format');
    // enable-spies is computed (but unused) by the Pyret original too
    const enableSpies = !r.has('no-spies');
    void enableSpies;
    const allowShadowed = r.has('allow-shadow');
    const moduleDir = r.get('module-load-dir');
    void moduleDir;
    const inlineCaseBodyLimit = r.get('inline-case-body-limit');
    const typeCheck = r.has('type-check');
    const tailCalls = !r.has('improper-tail-calls');
    const compiledDir = r.get('compiled-dir');
    const standaloneFile = r.get('standalone-file');
    const addProfiling = r.has('profile');
    void addProfiling;
    const displayProgress = !r.has('no-display-progress');
    const htmlFile: string | undefined =
      r.has('html-file') ? r.get('html-file') : undefined;
    const moduleEval = !r.has('no-module-eval');
    const userAnnotations = !r.has('no-user-annotations');
    const runtimeAnnotations = !r.has('no-runtime-annotations');
    const pauseSchedule: string | undefined =
      r.has('pause-schedule') ? fs.readFileSync(r.get('pause-schedule'), { encoding: 'utf8' }) : undefined;
    if (r.has('builtin-js-dir')) {
      B.setBuiltinJsDirs(r.get('builtin-js-dir'));
    }
    if (r.has('builtin-arr-dir')) {
      B.setBuiltinArrDirs(r.get('builtin-arr-dir'));
    }
    if (r.has('allow-builtin-overrides')) {
      B.setAllowBuiltinOverrides(r.get('allow-builtin-overrides'));
    }
    const urlFileModeStr = r.get('url-file-mode');
    const urlFileMode =
      urlFileModeStr === 'all-remote' ? CS.allRemote
      : urlFileModeStr === 'all-local' ? CS.allLocal
      : urlFileModeStr === 'local-if-present' ? CS.localIfPresent
      : raise('Unknown url-file-mode: ' + urlFileModeStr);
    if (r.has('checks') && r.has('no-check-mode') && !(r.get('checks') === 'none')) {
      printError("Can't use --checks " + r.get('checks') + ' with -no-check-mode\n');
      return failureCode;
    } else if (r.has('checks') && r.has('check-all') && !(r.get('checks') === 'all')) {
      printError("Can't use --checks " + r.get('checks') + ' with -check-all\n');
      return failureCode;
    } else if (r.has('run')) {
      const runArgs = rest.length === 0 ? [] : rest.slice(1);
      const result = await CLI.run(r.get('run'), {
        ...CS.defaultCompileOptions,
        standaloneFile: standaloneFile,
        displayProgress: displayProgress,
        checks: checks
      }, runArgs);
      print(result.message + '\n');
      return result.exitCode;
    } else if (rest.length !== 0) {
      printError('Invalid extra arguments ' + C.stringListToRepr(rest) + '. Option may no longer be supported.\n');
      return failureCode;
    } else if (r.has('build-runnable')) {
      const outfile = r.has('outfile')
        ? r.get('outfile')
        : r.get('build-runnable') + '.jarr';
      const compileOpts = CS.makeDefaultCompileOptions(thisPyretDir);
      await CLI.buildRunnableStandalone(
        r.get('build-runnable'),
        r.has('require-config')
          ? r.get('require-config')
          : P.resolve(P.join(thisPyretDir, 'config.json')),
        outfile,
        {
          ...compileOpts,
          thisPyretDir: thisPyretDir,
          standaloneFile: standaloneFile,
          checks: checks,
          checksFormat: checksFormat,
          typeCheck: typeCheck,
          allowShadowed: allowShadowed,
          collectAll: false,
          collectTimes: r.has('collect-times') && r.get('collect-times'),
          ignoreUnbound: false,
          properTailCalls: tailCalls,
          compiledCache: compiledDir,
          compiledReadOnly: r.has('compiled-read-only-dir') ? r.get('compiled-read-only-dir') : [],
          displayProgress: displayProgress,
          inlineCaseBodyLimit: inlineCaseBodyLimit,
          depsFile: r.has('deps-file') ? r.get('deps-file') : compileOpts.depsFile,
          htmlFile: htmlFile,
          moduleEval: moduleEval,
          userAnnotations: userAnnotations,
          runtimeAnnotations: runtimeAnnotations,
          urlFileMode: urlFileMode,
          pauseSchedule: pauseSchedule
        });
      return successCode;
    } else if (r.has('serve')) {
      const port = r.get('port');
      startedServer = true;
      S.serve(port, thisPyretDir);
      // The Pyret original blocks on the runtime stack until the server
      // shuts down (SIGINT/shutdown exit the process from server.ts) and
      // then returns success-code; here the server lives on the event loop,
      // so the final process.exit below must be skipped (see `serving`).
      return successCode;
    } else if (r.has('build-standalone')) {
      printError('Use build-runnable instead of build-standalone\n');
      return failureCode;
    } else if (r.has('build')) {
      const result = await CLI.compile(r.get('build'), {
        ...CS.defaultCompileOptions,
        checks: checks,
        typeCheck: typeCheck,
        allowShadowed: allowShadowed,
        collectAll: false,
        ignoreUnbound: false,
        properTailCalls: tailCalls,
        compileModule: false,
        displayProgress: displayProgress
      });
      // NOTE: loadables are module-as-string values, so CS.is-err never
      // matches and `failures` is always empty — the Pyret CLI exhibits
      // exactly this behavior (silent, exit 0, even for programs with
      // compile errors). Ported as-is for parity.
      const failures = result.loadables.filter((l: any) => CS.isErr(l));
      if (failures.length !== 0) {
        for (const f of failures) {
          for (const e of (f as any).errors) {
            printError(String(e));
            printError('\n');
          }
          printError('There were compilation errors\n');
        }
        return failureCode;
      } else {
        return successCode;
      }
    } else {
      printError(C.usageInfo(options).join('\n'));
      printError('Unknown command line options\n');
      return failureCode;
    }
  } else {
    printError(paramsParsed.message + '\n');
    printError(C.usageInfo(options).join('\n'));
    return failureCode;
  }
}

// The Pyret-hosted compiler runs on the runtime's segmented stack, so deep
// recursion over large modules is free there; this port recurses on the JS
// stack and large trove modules (lists.arr etc.) exceed node's default
// stack. Re-exec once with a bigger stack unless the caller already set one.
const hasStackSize = process.execArgv.some((a) => a.startsWith('--stack-size'));
if (!hasStackSize && !process.env.PYRET_TS_NO_RESPAWN) {
  const { spawnSync } = require('child_process');
  if (process.argv.includes('-serve')) {
    // Ctrl+C reaches both processes; the server child shuts down cleanly
    // (socket cleanup, exit 0 — see server.ts) and this wrapper must
    // survive the signal to report that status, like the Pyret original.
    process.on('SIGINT', () => { /* child handles it */ });
  }
  const res = spawnSync(process.execPath,
    ['--stack-size=8192', ...process.execArgv, process.argv[1], ...process.argv.slice(2)],
    { stdio: 'inherit', env: { ...process.env, PYRET_TS_NO_RESPAWN: '1' } });
  process.exit(res.status === null ? failureCode : res.status);
}

main(C.otherArgs).then((exitCode) => {
  if (!startedServer || exitCode !== successCode) {
    process.exit(exitCode);
  }
}, (e: any) => {
  // When main raises (e.g. "There were compilation errors" out of
  // build-runnable-standalone), the Pyret runtime prints
  // "The run ended in error:" followed by the message and a Pyret stack
  // before exiting 1. Mirror the message portion.
  //
  // Parse errors are Pyret exceptions in the original, rendered through
  // error.arr's render-reason; render ours the same way (the display
  // begins with a paragraph, whose leading "\n" supplies the blank line).
  if (e instanceof PP.PyretParseError) {
    printError('The run ended in error:\n' +
      RED.displayToString(e.renderReason(), String, []) + '\n');
  } else {
    printError('The run ended in error:\n\n' +
      (e && e.message !== undefined ? e.message : String(e)) + '\n');
  }
  // The JS stack, on request -- the message alone cannot say WHERE a
  // "Maximum call stack size exceeded" came from.
  if (process.env.PYRET_TS_STACK && e && e.stack) { printError(e.stack + '\n'); }
  process.exit(failureCode);
});
