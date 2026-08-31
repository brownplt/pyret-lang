/*
  Port of src/arr/compiler/server.arr (the compile handler) and
  src/arr/compiler/server.js (the ws/http server machinery, originally a
  Pyret-runtime FFI module).

  Protocol (JSON text frames over a websocket):
    client -> server: { "command": "compile", "compileOptions": "<JSON string of options dict>" }
                      { "command": "stop" }
                      { "command": "shutdown" }
    server -> client: {"type": "echo-log", "contents": <string>, "clear-first": false | <number>}
                      {"type": "echo-err", "contents": <string>}
                      {"type": "compile-success"}
                      {"type": "compile-failure"}
  The server closes the connection after each compile job. Messages from the
  Pyret-side handler are serialized like Pyret's json serialize (": " and
  ", " separators, insertion order); the internal-error path uses
  JSON.stringify, exactly as the original server.js did.
*/

import * as http from 'http';
import * as fs from 'fs';
import * as P from 'path';
import * as CLI from './cli-module-loader';
import * as CS from './compile-structs';
import * as B from './locators/builtin';
import * as ED from './error-display';
import * as RED from './render-error-display';
import { InternalCompilerError, TODOError } from './shared';

// Resolves against lang/node_modules at runtime (compiled output lives
// under build/ts-compiler).
const ws = require('ws');

const INFO = 4;
const LOG = 3;
const WARN = 2;
const ERROR = 1;
const LOG_LEVEL = ERROR;

function makeLogger(level: number): (...args: any[]) => void {
  return function (...args: any[]) {
    if (LOG_LEVEL >= level) {
      console.log.apply(console, (['[server] ', new Date()] as any[]).concat(args));
    }
  };
}

const info = makeLogger(INFO);
const log = makeLogger(LOG);
const warn = makeLogger(WARN);
const error = makeLogger(ERROR);
void log;
void warn;

// Pyret's J.j-obj(...).serialize(): pairs in insertion order, ": " after
// keys, ", " between pairs. Values here are only strings/numbers/booleans,
// whose Pyret serializations coincide with JSON.stringify.
function serializeMessage(pairs: Array<[string, string | number | boolean]>): string {
  return '{' + pairs.map(([k, v]) => JSON.stringify(k) + ': ' + JSON.stringify(v)).join(', ') + '}';
}

// Pyret string-dict get-value: raises on a missing key.
function getValue(options: { [key: string]: any }, key: string): any {
  if (!(key in options)) {
    throw new InternalCompilerError('Key ' + key + ' not found');
  }
  return options[key];
}

function orElse(options: { [key: string]: any }, key: string, dflt: any): any {
  return key in options ? options[key] : dflt;
}

// Port of server.arr's compile(options): options is the JSON-decoded dict
// from the client, extended by the serve handler below. Kebab-case dict
// keys map onto the camelCased CompileOptions fields.
async function compile(options: { [key: string]: any }): Promise<void> {
  const outfile = 'outfile' in options
    ? options['outfile']
    : getValue(options, 'program') + '.jarr';
  const compileOpts = CS.makeDefaultCompileOptions(getValue(options, 'this-pyret-dir'));
  await CLI.buildRunnableStandalone(
    getValue(options, 'program'),
    getValue(options, 'require-config'),
    outfile,
    {
      ...compileOpts,
      baseDir: getValue(options, 'base-dir'),
      thisPyretDir: getValue(options, 'this-pyret-dir'),
      checkMode: !orElse(options, 'no-check-mode', false),
      typeCheck: orElse(options, 'type-check', false),
      allowShadowed: orElse(options, 'allow-shadowed', false),
      collectAll: orElse(options, 'collect-all', false),
      ignoreUnbound: orElse(options, 'ignore-unbound', false),
      properTailCalls: orElse(options, 'improper-tail-calls', true),
      compiledCache: orElse(options, 'compiled-dir', './compiled'),
      compiledReadOnly: orElse(options, 'compiled-read-only', []),
      standaloneFile: orElse(options, 'standalone-file', compileOpts.standaloneFile),
      checks: getValue(options, 'checks'),
      checksFormat: getValue(options, 'checks-format'),
      displayProgress: orElse(options, 'display-progress', true),
      log: orElse(options, 'log', compileOpts.log),
      logError: orElse(options, 'log-error', compileOpts.logError),
      depsFile: orElse(options, 'deps-file', compileOpts.depsFile),
      userAnnotations: orElse(options, 'user-annotations', compileOpts.userAnnotations)
    });
}

// Mirror of exn-unwrap(exn).render-reason() under the runtime's raise:
// values carrying render-reason raise as-is; everything else is wrapped as
// error.arr's user-exception, whose reason is [ED.error: [ED.para:
// ED.embed(value)]]. Plain `raise(<string>)` in this port throws
// InternalCompilerError/TODOError carrying the string; arbitrary JS errors
// surface like the original runtime's message-exception (message + stack).
function renderReasonOf(exn: any): ED.ErrorDisplay {
  if (exn !== null && exn !== undefined && typeof exn.renderReason === 'function') {
    return exn.renderReason();
  }
  let value: any;
  if (exn instanceof InternalCompilerError || exn instanceof TODOError) {
    value = exn.message;
  } else if (exn instanceof Error) {
    value = String(exn) + '\n' + exn.stack;
  } else {
    value = exn;
  }
  return ED.error(ED.para(ED.embed(value)));
}

// Pyret tostring, restricted to the values that reach an ED.embed here
// (strings embed as themselves).
function tostring(v: any): string {
  return typeof v === 'string' ? v : String(v);
}

// Port of server.js's make-server. Port is a string: a number-like string
// is a TCP port, anything else is a unix socket path / windows pipe (node's
// listen() makes the same distinction the original relied on).
function makeServer(port: string, onmessage: (msg: string, sendMessage: (jsonData: string) => void) => void | Promise<void>): void {

  const runQueue: string[] = [];

  const server = http.createServer(function (_request, response) {
    response.writeHead(404);
    response.end();
  });
  server.listen(port as any, function () {
    info((new Date()) + ' Server is listening on port ' + port);
    info((new Date()) + " The server's working directory is " + process.cwd());
  });

  // At this point, using port as a file socket didn't fail, so make sure
  // to remove it when we shut down.
  process.on('SIGINT', function () {
    if (fs.existsSync(port)) {
      fs.unlinkSync(port);
    }
  });
  process.on('exit', function () {
    if (fs.existsSync(port)) {
      fs.unlinkSync(port);
    }
  });

  const wsServer = new ws.Server({
    server: server
  });

  wsServer.on('connection', function (connection: any) {

    function respond(jsonData: string): void {
      info('Sending: ', jsonData);
      connection.send(jsonData);
    }
    function respondJSON(json: any): void { respond(JSON.stringify(json)); }

    function tryQueue(): void {
      info('Trying run queue, length is ', runQueue.length);
      if (runQueue.length > 0) {
        const current = runQueue.pop()!;
        // The compile handler is async (the dependency chase awaits
        // locator steps), so the connection must stay open until it
        // settles -- its success/failure message is the last thing the
        // client sees before the close.
        Promise.resolve()
          .then(function () { return onmessage(current, respond); })
          .then(function () {
            connection.close();
          }, function (exn: any) {
            error('Failed: ', exn, exn && exn.stack);
            respondJSON({ type: 'echo-err', contents: 'There was an internal error, please report this as a bug' });
            respondJSON({ type: 'echo-err', contents: String(exn) });
            connection.close();
          })
          .then(tryQueue);
      }
    }

    info((new Date()) + ' Connection accepted.');

    connection.on('message', function (message: any) {
      info('Received Message: ' + message);

      const parsed = JSON.parse(String(message));

      if (parsed.command === 'stop') {
        // The original interrupts the in-flight compile on the Pyret
        // runtime's segmented stack. Compiles here run synchronously, so by
        // the time this message is processed no job is running; queued jobs
        // are what a break would have abandoned.
        runQueue.length = 0;
      }

      if (parsed.command === 'shutdown') {
        info('Exiting due to shutdown request');
        process.exit(0);
      }

      if (parsed.command === 'compile') {
        runQueue.push(parsed.compileOptions);
        tryQueue();
      }

    });
    connection.on('close', function (_reasonCode: any, _description: any) {
      // info((new Date()) + ' Peer disconnected.');
    });
  });

  info('Server startup successful');
  if (process.send) {
    process.send({ type: 'success' });
  }

  process.on('SIGINT', function () {
    info('Caught interrupt signal, exiting server');
    // The original resumes the paused Pyret stack, after which pyret.arr
    // returns success-code; exit 0 here likewise (the 'exit' handler above
    // removes the socket file).
    process.exit(0);
  });
}

// Port of server.arr's serve(port, pyret-dir).
export function serve(port: string, pyretDir: string): void {
  makeServer(port, function (msg: string, sendMessage: (jsonData: string) => void) {
    const opts: { [key: string]: any } = JSON.parse(msg);
    if ('builtin-js-dir' in opts) {
      const v = opts['builtin-js-dir'];
      B.setBuiltinJsDirs(Array.isArray(v) ? v : [v]);
    }
    if ('builtin-arr-dir' in opts) {
      const v = opts['builtin-arr-dir'];
      B.setBuiltinArrDirs(Array.isArray(v) ? v : [v]);
    }
    if ('allow-builtin-overrides' in opts) {
      B.setAllowBuiltinOverrides(opts['allow-builtin-overrides']);
    }
    function logToClient(s: string, toClear?: number): void {
      sendMessage(serializeMessage([
        ['type', 'echo-log'],
        ['contents', s],
        ['clear-first', toClear === undefined ? false : toClear]
      ]));
    }
    function err(s: string): void {
      sendMessage(serializeMessage([
        ['type', 'echo-err'],
        ['contents', s]
      ]));
    }
    opts['log'] = logToClient;
    opts['log-error'] = err;
    opts['this-pyret-dir'] = pyretDir;
    opts['compiled-read-only'] = [P.resolve(P.join(pyretDir, 'lib-compiled'))];
    if ('perilous' in opts && opts['perilous']) {
      opts['user-annotations'] = false;
    }
    opts['require-config'] = orElse(opts, 'require-config',
      P.resolve(P.join(pyretDir, 'config.json')));
    return compile(opts).then(() => {
      sendMessage(serializeMessage([['type', 'compile-success']]));
    }, (exn: any) => {
      const errStr = RED.displayToString(renderReasonOf(exn), tostring, []);
      err(errStr + '\n');
      sendMessage(serializeMessage([['type', 'compile-failure']]));
    });
  });
}
