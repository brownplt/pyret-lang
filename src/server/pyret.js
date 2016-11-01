#!/usr/bin/env node

const commandLineArgs = require('command-line-args');
const childProcess = require('child_process');

const optionDefinitions = [
  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client", defaultValue: false },
  { name: 'port', alias: 't', type: String, defaultValue: "1705", group: "client" },
  { name: 'compiler', alias: 'c', type: String, defaultValue: "node_modules/pyret-lang/build/phase0/pyret.jarr", group: "client" },

  { name: 'program', type: String, group: "client", defaultOption: true },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'outfile', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/scripts/node_modules-config.json" },
  { name: 'builtin-js-dir', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/js/trove" },
  { name: 'builtin-arr-dir', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/arr/trove" },
  { name: 'check-mode', type: Boolean, group: "pyret-options", defaultValue: true },
  { name: 'compiled-dir', type: String, group: "pyret-options" },
  { name: 'standalone-file', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/js/base/handalone.js" }

];

const options = commandLineArgs(optionDefinitions);

const child = childProcess.fork(
  "./node_modules/pyret-lang/src/server/client.js",
  (options.client.shutdown ?  [ "--shutdown" ] : [])
  .concat(options['pyret-options']['outfile'] ? [ '--outfile', options['pyret-options']['outfile'] ] : [])
  .concat([
   "--port", options.client.port,
   "--compiler", options.client.compiler,

   "--program", options.client.program,

   '--require-config', options['pyret-options']['require-config'],
   '--builtin-js-dir', options['pyret-options']['builtin-js-dir'],
   '--builtin-arr-dir', options['pyret-options']['builtin-arr-dir'],
   '--check-mode', options['pyret-options']['check-mode'],
   '--compiled-dir', options['pyret-options']['compiled-dir'],
   '--standalone-file', options['pyret-options']['standalone-file'],
  ]),
  { silent: false } // To send messages on completion of startup
);
