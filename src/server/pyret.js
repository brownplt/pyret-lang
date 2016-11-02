#!/usr/bin/env node

const commandLineArgs = require('command-line-args');
const pyretClient = require('./client-lib');

const optionDefinitions = [
  // These options affect how the client starts up and communiates with the server
  { name: 'shutdown', alias: 's', type: Boolean, group: "client", defaultValue: false },
  { name: 'port', alias: 't', type: String, defaultValue: "1705", group: "client" },
  { name: 'compiler', alias: 'c', type: String, defaultValue: "node_modules/pyret-lang/build/phase0/pyret.jarr", group: "client" },

  { name: 'program', type: String, group: "pyret-options", defaultOption: true },

  // These options are passed on to the compiler, and have no effect (yet)
  // on the client
  { name: 'outfile', type: String, group: "pyret-options" },
  { name: 'require-config', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/scripts/node_modules-config.json" },
  { name: 'builtin-js-dir', type: String, multiple: true, group: "pyret-options", defaultValue: ["node_modules/pyret-lang/src/js/trove"] },
  { name: 'builtin-arr-dir', type: String, multiple: true, group: "pyret-options", defaultValue: ["node_modules/pyret-lang/src/arr/trove"] },
  { name: 'allow-builtin-overrides', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'no-check-mode', type: Boolean, group: "pyret-options", defaultValue: false },
  { name: 'compiled-dir', type: String, group: "pyret-options" },
  { name: 'standalone-file', type: String, group: "pyret-options", defaultValue: "node_modules/pyret-lang/src/js/base/handalone.js" }

];

const options = commandLineArgs(optionDefinitions);

pyretClient.start(options);

